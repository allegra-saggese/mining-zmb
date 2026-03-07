#!/usr/bin/env Rscript

# WMD PDF table extraction workflow
# - Input: years (e.g., 2023,2024,2025) and local PDF directory
# - Optional: URL map CSV (year,url) to auto-download missing PDFs
# - Output: one CSV per extracted table + one combined long "cell-level" CSV

required_packages <- c("dplyr", "purrr", "readr", "stringr", "tibble", "httr", "pdftools")

missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

split_csv <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(character(0))
  }
  out <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  trimws(out[nzchar(trimws(out))])
}

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  kv <- lapply(args, function(a) strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]])
  keys <- vapply(kv, function(x) x[[1]], character(1))
  vals <- vapply(kv, function(x) paste(x[-1], collapse = "="), character(1))
  arg_map <- stats::setNames(as.list(vals), keys)

  this_year <- as.integer(format(Sys.Date(), "%Y"))
  default_years <- paste(seq(this_year - 2, this_year), collapse = ",")

  list(
    years = as.integer(split_csv(arg_map$years %||% Sys.getenv("WMD_YEARS", unset = default_years))),
    pdf_dir = arg_map$pdf_dir %||% Sys.getenv("WMD_PDF_DIR", unset = file.path("raws", "wmd", "pdfs")),
    out_dir = arg_map$out_dir %||% Sys.getenv("WMD_OUT_DIR", unset = file.path("raws", "wmd", "tables")),
    url_map_csv = arg_map$url_map %||% Sys.getenv("WMD_URL_MAP_CSV", unset = file.path("raws", "wmd", "wmd_pdf_urls.csv")),
    download_missing = tolower(arg_map$download_missing %||% Sys.getenv("WMD_DOWNLOAD_MISSING", unset = "true")) %in% c("true", "1", "yes"),
    timeout_sec = as.numeric(arg_map$timeout_sec %||% Sys.getenv("WMD_TIMEOUT_SEC", unset = "90")),
    tries = as.integer(arg_map$tries %||% Sys.getenv("WMD_MAX_TRIES", unset = "3"))
  )
}

fetch_binary_retry <- function(url, timeout_sec, tries) {
  res <- httr::RETRY(
    verb = "GET",
    url = url,
    httr::timeout(timeout_sec),
    times = tries,
    pause_base = 2,
    pause_cap = 30,
    pause_min = 1,
    terminate_on = c(400, 401, 403, 404),
    quiet = TRUE
  )
  httr::stop_for_status(res)
  httr::content(res, as = "raw")
}

load_url_map <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(year = integer(), url = character()))
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  if (!all(c("year", "url") %in% names(df))) {
    stop("URL map must contain columns named 'year' and 'url'.")
  }
  df %>%
    dplyr::transmute(
      year = as.integer(year),
      url = as.character(url)
    ) %>%
    dplyr::filter(!is.na(year), !is.na(url), nzchar(url))
}

find_existing_pdf <- function(year, pdf_dir) {
  if (!dir.exists(pdf_dir)) {
    return(NA_character_)
  }
  files <- list.files(pdf_dir, pattern = "(?i)\\.pdf$", full.names = TRUE)
  if (length(files) == 0) {
    return(NA_character_)
  }
  yr <- as.character(year)
  has_year <- stringr::str_detect(basename(files), stringr::regex(paste0("(^|[^0-9])", yr, "([^0-9]|$)")))
  hits <- files[has_year]
  if (length(hits) == 0) NA_character_ else hits[[1]]
}

resolve_pdf_for_year <- function(year, pdf_dir, url_map, download_missing, timeout_sec, tries) {
  existing <- find_existing_pdf(year, pdf_dir)
  if (!is.na(existing)) {
    return(list(path = existing, status = "found_local", note = ""))
  }

  if (!download_missing) {
    return(list(path = NA_character_, status = "missing", note = "no local PDF and download_missing=FALSE"))
  }

  row <- url_map %>% dplyr::filter(year == !!year)
  if (nrow(row) == 0) {
    return(list(path = NA_character_, status = "missing", note = "no URL in url_map for this year"))
  }

  dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(pdf_dir, paste0("world_mining_data_", year, ".pdf"))
  bin <- tryCatch(
    fetch_binary_retry(row$url[[1]], timeout_sec = timeout_sec, tries = tries),
    error = function(e) e
  )
  if (inherits(bin, "error")) {
    return(list(path = NA_character_, status = "download_error", note = conditionMessage(bin)))
  }
  writeBin(bin, out_path)
  list(path = out_path, status = "downloaded", note = row$url[[1]])
}

table_matrix_to_cells <- function(tbl, year, pdf_file, page_num, table_num) {
  if (length(tbl) == 0) {
    return(tibble::tibble())
  }
  nr <- nrow(tbl)
  nc <- ncol(tbl)
  if (is.null(nr) || is.null(nc) || nr == 0 || nc == 0) {
    return(tibble::tibble())
  }
  idx <- expand.grid(row_id = seq_len(nr), col_id = seq_len(nc), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  vals <- as.vector(tbl)
  tibble::tibble(
    year = as.integer(year),
    pdf_file = basename(pdf_file),
    page = as.integer(page_num),
    table_id = as.integer(table_num),
    row_id = as.integer(idx$row_id),
    col_id = as.integer(idx$col_id),
    value = as.character(vals)
  ) %>%
    dplyr::mutate(value = stringr::str_squish(value))
}

extract_pdf_tables <- function(pdf_path, year, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_name <- tools::file_path_sans_ext(basename(pdf_path))
  year_dir <- file.path(out_dir, as.character(year))
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)

  has_tabulizer <- requireNamespace("tabulizer", quietly = TRUE)
  cells_all <- list()
  log_rows <- list()

  if (has_tabulizer) {
    pages <- pdftools::pdf_info(pdf_path)$pages
    for (pg in seq_len(pages)) {
      tables <- tryCatch(
        tabulizer::extract_tables(pdf_path, pages = pg, output = "matrix", method = "decide"),
        error = function(e) e
      )
      if (inherits(tables, "error")) {
        log_rows[[length(log_rows) + 1]] <- tibble::tibble(
          year = year,
          pdf_file = basename(pdf_path),
          page = pg,
          status = "extract_error",
          detail = conditionMessage(tables)
        )
        next
      }
      if (length(tables) == 0) {
        next
      }

      for (ti in seq_along(tables)) {
        tbl <- tables[[ti]]
        tbl_df <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)
        out_csv <- file.path(year_dir, sprintf("%s_p%03d_t%02d.csv", pdf_name, pg, ti))
        readr::write_csv(tbl_df, out_csv)
        cells_all[[length(cells_all) + 1]] <- table_matrix_to_cells(tbl, year, pdf_path, pg, ti)
      }
    }
  } else {
    # Fallback: save word-level extraction if tabulizer is not available.
    page_words <- pdftools::pdf_data(pdf_path)
    for (pg in seq_along(page_words)) {
      words <- page_words[[pg]]
      if (nrow(words) == 0) {
        next
      }
      out_csv <- file.path(year_dir, sprintf("%s_p%03d_words.csv", pdf_name, pg))
      readr::write_csv(words, out_csv)
      log_rows[[length(log_rows) + 1]] <- tibble::tibble(
        year = year,
        pdf_file = basename(pdf_path),
        page = pg,
        status = "fallback_words_only",
        detail = "tabulizer not installed; wrote word coordinates instead of detected tables"
      )
    }
  }

  list(
    cells = dplyr::bind_rows(cells_all),
    log = dplyr::bind_rows(log_rows)
  )
}

cfg <- parse_args()
dir.create(cfg$pdf_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)

url_map <- load_url_map(cfg$url_map_csv)

resolution <- purrr::map_dfr(cfg$years, function(yr) {
  res <- resolve_pdf_for_year(
    year = yr,
    pdf_dir = cfg$pdf_dir,
    url_map = url_map,
    download_missing = cfg$download_missing,
    timeout_sec = cfg$timeout_sec,
    tries = cfg$tries
  )
  tibble::tibble(
    year = as.integer(yr),
    pdf_path = res$path %||% NA_character_,
    resolve_status = res$status %||% NA_character_,
    resolve_note = res$note %||% NA_character_
  )
})

valid_pdfs <- resolution %>% dplyr::filter(!is.na(pdf_path), file.exists(pdf_path))

extracted <- purrr::map(seq_len(nrow(valid_pdfs)), function(i) {
  extract_pdf_tables(
    pdf_path = valid_pdfs$pdf_path[[i]],
    year = valid_pdfs$year[[i]],
    out_dir = cfg$out_dir
  )
})

cells <- dplyr::bind_rows(purrr::map(extracted, "cells"))
extract_log <- dplyr::bind_rows(purrr::map(extracted, "log"))

combined_out <- file.path(cfg$out_dir, "wmd_tables_cells_long.csv")
if (nrow(cells) > 0) {
  readr::write_csv(cells, combined_out)
}

resolution_out <- file.path(cfg$out_dir, "wmd_pdf_resolution_log.csv")
readr::write_csv(resolution, resolution_out)

if (nrow(extract_log) > 0) {
  readr::write_csv(extract_log, file.path(cfg$out_dir, "wmd_extraction_log.csv"))
}

message("WMD extraction completed.")
message("- years requested: ", paste(cfg$years, collapse = ", "))
message("- resolution log: ", resolution_out)
if (nrow(cells) > 0) {
  message("- combined table cells: ", combined_out)
} else {
  message("- no table cells extracted (check logs and tabulizer availability).")
}

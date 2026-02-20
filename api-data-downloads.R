# Attempted path (kept as reminder; this approach was unreliable/deprecated):
# install.packages("devtools")
# devtools::install_github("Knoema/knoema-r-driver")
# library(Knoema)
# httr::GET("https://api.knoema.com")

required_packages <- c(
  "httr", "jsonlite", "dplyr", "purrr", "stringr",
  "tibble", "readr", "rvest", "xml2", "lubridate"
)

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

as_node_list <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  if (is.list(x) && !is.null(names(x)) && any(startsWith(names(x), "@"))) {
    return(list(x))
  }
  if (is.list(x)) {
    return(x)
  }
  list(x)
}

fetch_json_retry <- function(url, timeout_sec = 120, tries = 6) {
  response <- httr::RETRY(
    verb = "GET",
    url = url,
    httr::timeout(timeout_sec),
    times = tries,
    pause_base = 2,
    pause_cap = 45,
    pause_min = 1,
    terminate_on = c(400, 401, 403, 404),
    quiet = TRUE
  )
  httr::stop_for_status(response)
  text <- httr::content(response, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(text, simplifyDataFrame = FALSE)
}

fetch_html_retry <- function(url, timeout_sec = 120, tries = 6) {
  response <- httr::RETRY(
    verb = "GET",
    url = url,
    httr::timeout(timeout_sec),
    times = tries,
    pause_base = 2,
    pause_cap = 45,
    pause_min = 1,
    terminate_on = c(400, 401, 403, 404),
    quiet = TRUE
  )
  httr::stop_for_status(response)
  xml2::read_html(httr::content(response, as = "text", encoding = "UTF-8"))
}

parse_compactdata <- function(payload, source_url) {
  series_nodes <- as_node_list(payload$CompactData$DataSet$Series)
  if (length(series_nodes) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(series_nodes, function(series_node) {
    indicator <- series_node[["@INDICATOR"]] %||% NA_character_
    unit <- series_node[["@UNIT_MEASURE"]] %||% NA_character_
    obs_nodes <- as_node_list(series_node$Obs)
    if (length(obs_nodes) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(obs_nodes, function(obs_node) {
      tibble::tibble(
        indicator = indicator,
        time_period = as.character(obs_node[["@TIME_PERIOD"]] %||% NA_character_),
        obs_value = suppressWarnings(as.numeric(obs_node[["@OBS_VALUE"]] %||% NA_character_)),
        unit_measure = unit,
        source_url = source_url
      )
    })
  })
}

extract_indicator_catalog <- function(payload, source_url) {
  code_lists <- as_node_list(payload$Structure$CodeLists$CodeList)
  if (length(code_lists) == 0) {
    return(tibble::tibble())
  }

  indicator_list <- NULL
  for (code_list in code_lists) {
    codes <- as_node_list(code_list$Code)
    code_values <- vapply(codes, function(code_node) {
      as.character(code_node[["@value"]] %||% "")
    }, character(1))
    if ("PCOPP_USD" %in% code_values) {
      indicator_list <- code_list
      break
    }
  }

  if (is.null(indicator_list)) {
    return(tibble::tibble())
  }

  codes <- as_node_list(indicator_list$Code)
  tibble::tibble(
    indicator = vapply(codes, function(code_node) {
      as.character(code_node[["@value"]] %||% NA_character_)
    }, character(1)),
    indicator_name = vapply(codes, function(code_node) {
      desc <- code_node$Description
      if (is.null(desc)) {
        return(NA_character_)
      }
      as.character(desc[["#text"]] %||% desc %||% NA_character_)
    }, character(1)),
    source_url = source_url
  ) %>%
    dplyr::arrange(indicator)
}

download_imf_pcps_indicator_catalog <- function(timeout_sec = 45, tries = 3) {
  url <- "https://dataservices.imf.org/REST/SDMX_JSON.svc/DataStructure/PCPS"
  payload <- fetch_json_retry(url, timeout_sec = timeout_sec, tries = tries)
  extract_indicator_catalog(payload, source_url = url)
}

download_imf_pcps_series <- function(indicator, start_period = "2000-01", timeout_sec = 45, tries = 3) {
  url <- paste0(
    "https://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/PCPS/M.",
    indicator,
    "?startPeriod=",
    start_period
  )
  payload <- fetch_json_retry(url, timeout_sec = timeout_sec, tries = tries)
  parse_compactdata(payload, url)
}

choose_base_metals_indicator <- function(imf_indicator_catalog) {
  if (nrow(imf_indicator_catalog) == 0) {
    return(NA_character_)
  }

  exact <- imf_indicator_catalog %>%
    dplyr::filter(
      stringr::str_detect(
        indicator_name,
        stringr::regex(
          "^Base Metals index, Commodity price index, Index, 2016\\s*=\\s*100, Index$",
          ignore_case = TRUE
        )
      )
    )
  if (nrow(exact) > 0) {
    return(exact$indicator[[1]])
  }

  broad <- imf_indicator_catalog %>%
    dplyr::filter(
      stringr::str_detect(indicator_name, stringr::regex("Base Metals", ignore_case = TRUE)),
      stringr::str_detect(indicator_name, stringr::regex("Commodity price index", ignore_case = TRUE)),
      stringr::str_detect(indicator_name, stringr::regex("2016\\s*=\\s*100", ignore_case = TRUE))
    )
  if (nrow(broad) > 0) {
    return(broad$indicator[[1]])
  }

  fallback_candidates <- c("PBCMET_IX", "PBCMET_USD", "PBCMET")
  matched <- fallback_candidates[fallback_candidates %in% imf_indicator_catalog$indicator]
  if (length(matched) > 0) {
    return(matched[[1]])
  }

  NA_character_
}

download_imf_base_metals_index <- function(imf_indicator_catalog, start_period = "2000-01", timeout_sec = 45, tries = 3) {
  indicator <- choose_base_metals_indicator(imf_indicator_catalog)
  if (is.na(indicator)) {
    warning("Base metals index indicator was not found in IMF PCPS catalog.", call. = FALSE)
    return(tibble::tibble())
  }

  series <- download_imf_pcps_series(
    indicator = indicator,
    start_period = start_period,
    timeout_sec = timeout_sec,
    tries = tries
  )

  series_name <- imf_indicator_catalog %>%
    dplyr::filter(indicator == !!indicator) %>%
    dplyr::pull(indicator_name) %>%
    .[[1]] %||% "Base Metals index, Commodity price index, Index, 2016=100, Index"

  dplyr::mutate(
    series,
    series_name = series_name,
    date = suppressWarnings(lubridate::ym(time_period))
  ) %>%
    dplyr::arrange(date)
}

extract_average_price_per_carat <- function(text) {
  patterns <- c(
    "(?i)average\\s+sales\\s+price\\s+of\\s+USD\\s*\\$?\\s*([0-9][0-9,]*(?:\\.[0-9]+)?)\\s+per\\s+carat",
    "(?i)average\\s+reali[sz]ed\\s+price\\s+of\\s+USD\\s*\\$?\\s*([0-9][0-9,]*(?:\\.[0-9]+)?)\\s+per\\s+carat",
    "(?i)average\\s+price\\s+of\\s+USD\\s*\\$?\\s*([0-9][0-9,]*(?:\\.[0-9]+)?)\\s+per\\s+carat",
    "(?i)average\\s+price\\s+of\\s*\\$?\\s*([0-9][0-9,]*(?:\\.[0-9]+)?)\\s+per\\s+carat",
    "(?i)\\$\\s*([0-9][0-9,]*(?:\\.[0-9]+)?)\\s*/\\s*ct"
  )

  for (pattern in patterns) {
    hit <- stringr::str_match(text, pattern)
    if (!is.na(hit[1, 2])) {
      return(as.numeric(gsub(",", "", hit[1, 2])))
    }
  }

  NA_real_
}

extract_auction_date <- function(page, text) {
  date_text <- rvest::html_text2(rvest::html_elements(page, "time"))
  date_text <- date_text[nzchar(date_text)]
  if (length(date_text) > 0) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      date_text[1],
      orders = c("d b Y", "d B Y", "Y-m-d", "Y/m/d")
    ))
    if (!is.na(parsed)) {
      return(as.Date(parsed))
    }
  }

  date_match <- stringr::str_match(text, "(?i)\\bdate\\s*([0-9]{1,2}\\s+[A-Za-z]{3,9}\\s+[0-9]{4})\\b")
  if (!is.na(date_match[1, 2])) {
    parsed <- suppressWarnings(lubridate::dmy(date_match[1, 2]))
    if (!is.na(parsed)) {
      return(as.Date(parsed))
    }
  }

  NA
}

collect_gemfields_auction_links <- function(max_pages = 4, timeout_sec = 45, tries = 3) {
  base <- "https://www.gemfieldsgroup.com/category/new-and-announcements/auction-update/"
  page_urls <- c(
    base,
    paste0(base, "page/", seq_len(max_pages - 1), "/")
  )

  links <- purrr::map(page_urls, function(page_url) {
    page <- tryCatch(
      fetch_html_retry(page_url, timeout_sec = timeout_sec, tries = tries),
      error = function(e) NULL
    )
    if (is.null(page)) {
      return(character(0))
    }
    rvest::html_elements(page, "a") %>%
      rvest::html_attr("href") %>%
      stats::na.omit() %>%
      unique()
  }) %>%
    unlist(use.names = FALSE)

  links <- links[stringr::str_detect(links, "gemfieldsgroup\\.com/.+auction")]
  links <- links[!stringr::str_detect(links, "/category/")]
  unique(links)
}

download_gemfields_auction_prices <- function(max_pages = 4, timeout_sec = 45, tries = 3) {
  links <- collect_gemfields_auction_links(
    max_pages = max_pages,
    timeout_sec = timeout_sec,
    tries = tries
  )

  purrr::map_dfr(links, function(link) {
    page <- tryCatch(
      fetch_html_retry(link, timeout_sec = timeout_sec, tries = tries),
      error = function(e) NULL
    )
    if (is.null(page)) {
      return(tibble::tibble())
    }

    title <- rvest::html_text2(rvest::html_element(page, "h1")) %||% NA_character_
    text <- rvest::html_text2(rvest::html_element(page, "body")) %||% ""

    tibble::tibble(
      announcement_date = extract_auction_date(page, text),
      auction_title = stringr::str_squish(title),
      avg_price_usd_per_carat = extract_average_price_per_carat(text),
      source_url = link
    )
  }) %>%
    dplyr::filter(!is.na(avg_price_usd_per_carat)) %>%
    dplyr::distinct(source_url, .keep_all = TRUE) %>%
    dplyr::arrange(announcement_date)
}

output_dir <- "raws"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

timeout_sec <- as.numeric(Sys.getenv("API_TIMEOUT_SEC", unset = "45"))
tries <- as.integer(Sys.getenv("API_MAX_TRIES", unset = "3"))
start_period <- Sys.getenv("IMF_START_PERIOD", unset = "2000-01")
max_gemfields_pages <- as.integer(Sys.getenv("GEMFIELDS_MAX_PAGES", unset = "4"))

imf_indicator_catalog <- tryCatch(
  download_imf_pcps_indicator_catalog(timeout_sec = timeout_sec, tries = tries),
  error = function(e) {
    warning(sprintf("Could not download IMF PCPS indicator catalog: %s", conditionMessage(e)), call. = FALSE)
    tibble::tibble()
  }
)

if (nrow(imf_indicator_catalog) > 0) {
  readr::write_csv(
    imf_indicator_catalog,
    file.path(output_dir, "imf_pcps_indicator_catalog.csv")
  )
}

imf_base_metals <- tryCatch(
  download_imf_base_metals_index(
    imf_indicator_catalog = imf_indicator_catalog,
    start_period = start_period,
    timeout_sec = timeout_sec,
    tries = tries
  ),
  error = function(e) {
    warning(sprintf("Could not download IMF base metals index: %s", conditionMessage(e)), call. = FALSE)
    tibble::tibble()
  }
)

if (nrow(imf_base_metals) > 0) {
  readr::write_csv(
    imf_base_metals,
    file.path(output_dir, "imf_pcps_base_metals_index_monthly.csv")
  )
}

gemfields_auction <- download_gemfields_auction_prices(
  max_pages = max_gemfields_pages,
  timeout_sec = timeout_sec,
  tries = tries
)
readr::write_csv(
  gemfields_auction,
  file.path(output_dir, "gemfields_auction_avg_price_per_carat.csv")
)

gemfields_monthly <- gemfields_auction %>%
  dplyr::mutate(month = lubridate::floor_date(announcement_date, unit = "month")) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    avg_price_usd_per_carat = mean(avg_price_usd_per_carat, na.rm = TRUE),
    n_auctions = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(month)

readr::write_csv(
  gemfields_monthly,
  file.path(output_dir, "gemfields_auction_avg_price_per_carat_monthly.csv")
)

message("Saved files:")
if (nrow(imf_indicator_catalog) > 0) {
  message("- ", file.path(output_dir, "imf_pcps_indicator_catalog.csv"))
}
if (nrow(imf_base_metals) > 0) {
  message("- ", file.path(output_dir, "imf_pcps_base_metals_index_monthly.csv"))
}
message("- ", file.path(output_dir, "gemfields_auction_avg_price_per_carat.csv"))
message("- ", file.path(output_dir, "gemfields_auction_avg_price_per_carat_monthly.csv"))

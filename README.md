# zambia
summary analysis of electricity, industry and mining in zambia
study of copper prices + local economic conditions 

### scripts

| Script | What it does | Writes to |
|---|---|---|
| `api-data-downloads.R` | Pulls the IMF Primary Commodity Prices (PCPS) indicator catalogue and base-metals/copper price series (monthly, from `IMF_START_PERIOD`, default 2000-01) via the IMF SDMX API, and scrapes Gemfields auction announcements. Settings via env vars (`API_TIMEOUT_SEC`, `API_MAX_TRIES`, `IMF_START_PERIOD`, `GEMFIELDS_MAX_PAGES`). The Knoema API route at the top is commented out (unreliable) | `raws/` |
| `wmd-pdf-table-extract.R` | Extracts tables from WMD annual-report PDFs: downloads missing PDFs from a `year,url` map, writes one CSV per table plus a combined long cell-level CSV. Arguments `--years=`, `--pdf_dir=`, `--out_dir=`, `--url_map=` (or `WMD_*` env vars) | `raws/wmd/{pdfs,tables}/` |
| `mineral_map.R` | Maps of Zambia: global deposit and critical-mineral KMLs, USGS Zambia layer, district and national boundaries, plus regional employment (ag / non-ag) and electricity-by-region series | plots |

`zambia.Rproj` is the R project file. Raw data (`/raws`, `*.csv`, `*.dta`, `/Dropbox/`) are gitignored; only `data/data-doc/` (ILOSTAT bulk-download guidelines) is tracked. `mineral_map.R` reads its inputs by bare filename, so set the working directory to the folder holding the KMLs and shapefiles first. `.Rproj.user/` and `.RData` are tracked in git and should be removed from the index.

### data sources

# minerals
(1) Global assessment of undiscovered copper resources - https://mrdata.usgs.gov/sir20105090z/, https://mrdata.usgs.gov/services/sir20105090z, 
Deposits, prospects, and permissive tracts for porphyry and sediment-hosted copper resources worldwide, with estimates of undiscovered copper resources.
(2) Major mineral deposits of the world - https://mrdata.usgs.gov/major-deposits/, https://mrdata.usgs.gov/services/ofr20051294, 
Regional locations and general geologic setting of known deposits of major nonfuel mineral commodities.
(3) Global distribution of selected mines, deposits, and districts of critical minerals, https://mrdata.usgs.gov/pp1802/, https://mrdata.usgs.gov/services/pp1802	
Approximate locations and short descriptions of mines, deposits, and districts where critical minerals are found. The critical minerals are discussed in USGS Professional Paper 1802, and many of these locations are described in further detail in that report.

# solar
https://globalsolaratlas.info/download/zambia using the data for PVOUT - Photovoltaic power potential [kWh/kWp] 
This data is from Solar Resource Atlas and was last updated March 2019 

# zambia level data
(1) https://rplumber.ilo.org/files/website/bulk/ref_area.html - ILO bulk download of file "ZMB_A" 
(2) https://zambia.opendataforafrica.org/data/#topic=Population Central Statistical Office of Zambia, Zambia Population and Housing Census Data, 1969-202

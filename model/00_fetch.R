# 00_fetch.R: download and cache every raw input the model needs.
# Historical files are cached once; live inputs (polls, ratings, legislators, Wikipedia) are refreshed on every run.
# Everything here is openly downloadable (no login / guestbook).
# Usage (from the project root): Rscript model/00_fetch.R [--force]

args <- commandArgs(trailingOnly = TRUE)
FORCE <- "--force" %in% args
ROOT <- normalizePath(Sys.getenv("MODEL_ROOT", unset = "."), mustWork = TRUE)
CACHE <- file.path(ROOT, "model", "cache")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
options(timeout = 600)

get <- function(url, dest, live = FALSE) {
    path <- file.path(CACHE, dest)
    if (!FORCE && !live && file.exists(path) && file.size(path) > 0) {
        message("cached   ", dest)
        return(invisible(path))
    }
    message("download ", dest)
    utils::download.file(url, path, mode = "wb", quiet = TRUE)
    invisible(path)
}

# ---- historical results ----
# MIT Election Data + Science Lab (open copies): House 1976-2018 and President 1976-2016 by state
medsl <- "https://raw.githubusercontent.com/MEDSL/constituency-returns/master/"
get(paste0(medsl, "1976-2018-house.csv"), "house_1976_2018.csv")
get(paste0(medsl, "1976-2016-president.csv"), "president_1976_2016.csv")
get("https://dataverse.harvard.edu/api/access/datafile/13887039", "senate_1976_2024.tab") # Senate 1976-2024
# FEC official results: House by district for 2020 and 2022
get("https://www.fec.gov/resources/cms-content/documents/federalelections2020.xlsx", "fec_2020.xlsx")
get("https://www.fec.gov/resources/cms-content/documents/federalelections2022.xlsx", "fec_2022.xlsx")
# County presidential results (tonmcg): 2008/2012/2016, 2020, 2024
tm <- "https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/master/"
get(paste0(tm, "US_County_Level_Presidential_Results_08-16.csv"), "county_pres_08_16.csv")
get(paste0(tm, "2020_US_County_Level_Presidential_Results.csv"), "county_pres_2020.csv")
get(paste0(tm, "2024_US_County_Level_Presidential_Results.csv"), "county_pres_2024.csv")

# ---- boundaries ----
get("https://cdn.jsdelivr.net/npm/us-atlas@3/counties-10m.json", "us_counties_10m.json")
cd <- c(
    cd113 = "https://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_cd113_20m.zip",
    cd115 = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_cd115_20m.zip",
    cd116 = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_cd116_20m.zip",
    cd118 = "https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_cd118_20m.zip"
)
for (n in names(cd)) get(cd[[n]], paste0(n, ".zip"))
file.copy(file.path(ROOT, "data", "cb_2024_us_cd119_20m.zip"), file.path(CACHE, "cd119.zip"), overwrite = TRUE)

# ---- live 2026 inputs ----
get("https://dcdossier.github.io/Generational-Mid-terms/data.json", "election_data.json", live = TRUE)
get("https://unitedstates.github.io/congress-legislators/legislators-current.json", "legislators.json", live = TRUE)
get("https://en.wikipedia.org/w/api.php?action=parse&page=2026_United_States_House_of_Representatives_elections&prop=text&format=json",
    "wiki_house.json",
    live = TRUE
)

message("done")

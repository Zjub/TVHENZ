# Acquire and cache sources for the fiscal replication. Run from any directory.
args <- commandArgs(FALSE)
this <- sub("^--file=", "", args[grepl("^--file=", args)][1])
root <- Sys.getenv("OECD_LTS_ROOT")
if (!nzchar(root)) root <- normalizePath(file.path(dirname(this), ".."), mustWork = TRUE)
raw <- file.path(root, "data", "raw", "fiscal")
dir.create(raw, recursive = TRUE, showWarnings = FALSE)
library(jsonlite)
library(data.table)
options(timeout = 180)
downloads <- data.table(file = character(), source_url = character(), status = character())
fetch <- function(url, name, required = TRUE) {
  dest <- file.path(raw, name)
  if (file.exists(dest) && file.info(dest)$size > 0) {
    downloads <<- rbind(downloads, data.table(file = name, source_url = url, status = "cached"))
    return(invisible(dest))
  }
  tmp <- paste0(dest, ".part")
  message("Fetching ", name)
  status <- tryCatch({
    curl::curl_download(url, tmp, quiet = TRUE)
    file.rename(tmp, dest)
  }, error = function(e) { message(conditionMessage(e)); FALSE })
  if (!status && required) stop("Required source unavailable: ", url)
  downloads <<- rbind(downloads, data.table(file = name, source_url = url,
                                         status = if (status) "downloaded" else "request_failed"))
  invisible(if (status) dest else NA_character_)
}
fetch("https://stat.link/gdxr47", "australia_2026_figure_1_14.xlsx")
fetch("https://api.db.nomics.world/v22/series/OECD/DSD_EO_LTB%40DF_EO_LTB/AUS..BAU1.A?limit=1000&observations=1", "eo117_all_australia_bau1.json")
fetch("https://api.db.nomics.world/v22/datasets/OECD/DSD_SOCX_AGG%40DF_SOCX_AGG", "socx_metadata.json")
fetch("https://api.db.nomics.world/v22/datasets/OECD/DSD_SHA%40DF_SHA", "sha_metadata.json")
fetch("https://api.db.nomics.world/v22/datasets/OECD/DSD_LFS%40DF_LFS_INDIC", "lfs_metadata.json")
fetch_series <- function(dataset, dimensions, name) {
  fetch(paste0("https://api.db.nomics.world/v22/series/OECD/", dataset, "/",
               paste(dimensions, collapse = "."), "?limit=1000&observations=1"), name)
}
fetch_series("DSD_SOCX_AGG%40DF_SOCX_AGG", c("AUS", "A", "SOCX", "PT_B1GQ", "ES10", "", "", ""), "socx_australia.json")
fetch_series("DSD_SHA%40DF_SHA", c("AUS", "A", "EXP_HEALTH", "PT_B1GQ", "HF1", "", "", "", "", "", "", ""), "sha_australia.json")
for (age in c("Y55T64", "Y65T74")) {
  fetch_series("DSD_LFS%40DF_LFS_INDIC", c("AUS", "EMP_RATIO", "", "_T", age, ""), paste0("lfs_", age, ".json"))
}
fetch("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/2_Population/WPP2024_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES.xlsx", "wpp2024_age_groups.xlsx")
fetch("https://www.oecd.org/content/dam/oecd/en/publications/reports/2019/12/recent-improvements-to-the-public-finance-block-of-the-oecd-s-long-term-global-model_467e14ad/4f07fb8d-en.pdf", "oecd_fiscal_method_2019.pdf")
fetch("https://www.oecd.org/content/dam/oecd/en/publications/reports/2017/11/the-fiscal-projection-framework-in-long-term-scenarios_56561d45/8eddfa18-en.pdf", "oecd_fiscal_method_2017.pdf")
fetch("https://www.oecd.org/content/dam/oecd/en/publications/reports/2026/09/illustrating-long-run-fiscal-challenges-using-the-long-term-model_aeacb594/421bf86f-en.pdf", "oecd_fiscal_method_2026.pdf")
fetch("https://www.oecd.org/content/dam/oecd/en/publications/reports/2026/01/oecd-economic-surveys-australia-2026_1b6f84bc/d22a1efd-en.pdf", "australia_survey_2026.pdf")
fwrite(downloads, file.path(raw, "download_manifest.csv"))
message("Source cache: ", raw)

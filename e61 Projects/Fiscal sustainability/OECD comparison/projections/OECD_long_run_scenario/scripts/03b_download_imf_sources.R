# Public IMF sources. Existing snapshots are retained; default model runs offline.
args <- commandArgs(FALSE)
this <- sub("^--file=", "", args[grepl("^--file=", args)][1])
imf_root <- Sys.getenv("OECD_LTS_ROOT")
if (!nzchar(imf_root)) imf_root <- normalizePath(file.path(dirname(this), ".."))
imf_raw <- file.path(imf_root, "data", "raw", "imf")
dir.create(imf_raw, recursive = TRUE, showWarnings = FALSE)
imf_requests <- data.table::data.table(file = character(), url = character(), status = character())
get_imf <- function(url, name, required = TRUE) {
  dest <- file.path(imf_raw, name)
  status <- "cached"
  if (!file.exists(dest)) {
    message("Downloading IMF source: ", name)
    status <- tryCatch({curl::curl_download(url, paste0(dest, ".part"), quiet = TRUE,
      handle = curl::new_handle(timeout = 120));
      if (!file.rename(paste0(dest, ".part"), dest)) stop("Could not save source")
      "downloaded"}, error = function(e) {message(conditionMessage(e)); "request_failed"})
  }
  imf_requests <<- rbind(imf_requests, data.table::data.table(file = name, url = url, status = status))
  if (status == "request_failed" && required) stop("Required IMF source unavailable: ", url)
  invisible(dest)
}
get_imf("https://www.imf.org/external/datamapper/api/v1/indicators", "datamapper_indicators.json")
get_imf("https://www.imf.org/external/datamapper/datasets/FM", "fiscal_monitor_dataset_page.html")
for (indicator in c("G_X_G01_GDP_PT", "GGXCNL_G01_GDP_PT", "GGXONLB_G01_GDP_PT", "G_XWDG_G01_GDP_PT")) {
  get_imf(paste0("https://www.imf.org/external/datamapper/api/v1/", indicator, "/AUS"), paste0(indicator, "_AUS.json"))
}
for (url in c(
  "https://www.imf.org/-/media/files/publications/fiscal-monitor/2026/april/english/msa.pdf?download=1",
  "https://www.imf.org/-/media/Files/Publications/fiscal-monitor/2026/April/English/msa.ashx")) {
  get_imf(url, "fiscal_monitor_april_2026_appendix.pdf", FALSE)
}
get_imf("https://www.imf.org/-/media/files/publications/cr/2026/english/1ausea2026001-source-pdf.pdf?download=1", "australia_article_iv_2025_published_2026.pdf", FALSE)
data.table::fwrite(imf_requests, file.path(imf_raw, "download_manifest.csv"))

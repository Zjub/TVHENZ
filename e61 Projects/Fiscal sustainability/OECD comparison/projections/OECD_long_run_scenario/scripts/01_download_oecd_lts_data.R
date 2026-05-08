# Download and tidy OECD Economic Outlook 117 long-run scenario data for
# Australia. The OECD Data Explorer dataset is mirrored by DBnomics, which gives
# a stable JSON endpoint and keeps the official OECD dimensions intact.

if (!exists("oecd_lts_config")) {
  source(file.path(Sys.getenv("OECD_LTS_ROOT", unset = getwd()), "scripts", "00_config.R"))
}

fetch_oecd_lts_metadata <- function() {
  metadata_url <- paste0(
    "https://api.db.nomics.world/v22/datasets/",
    oecd_lts_config$source_provider,
    "/",
    utils::URLencode(oecd_lts_config$source_dataset, reserved = TRUE)
  )

  metadata <- jsonlite::fromJSON(metadata_url, simplifyVector = FALSE)
  metadata_path <- file.path(oecd_lts_config$raw_dir, "oecd_lts_dataset_metadata.json")
  writeLines(jsonlite::toJSON(metadata, auto_unbox = TRUE, pretty = TRUE), metadata_path)

  doc <- metadata$datasets$docs[[1]]

  scenario_metadata <- data.table(
    scenario = names(doc$dimensions_values_labels$SCENARIO),
    oecd_scenario_label = unlist(doc$dimensions_values_labels$SCENARIO, use.names = FALSE)
  )
  measure_metadata <- data.table(
    measure = names(doc$dimensions_values_labels$MEASURE),
    oecd_measure_label = unlist(doc$dimensions_values_labels$MEASURE, use.names = FALSE)
  )

  fwrite(scenario_metadata, file.path(oecd_lts_config$processed_dir, "oecd_lts_scenario_metadata.csv"))
  fwrite(measure_metadata, file.path(oecd_lts_config$processed_dir, "oecd_lts_measure_metadata.csv"))

  list(
    raw = metadata,
    scenarios = scenario_metadata,
    measures = measure_metadata
  )
}

fetch_oecd_lts_measure <- function(measure) {
  # Leaving the scenario dimension blank pulls all six OECD long-run scenarios
  # for the requested measure and country.
  series_mask <- paste(
    oecd_lts_config$country_code,
    measure,
    "",
    oecd_lts_config$frequency,
    sep = "."
  )
  url <- paste0(
    oecd_lts_config$dbnomics_dataset_url,
    "/",
    series_mask,
    "?observations=1"
  )

  response <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE),
    error = function(e) {
      warning("Could not download ", series_mask, ": ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(response) || is.null(response$series$docs) || !length(response$series$docs)) {
    return(list(
      data = data.table(),
      status = data.table(
        measure = measure,
        series_mask = series_mask,
        status = "missing",
        n_series = 0L,
        source_url = url
      )
    ))
  }

  series_dt <- rbindlist(lapply(response$series$docs, function(doc) {
    if (is.null(doc$period) || is.null(doc$value)) {
      return(NULL)
    }

    data.table(
      country = doc$dimensions$REF_AREA %||% oecd_lts_config$country_code,
      measure = doc$dimensions$MEASURE %||% measure,
      scenario = doc$dimensions$SCENARIO %||% NA_character_,
      frequency = doc$dimensions$FREQ %||% oecd_lts_config$frequency,
      year = as.integer(doc$period),
      value = as.numeric(doc$value),
      series_code = doc$series_code %||% NA_character_,
      series_name = doc$series_name %||% NA_character_,
      source_url = url
    )
  }), use.names = TRUE, fill = TRUE)

  list(
    data = series_dt,
    status = data.table(
      measure = measure,
      series_mask = series_mask,
      status = "downloaded",
      n_series = length(response$series$docs),
      source_url = url
    )
  )
}

download_oecd_lts_australia <- function(force = TRUE) {
  output_path <- file.path(oecd_lts_config$processed_dir, "oecd_lts_australia_long.csv")
  status_path <- file.path(oecd_lts_config$processed_dir, "oecd_lts_download_status.csv")

  if (!force && file.exists(output_path)) {
    message("Using existing OECD LTS data: ", normalizePath(output_path))
    return(invisible(fread(output_path)))
  }

  metadata <- fetch_oecd_lts_metadata()

  download_results <- lapply(measure_lookup$measure, fetch_oecd_lts_measure)
  raw_dt <- rbindlist(lapply(download_results, `[[`, "data"), use.names = TRUE, fill = TRUE)
  status_dt <- rbindlist(lapply(download_results, `[[`, "status"), use.names = TRUE, fill = TRUE)

  if (!nrow(raw_dt)) {
    stop("No OECD long-run scenario data could be downloaded.", call. = FALSE)
  }

  raw_dt <- merge(raw_dt, scenario_lookup, by = "scenario", all.x = TRUE)
  raw_dt <- merge(raw_dt, measure_lookup, by = "measure", all.x = TRUE)
  raw_dt <- merge(raw_dt, metadata$measures, by = "measure", all.x = TRUE)

  raw_dt[, country_name := oecd_lts_config$country_name]
  raw_dt[, source_dataset := oecd_lts_config$source_dataset_name]
  raw_dt[, downloaded_at := format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")]
  setcolorder(
    raw_dt,
    c(
      "country",
      "country_name",
      "scenario",
      "scenario_short",
      "scenario_label",
      "transition",
      "damage_curve",
      "mitigation_cost",
      "measure",
      "measure_label",
      "oecd_measure_label",
      "category",
      "unit_hint",
      "frequency",
      "year",
      "value",
      "series_code",
      "series_name",
      "source_dataset",
      "source_url",
      "downloaded_at"
    )
  )
  setorder(raw_dt, measure, scenario, year)

  fwrite(raw_dt, output_path)
  fwrite(status_dt, status_path)

  message(
    "Downloaded ",
    nrow(raw_dt),
    " observations across ",
    uniqueN(raw_dt$measure),
    " measures and ",
    uniqueN(raw_dt$scenario),
    " scenarios."
  )

  invisible(raw_dt)
}

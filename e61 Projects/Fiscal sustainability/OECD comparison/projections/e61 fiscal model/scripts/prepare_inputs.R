# Run from the new project root. Source snapshots are copied, never refreshed.
dir.create('data', showWarnings = FALSE)
source_dir <- file.path('..', '2026 projections', 'data', 'processed')
files <- c('official_pbo_nfo_wide.csv', 'population_projection_age_shares.csv',
           'historical_gfs_expenses_by_purpose.csv', 'historical_gfs_operating_statement.csv')
manifest <- do.call(rbind, lapply(files, function(f) {
  src <- file.path(source_dir, f)
  dest <- file.path('data', f)
  if (!file.exists(dest)) stopifnot(file.copy(src, dest))
  data.frame(file = f, copied_from = src, md5 = unname(tools::md5sum(dest)),
             snapshot_date = '2026-10-25', stringsAsFactors = FALSE)
}))
write.csv(manifest, 'data/snapshot_manifest.csv', row.names = FALSE)
message('Cached four existing project inputs; no parent files changed.')

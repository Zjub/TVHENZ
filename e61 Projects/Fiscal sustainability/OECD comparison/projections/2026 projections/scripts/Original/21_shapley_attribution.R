source(file.path("scripts", "00_config.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
model_data <- historical[, .(
  year, broad_expenditure_gdp,
  age_0_14 = `0_14`, age_15_34 = `15_34`, age_55_64 = `55_64`, age_65p = `65p`,
  tot_z, rp_z, unemployment, covid
)]

base_year <- 2000L
comparison_year <- max(historical$year)
stopifnot(base_year %in% historical$year, comparison_year > base_year)

factor_columns <- list(
  Demography = c("age_0_14", "age_15_34", "age_55_64", "age_65p"),
  `Terms of trade` = "tot_z",
  `Relative government prices` = "rp_z",
  Unemployment = "unemployment",
  `COVID period` = "covid"
)
factor_names <- names(factor_columns)
n_factors <- length(factor_names)

base_row <- model_data[year == base_year]
comparison_row <- model_data[year == comparison_year]

# The value of a coalition is the fitted change between the two years from a
# regression containing only that coalition. Re-fitting each coalition makes
# the allocation sensitive to shared explanatory power; Shapley averaging then
# removes dependence on any one ordering.
coalition_value <- function(included) {
  if (!length(included)) return(0)
  columns <- unlist(factor_columns[included], use.names = FALSE)
  fit <- lm(reformulate(columns, response = "broad_expenditure_gdp"), data = model_data)
  as.numeric(predict(fit, comparison_row) - predict(fit, base_row))
}

coalitions <- lapply(0:(2^n_factors - 1L), function(mask) {
  factor_names[as.logical(intToBits(mask)[seq_len(n_factors)])]
})
coalition_key <- function(x) if (length(x)) paste(sort(x), collapse = "|") else "<empty>"
coalition_keys <- vapply(coalitions, coalition_key, character(1))
coalition_values <- setNames(vapply(coalitions, coalition_value, numeric(1)), coalition_keys)

shapley <- vapply(factor_names, function(factor) {
  others <- setdiff(factor_names, factor)
  subsets <- lapply(0:(2^(n_factors - 1L) - 1L), function(mask) {
    others[as.logical(intToBits(mask)[seq_along(others)])]
  })
  sum(vapply(subsets, function(s) {
    weight <- factorial(length(s)) * factorial(n_factors - length(s) - 1L) / factorial(n_factors)
    without_key <- coalition_key(s)
    with_key <- coalition_key(c(s, factor))
    weight * (coalition_values[[with_key]] - coalition_values[[without_key]])
  }, numeric(1)))
}, numeric(1))

full_key <- coalition_key(factor_names)
fitted_change <- coalition_values[[full_key]]
observed_change <- comparison_row$broad_expenditure_gdp - base_row$broad_expenditure_gdp
residual_change <- observed_change - fitted_change

decomposition <- rbindlist(list(
  data.table(component = factor_names, contribution = unname(shapley), component_type = "Shapley factor"),
  data.table(component = "Residual / unexplained change", contribution = residual_change,
             component_type = "Residual")
))
decomposition[, `:=`(
  base_year = base_year,
  comparison_year = comparison_year,
  contribution_pp = contribution * 100
)]

reconciliation <- data.table(
  base_year = base_year,
  comparison_year = comparison_year,
  observed_change_pp = observed_change * 100,
  full_model_fitted_change_pp = fitted_change * 100,
  shapley_sum_pp = sum(shapley) * 100,
  residual_change_pp = residual_change * 100,
  reconciled_change_pp = sum(decomposition$contribution) * 100,
  reconciliation_error_pp = (sum(decomposition$contribution) - observed_change) * 100,
  coalitions_estimated = length(coalitions),
  implied_orderings = factorial(n_factors)
)

fwrite(decomposition, file.path(table_dir, "shapley_change_decomposition.csv"))
fwrite(reconciliation, file.path(table_dir, "shapley_reconciliation.csv"))
message("Shapley attribution written for ", base_year, " to ", comparison_year,
        "; factor contributions average over ", factorial(n_factors), " orderings.")

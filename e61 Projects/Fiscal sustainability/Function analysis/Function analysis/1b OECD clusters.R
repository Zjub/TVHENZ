
## Setup ----

library(cli)
library(tidyverse)
library(data.table)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(OECD)
library(jsonlite)
library(httr)
library(Synth)
library(mFilter)
library(dendextend)
library(FactoMineR)
library(factoextra)
library(ggalluvial)
library(pheatmap)
library(gridExtra)
library(TSclust)
library(dtwclust)
library(moments)
library(cluster)
library(quantreg)

rm(list=ls())
gc()

k <- 5

## Import data ----

dt <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                                                                       sheet = "COFOGexp", skip = 1) # The absolute amounts
Consol_toG <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                         sheet = "COFOGexp_%oftotal", skip = 1)

Consol_toGDP <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                           sheet = "COFOGexp_%GDP", skip = 1)

setDT(dt)
setDT(Consol_toG)
setDT(Consol_toGDP)

dt[,":=" (`1995` = NULL,`1996` = NULL,`1997` = NULL,ISO = NULL,`COFOG Code` = NULL,`Government level code` = NULL)]

dt_long <- melt(
  dt,
  id.vars = c("Country", "COFOG Area", "Government level"),
  variable.name = "Year",
  value.name = "Value"
)

dt <- dt_long[, .(value = sum(Value, na.rm = TRUE)),
                  by = .(Country, cofog_group = `COFOG Area`, Year)][cofog_group != "Total"]

Consol_toGDP[,":=" (`1995` = NULL,`1996` = NULL,`1997` = NULL,ISO = NULL,`COFOG Code` = NULL,`Government level code` = NULL)]

Consol_toGDP_long <- melt(
  Consol_toGDP,
  id.vars = c("Country", "COFOG Area", "Government level"),
  variable.name = "Year",
  value.name = "Value"
)

Consol_toGDP <- Consol_toGDP_long[, .(share_gdp = sum(Value, na.rm = TRUE)),
              by = .(Country, cofog_group = `COFOG Area`, Year)][cofog_group != "Total"]

###############
# dt_AUS: Australia, nominal spending by COFOG Area, yearly
dt_AUS <- copy(dt[Country == "Australia"])

# Compute nominal log differences by function
setorder(dt_AUS, cofog_group, Year)
dt_AUS[, dlog := c(NA, diff(log(value))), by = cofog_group]

# z_t = within-year median growth across functions
z_med <- dt_AUS[!is.na(dlog), .(z = median(dlog, na.rm = TRUE)), by = Year]
dt_AUS <- z_med[dt_AUS, on = "Year"]

# (Optional) controls X_t — fill these from your macro merges
# Example placeholders (replace/merge real series):
# dt_AUS_year <- unique(dt_AUS[, .(Year)])
# dt_AUS_year[, ':='(pop_working_to_old = ..., gdp_pc_ppp = ..., unemp = ...)]
# dt_AUS <- dt_AUS_year[dt_AUS, on = "Year"]

# Estimate β_j via median regression Δlog y_{t,j} ~ z_t + X_t  (one model per function)
get_beta <- function(D) {
  # If you have controls, add them to the formula
  # fit <- rq(dlog ~ z + pop_working_to_old + gdp_pc_ppp + unemp, data = D, tau = 0.5)
  fit <- rq(dlog ~ z, data = D, tau = 0.5)
  coef(fit)["z"]
}

betas <- dt_AUS[!is.na(dlog) & !is.na(z),
                .(beta = tryCatch(get_beta(.SD), error = function(e) NA_real_)),
                by = cofog_group]

# Reallocation intensity = dispersion of β across functions
beta_disp <- betas[, .(
  p10 = quantile(beta, 0.10, na.rm = TRUE),
  p25 = quantile(beta, 0.25, na.rm = TRUE),
  p50 = quantile(beta, 0.50, na.rm = TRUE),
  p75 = quantile(beta, 0.75, na.rm = TRUE),
  p90 = quantile(beta, 0.90, na.rm = TRUE)
)]
beta_disp[, `:=`(p90_p10 = p90 - p10, p75_p25 = p75 - p25)]
beta_disp

############################
# Build shares of GDP at Level II, then period averages
cofog_shares <- copy(Consol_toGDP)
cofog_shares$Year <- as.numeric(cofog_shares$Year) + 1997
# Two periods as in paper:
cofog_shares[, period := fifelse(Year %between% c(1997, 2003), "P1_1997_2003",
                                 fifelse(Year %between% c(2012, 2017), "P2_2012_2017", NA_character_))]
cofog_shares <- cofog_shares[!is.na(period)]

# Average shares by Country x Period x Function
panel <- cofog_shares[, .(share = mean(share_gdp, na.rm = TRUE)),
                      by = .(Country, period, cofog_group)]

# → Wide: rows = Country×Period, cols = functions
wide <- dcast(panel, Country + period ~ cofog_group, value.var = "share", fill = 0)

# PCA on functions (as columns)
rownms <- paste(wide$Country, wide$period, sep = "_")
mat <- as.matrix(wide[, -(1:2)])
rownames(mat) <- rownms

pca <- PCA(mat, graph = FALSE)  # PC variance profile ~Fig 5
fviz_eig(pca, addlabels = TRUE) + ggtitle("Explained variance by PCs")

# Plot countries in PC1–PC2 for each period (≈Figs 6 & 7)
coords <- data.table(pca$ind$coord[, 1:2])
coords[, `:=`(Country = wide$Country, Period = wide$period)]

ggplot(coords, aes(Dim.1, Dim.2, label = Country)) +
  geom_point() + ggrepel::geom_text_repel(size = 3) +
  facet_wrap(~Period) +
  labs(title = "Countries in PCA1–PCA2 space (shares of GDP)")

############### σ-convergence: did cross-country dispersion of PC scores fall?
sd_by_pc <- coords[, .(sd = sd(Dim.1)), by = Period][order(Period)]  # demo for PC1
# Better: compute sd for several PCs and compare ratio P2/P1 as in Fig 8:
pc_scores <- as.data.table(pca$ind$coord)
pc_scores[, `:=`(Country = coords$Country, Period = coords$Period)]
sd_ratio <- lapply(1:5, function(j) {
  d <- pc_scores[, .(sd = sd(get(paste0("Dim.", j)))), by = Period][order(Period)]
  data.table(pc = j, ratio = d[Period=="P2_2012_2017"]$sd / d[Period=="P1_1997_2003"]$sd)
}) |> rbindlist()
ggplot(sd_ratio, aes(pc, ratio)) +
  geom_line() + geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "σ-convergence: SD ratio of PC scores (P2 / P1)",
       y = "Cross-country SD ratio (<1 ⇒ convergence)")

# 1) What does PC1 load on?
load1 <- data.table(func = colnames(mat),
                    loading = pca$var$coord[,1])
load1[order(-abs(loading))]  # biggest contributors first
table(sign(load1$loading))   # are they mostly the same sign?

# 2) Is PC1 just "size"? Check correlation with total gov share
total_share <- rowSums(mat)  # sums of COFOG shares = government size (%GDP)
scores <- data.table(Country = wide$Country, Period = wide$period,
                     PC1 = pca$ind$coord[,1], total = total_share)
cor(scores$PC1, scores$total, use="complete.obs")

# Standardize mat the same way PCA did
X <- sweep(mat, 2, pca$call$centre, "-")
if (isTRUE(pca$call$ecart.type)) {
  X <- sweep(X, 2, pca$call$ecart.type, "/")
}

# Map rownames back to Country/Period
row_map <- data.table(row = seq_len(nrow(X)),
                      Country = wide$Country, Period = wide$period)

# Contribution of each COFOG to the change in PC1 score for one country
decompose_pc1_change <- function(cty) {
  r1 <- row_map[Country==cty & Period=="P1_1997_2003", row]
  r2 <- row_map[Country==cty & Period=="P2_2012_2017", row]
  if (length(r1)*length(r2)==0) return(NULL)
  dz   <- X[r2, ] - X[r1, ]                   # change in standardized shares
  cont <- as.numeric(dz) * pca$var$coord[,1]  # variable-wise contributions
  data.table(country=cty, func=colnames(mat), contrib=cont)[order(-abs(contrib))]
}

# Example: which functions moved AUS along PC1?
decompose_pc1_change("Australia")[1:10]



################ k-means on first ~k PCs, choose k by silhouette (≈Tables 9/10)
pc_use <- as.matrix(pca$ind$coord[, 1:k])
sil <- sapply(2:6, function(k) {
  km <- kmeans(pc_use, centers = k, nstart = 50)
  mean(silhouette(km$cluster, dist(pc_use))[, "sil_width"])
})
k_opt <- which.max(sil) + 1
km <- kmeans(pc_use, centers = k_opt, nstart = 200)
coords[, cluster := factor(km$cluster)]
ggplot(coords, aes(Dim.1, Dim.2, color = cluster, label = Country)) +
  geom_point() + ggrepel::geom_text_repel(size = 3) +
  facet_wrap(~Period) +
  labs(title = sprintf("k-means on first 5 PCs (k=%d chosen by silhouette)", k_opt))

## 1) Is PC1 a size axis?
cor(scores$PC1, scores$total, use = "complete.obs")  # expect large + (e.g., >0.8)

## 2) Did average spending/GDP rise?
# keep only countries present in BOTH periods
common <- scores[, .N, by = Country][N == 2, Country]

scores[Country %in% common,
       .(mean_PC1   = mean(PC1),
         median_PC1 = median(PC1),
         mean_total = mean(total),        # average gov size (% GDP)
         median_total = median(total)),
       by = Period][order(Period)]


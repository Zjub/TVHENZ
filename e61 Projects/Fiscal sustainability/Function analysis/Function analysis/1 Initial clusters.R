## Created:  1/08/2025
## Last update:  16/10/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Looking at the initial consolidated data to consider i) defence and interest, ii) capital spending, iii) clustering.

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

rm(list=ls())
gc()

k <- 5

## Import data ----

work = FALSE

if (work == TRUE){
  consolidate_dt <- read_csv("C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
} else{
  consolidate_dt <- read_csv("~/GitHub/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
}


setDT(consolidate_dt)

colnames(consolidate_dt)

unique(consolidate_dt$cofog_group_name)
unique(consolidate_dt$etf_subclass_name)
unique(consolidate_dt$etf_class_name)
unique(consolidate_dt$etf_type_name)

### Defence and interest ----

consolidated_expenses_dt <- consolidate_dt[etf_type_name == "Revenue and expenses"]

# Two categories that are swapped between in data. Combine them
#019 General public services not elsewhere classified
#011 Executive and legislative organs financial and fiscal affairs external affairs

consolidated_expenses_dt[
  cofog_group_name %in% c(
    "019 General public services not elsewhere classified",
    "011 Executive and legislative organs financial and fiscal affairs external affairs"
  ),
  cofog_group_name := "01 General public services"
]

group_exp_dt <- consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_group_name,fin_year)]

unique(as.numeric(consolidated_expenses_dt$cofog_group_code))

consolidated_expenses_dt[, cofog_group_name := fcase(
  !substr(cofog_group_name, 1, 2) %in% c("07","09","10"),substr(cofog_group_name, 1, 2),
  as.numeric(cofog_group_code) %in% seq(74,79,by=1), "74 Other Health",
  as.numeric(cofog_group_code) %in% c(72,73), "73 Hospitals",
  as.numeric(cofog_group_code) > 93 & as.numeric(cofog_group_code) <= 99,"94 Other Education",
  as.numeric(cofog_group_code) == 102 | (as.numeric(substr(cofog_group_name, 1, 3)) > 105 & as.numeric(substr(cofog_group_name, 1, 3)) <= 109),"106 Other Social Protection",default = cofog_group_name)]

consolidated_expenses_dt[, cofog_group_name := fcase(
  cofog_group_name == "01","01 General Public Service",
  cofog_group_name == "02","02 Defence",
  cofog_group_name == "03","03 Public Order",
  cofog_group_name == "04","04 Economic Affairs",
  cofog_group_name == "05","05 Environmental Protection",
  cofog_group_name == "06","06 Amenities",
  #cofog_group_name == "07","07 Health",
  cofog_group_name == "08","08 Culture",
  #cofog_group_name == "09","09 Education",
  cofog_group_name == "11","11 Transport",
  default = cofog_group_name)]

group_exp_dt <- consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_group_name,fin_year)]

for(i in unique(group_exp_dt$cofog_group_name)){
  a <- ggplot(group_exp_dt[cofog_group_name == i],aes(x=fin_year,y=nom_expense)) +
    geom_line() + 
    labs_e61(title = i)
  
  print(a)
}

### Cluster analysis test ----
# Note clustering is fairly detailed work, so put most of this in another script - just set up some basic NN clustering and PCA to give an example of looking at i) what is similar ii) what drives similarity.

# Start with total spending by function.

group_exp_dt <- consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_group_name,fin_year)]
div_exp_dt <- consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]

## Cluster example
wide_exp_dt <- dcast(
  group_exp_dt,
  cofog_group_name ~ fin_year,
  value.var = "nom_expense",
  fun.aggregate = sum,
  na.rm = TRUE
)

colnames(wide_exp_dt)

# Set cofog_group_name as rownames
row_names <- wide_exp_dt$cofog_group_name
wide_exp_dt <- as.data.frame(wide_exp_dt[, -1, with = FALSE])
rownames(wide_exp_dt) <- row_names

# --- 1. Nominal levels ---
dist_nominal <- as.dist(1 - cor(t(wide_exp_dt), method = "pearson"))
hc_nominal <- hclust(dist_nominal, method = "ward.D2")
plot_nominal <- fviz_dend(hc_nominal, k = 10, horiz = TRUE, rect = TRUE, rect_fill = TRUE,
                          main = "Pearson correlation - Nominal levels",
                          cex = 0.6, lwd = 0.4)

# --- 2. Scaled levels (remove magnitude differences) ---
scaled_exp_dt <- scale(wide_exp_dt)
dist_scaled <- as.dist(1 - cor(t(scaled_exp_dt), method = "pearson"))
hc_scaled <- hclust(dist_scaled, method = "ward.D2")
plot_scaled <- fviz_dend(hc_scaled, k = 10, horiz = TRUE, rect = TRUE, rect_fill = TRUE,
                         main = "Pearson correlation - Scaled levels",
                         cex = 0.6, lwd = 0.4)

# --- 3. Year-on-year growth rates ---
wide_exp_growth_dt <- wide_exp_dt %>%
  mutate(across(everything(), ~ (.- dplyr::lag(.)) / dplyr::lag(.),
                .names = "diff_{.col}")) %>%
  select(-1) %>% # drop first growth column
  drop_na()

dist_growth <- as.dist(1 - cor(t(wide_exp_growth_dt), method = "pearson"))
hc_growth <- hclust(dist_growth, method = "ward.D2")
plot_growth <- fviz_dend(hc_growth, k = 10, horiz = TRUE, rect = TRUE, rect_fill = TRUE,
                         main = "Pearson correlation - YoY growth",
                         cex = 0.6, lwd = 0.4)

# --- Arrange in a grid ---
grid.arrange(plot_scaled, plot_growth, ncol = 1)
plot_nominal
plot_scaled
plot_growth

### Broaden out the clustering

# Scale by category - rather than year (note I dont't think the year clustering makes sense)
scaled2 <- t(scale(t(wide_exp_dt), center = TRUE, scale = TRUE))

ts_mat <- as.matrix(scaled2)  # rows = series, cols = years

# Function to compute features for one series
get_features <- function(x) {
  c(
    mean     = mean(x, na.rm = TRUE),
    sd       = sd(x, na.rm = TRUE),
    skew     = moments::skewness(x, na.rm = TRUE),
    kurtosis = moments::kurtosis(x, na.rm = TRUE),
    acf1     = acf(x, plot = FALSE, lag.max = 1)$acf[2],
    slope    = coef(lm(x ~ seq_along(x)))[2]  # trend slope
  )
}

# Compute features for all series

feature_mat <- t(apply(ts_mat, 1, get_features))

## Cluster on these features
# Standard distance
dist_feat <- dist(scale(feature_mat), method = "euclidean")

## Pearson
# corr_mat <- cor(t(feature_mat), method = "pearson")
# dist_feat <- as.dist(1 - corr_mat)

hc_feat <- hclust(dist_feat, method = "ward.D2")

fviz_dend(hc_feat, k = 7, horiz = TRUE, rect = TRUE, rect_fill = TRUE,
          main = "Clustering on time series feature vectors")

##### Ask ChatGPT to help with suggestions to make this more readable.
# --- A) Classic dendrogram (features-based) ---
plot_feat_classic <- fviz_dend(
  hc_feat, k = 7, horiz = TRUE, rect = TRUE, rect_fill = TRUE,
  main = "Hierarchical clustering (feature vectors)",
  hang = -1, cex = 0.6, lwd = 0.4
)

# --- B) Circular dendrogram (same clusters, different layout) ---
plot_feat_circular <- fviz_dend(
  hc_feat, k = 7, type = "circular", rect = TRUE, rect_fill = TRUE,
  main = "Circular dendrogram (feature vectors)",
  cex = 0.6, lwd = 0.4
)

# --- C) Cluster heatmap ---
# Use the wide matrix (rows = categories, cols = years). Scale by row to emphasise patterns.
mat_for_heatmap <- as.matrix(wide_exp_dt)
mat_for_heatmap <- t(scale(t(mat_for_heatmap)))  # scale within each category

# Row clustering: use the features-based tree (hc_feat) for consistency
# Column clustering: build a sensible year dendrogram (correlation/ward)
hc_cols <- hclust(as.dist(1 - cor(mat_for_heatmap, use = "pairwise.complete.obs")), method = "ward.D2")

pheatmap(
  mat_for_heatmap,
  cluster_rows = hc_feat,
  cluster_cols = hc_cols,
  cutree_rows  = 7,           # show row cluster bands
  show_rownames = TRUE,
  show_colnames = TRUE,
  color = colorRampPalette(c("#16396B", "white", "#B81D13"))(101),
  border_color = NA,
  main = "Cluster heatmap (rows: categories, cols: years)"
)

# --- D) PCA scatter coloured by cluster (features space) ---
# Use the same feature matrix used to build hc_feat
feat_df <- as.data.frame(scale(feature_mat))       # scale features for PCA
pca_res <- FactoMineR::PCA(feat_df, graph = FALSE) # PCA on features

pc_dt <- data.table(
  name    = rownames(feat_df),
  PC1     = pca_res$ind$coord[, 1],
  PC2     = pca_res$ind$coord[, 2],
  cluster = factor(cutree(hc_feat, k = 5))
)

plot_pca <- ggplot(pc_dt, aes(PC1, PC2, colour = cluster)) +
  geom_point(size = 2, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = name), size = 1.2, show.legend = FALSE) +
  labs_e61(
    title = "Hierarchical clusters",
    x = "PC1", #sprintf("PC1 (%.1f%%)", 100 * pca_res$eig[1, 2]),
    y = "PC2", #sprintf("PC2 (%.1f%%)", 100 * pca_res$eig[2, 2])
    sources = c("ABS","e61"),
    footnotes = c("Figure shows the first two principal components of time-series features for each expenditure category, with points coloured by their hierarchical cluster membership. Labels indicate expenditure categories. This visual is for illustration only: clusters were estimated on the full feature set, not just the two dimensions shown here.")
  )

# --- E) Arrange a compact gallery for the report (optional) ---
gridExtra::grid.arrange(plot_feat_classic, plot_feat_circular, ncol = 2)
print(plot_pca)   # heatmap prints itself

save_e61("PCA_cluster_forreport.png",res=2)


## Use these clusters to describe the trajectory of each cluster.

k_clusters <- 5
cluster_vec <- factor(cutree(hc_feat, k = k_clusters))
names(cluster_vec) <- names(cluster_vec)  # ensure names exist

# 2) Align matrix rows to the cluster vector
stopifnot(all(names(cluster_vec) %in% rownames(wide_exp_dt)))
mat_aligned <- as.matrix(wide_exp_dt[names(cluster_vec), , drop = FALSE])

# 3) Pick base year (default to "1999"; fallback to first column if absent)
base_year <- "1999"
if (!base_year %in% colnames(mat_aligned)) base_year <- colnames(mat_aligned)[1]

# Guard against zero/NA baseline (avoid division by zero)
baseline <- as.numeric(mat_aligned[, base_year])
baseline[baseline <= 0] <- NA

# 4) Normalise each category so that base_year == 1
norm_mat <- sweep(mat_aligned, 1, baseline, "/")

# 5) Long form + attach cluster labels
traj_dt <- as.data.table(norm_mat, keep.rownames = "name")
traj_dt <- melt(traj_dt, id.vars = "name", variable.name = "year", value.name = "norm")
traj_dt[, year := as.integer(as.character(year))]
traj_dt[, cluster := cluster_vec[name]]

# 6) Unweighted cluster averages (and optional IQR for ribbons)
cluster_mean <- traj_dt[
  , .(
    avg = mean(norm, na.rm = TRUE),
    p25 = suppressWarnings(quantile(norm, 0.25, na.rm = TRUE)),
    p75 = suppressWarnings(quantile(norm, 0.75, na.rm = TRUE))
  ),
  by = .(cluster, year)
]

# 7) Plot: cluster-mean trajectories with optional IQR ribbons
plot_cluster_means <- ggplot(cluster_mean, aes(year, avg, colour = cluster, group = cluster)) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = cluster), alpha = 0.15, colour = NA, show.legend = FALSE) +
  geom_line() +
  labs_e61(
    title = "Spending paths for different clusters",
    y = "Average spending in cluster 1999 = 1",
    x = "Year", 
    sources = c("ABS","e61"),
    footnotes = c("Each function is normalised to 1 in 1999, line represents the unweighted average of expenses within each cluster. The shaded regions represent the range of values within the cluster.")
  ) +
  plab(c("Infrastructure and hospitals","Public order and old age","Cyclical social protection","Youth and medical investments","Housing transfers"),x=rep(1999,5),y=c(8.5,7.5,6.5,5.5,4.5)) +
  scale_y_continuous_e61(limits=c(0,9,3))

print(plot_cluster_means)

save_e61("Cluster_growth.png",res=2)

# 8) (Optional) Small multiples: grey individual lines + black cluster mean
plot_small_multiples <- ggplot() +
  geom_line(data = traj_dt, aes(year, norm, group = name), colour = "grey80", alpha = 0.6) +
  geom_line(data = cluster_mean, aes(year, avg), colour = "black", size = 1) +
  facet_wrap(~ cluster) +
  labs(
    title = sprintf("Within-cluster trajectories (base = %s = 1)", base_year),
    x = "Year", y = "Normalised level (category base = 1)"
  ) 





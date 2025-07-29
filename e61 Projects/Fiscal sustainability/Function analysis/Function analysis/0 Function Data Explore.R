## Last update:  23/07/2025
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

rm(list=ls())
gc()

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


### Assets by function ----


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

# Set cofog_group_name as rownames
row_names <- wide_exp_dt$cofog_group_name
wide_exp_dt <- as.data.frame(wide_exp_dt[, -1, with = FALSE])
rownames(wide_exp_dt) <- row_names

scaled_data_exp <- scale(wide_exp_dt) # Scales to remove level effects

dist_matrix_exp <- dist(scaled_data_exp) # Simple Euclidean distance measure - can complicate later when we have visualisations nailed down
hc_exp <- hclust(dist_matrix_exp, method = "ward.D2")

# Plot dendrogram
dend <- as.dendrogram(hc_exp)
plot(dend, main = "Hierarchical Clustering of Government Functions")

# Cut tree into k clusters
k <- 6
clusters <- cutree(hc_exp, k = k)

clustered_exp_dt <- data.table(cofog_group_name = rownames(scaled_data_exp), cluster = clusters)
clustered_exp_dt[order(cluster)]

group_exp_dt <- merge(group_exp_dt, clustered_exp_dt, by = "cofog_group_name")


group_exp_dt[cluster == 3 & fin_year %in% c(2016,2017,2018)]

## Cluster plots
# 1. Line plots of spending trends by cluster
ggplot(group_exp_dt, aes(x = fin_year, y = nom_expense, group = cofog_group_name, color = as.factor(cluster))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~cluster, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Government Function Spending Trends by Cluster",
       x = "Financial Year", y = "Nominal Expense (mn)",
       color = "Cluster") 

# 2. Boxplots of total spending per cluster
total_exp_cluster <- group_exp_dt[, .(total_expense = sum(nom_expense, na.rm = TRUE)), by = .(cofog_group_name, cluster)]
ggplot(total_exp_cluster, aes(x = as.factor(cluster), y = total_expense)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Total Spending by Cluster",
       x = "Cluster", y = "Total Nominal Expense (mn)") +
  theme_minimal()

# 3. PCA biplot colored by cluster
pca_res <- PCA(scaled_data_exp, graph = FALSE)
fviz_pca_biplot(pca_res,
                label = "none",
                habillage = as.factor(clusters),
                palette = "jco",
                addEllipses = TRUE,
                ellipse.level = 0.95,
                title = "PCA Biplot of Government Functions Colored by Cluster")

# 4. Alluvial plot comparing k=4 and k=6 clusters
clusters_k4 <- cutree(hc_exp, k = 4)
cluster_compare_dt <- data.table(cofog_group_name = rownames(scaled_data_exp),
                                 k4_cluster = clusters_k4,
                                 k6_cluster = clusters)
ggplot(cluster_compare_dt,
       aes(axis1 = as.factor(k4_cluster), axis2 = as.factor(k6_cluster))) +
  geom_alluvium(aes(fill = as.factor(k4_cluster)), width = 0.2) +
  geom_stratum(width = 0.2, fill = "grey") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("k=4", "k=6"), expand = c(.05, .05)) +
  labs(title = "Cluster Membership Changes from k=4 to k=6",
       x = "Cluster Count", y = "Government Functions") +
  theme_minimal()

# 5. Heatmap of scaled data by cluster
ordered_data <- scaled_data_exp[order(clusters), ]
pheatmap(ordered_data,
         cluster_rows = FALSE, cluster_cols = TRUE,
         show_rownames = TRUE,
         annotation_row = data.frame(Cluster = as.factor(clusters)),
         main = "Heatmap of Scaled Spending by Function and Cluster")

# 
# ## PCA example
# # PCA on scaled data
# pca_result <- PCA(scaled_data_exp, graph = FALSE)
# 
# # Scree plot: variance explained
# fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))
# 
# # Biplot of first 2 PCs
# fviz_pca_biplot(pca_result, 
#                 repel = TRUE, 
#                 col.var = "blue", 
#                 col.ind = "red", 
#                 title = "PCA Biplot: Functions and Years")
# 
# # Extract PC scores for functions
# pca_scores <- as.data.table(pca_result$ind$coord)
# pca_scores[, cofog_group_name := rownames(scaled_data)]
# 
# # Import the economic variables we want - OLS regression of those on components, tells us what the components reflect economically.

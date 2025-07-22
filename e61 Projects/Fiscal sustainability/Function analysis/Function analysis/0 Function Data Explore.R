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

consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_group_name,fin_year)]
consolidated_expenses_dt[,.(nom_expense = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]


## Cluster example

library(dendextend)

# Aggregate and pivot data: rows = functions, columns = years
wide_dt <- dcast(
  consolidated_expenses_dt,
  cofog_group_name ~ fin_year,
  value.var = "nom_expense",
  fun.aggregate = sum,
  na.rm = TRUE
)

# Set cofog_group_name as rownames
row_names <- wide_dt$cofog_group_name
wide_dt <- as.data.frame(wide_dt[, -1, with = FALSE])
rownames(wide_dt) <- row_names

# Scale data (normalize each column)
scaled_data <- scale(wide_dt)

# Hierarchical clustering
dist_matrix <- dist(scaled_data)  # Euclidean distance
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
dend <- as.dendrogram(hc)
plot(dend, main = "Hierarchical Clustering of Government Functions")

# Cut tree into k clusters (e.g., k = 4)
k <- 4
clusters <- cutree(hc, k = k)

# Add cluster assignments to the original data
clustered_dt <- data.table(cofog_group_name = rownames(scaled_data), cluster = clusters)
print(clustered_dt[order(cluster)])


## PCA example

library(FactoMineR)
library(factoextra)

# PCA on scaled data
pca_result <- PCA(scaled_data, graph = FALSE)

# Scree plot: variance explained
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Biplot of first 2 PCs
fviz_pca_biplot(pca_result, 
                repel = TRUE, 
                col.var = "blue", 
                col.ind = "red", 
                title = "PCA Biplot: Functions and Years")

# Extract PC scores for functions
pca_scores <- as.data.table(pca_result$ind$coord)
pca_scores[, cofog_group_name := rownames(scaled_data)]

# Import the economic variables we want - OLS regression of those on components, tells us what the components reflect economically.


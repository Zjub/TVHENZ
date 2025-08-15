## Last update:  12/08/2025
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

rm(list=ls())
gc()

k <- 5

## Import data ----

work = TRUE

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

# Think about further consolidation of categories first plot all

for(i in unique(group_exp_dt$cofog_group_name)){
  a <- ggplot(group_exp_dt[cofog_group_name == i],aes(x=fin_year,y=nom_expense)) +
    geom_line() + 
    labs_e61(title = i)
  
  print(a)
}

# Lets first reduce to two digit, and then break out categories that are of interest

# consolidated_expenses_dt[, cofog_group_name := fifelse(
#   substr(cofog_group_name, 1, 2) != 10,substr(cofog_group_name, 1, 2),
#   fifelse(substr(cofog_group_name, 1, 3) > 105,"106 Other Social Protection",cofog_group_name))]

substr(consolidated_expenses_dt$cofog_group_name, 1, 2)
substr(consolidated_expenses_dt$cofog_group_name, 1, 3)
as.numeric(substr(consolidated_expenses_dt$cofog_group_name, 1, 3))

## There is an exisiting group code
# consolidated_expenses_dt[, cofog_group_name := fcase(
#   !substr(cofog_group_name, 1, 2) %in% c(09,10),substr(cofog_group_name, 1, 2),
#   as.numeric(substr(cofog_group_name, 1, 3)) > 93 & as.numeric(substr(cofog_group_name, 1, 3)) <= 99,"94 Other Education",
#   as.numeric(substr(cofog_group_name, 1, 3)) > 105 & as.numeric(substr(cofog_group_name, 1, 3)) <= 109,"106 Other Social Protection",default = cofog_group_name)]

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

colnames(wide_exp_dt)

# Set cofog_group_name as rownames
row_names <- wide_exp_dt$cofog_group_name
wide_exp_dt <- as.data.frame(wide_exp_dt[, -1, with = FALSE])
rownames(wide_exp_dt) <- row_names

scaled_data_exp <- scale(wide_exp_dt) # Scales to remove level effects

dist_matrix_exp <- dist(scaled_data_exp) # Simple Euclidean distance measure - can complicate later when we have visualisations nailed down
hc_exp <- hclust(dist_matrix_exp, method = "ward.D2")

############################################ 
### Comparison set of a variety of different distance measures - give quite different results, so need to evaluate what is the appropriate choice

distance_list <- list(
  Euclidean     = function(x) dist(x, method = "euclidean"),
  Manhattan     = function(x) dist(x, method = "manhattan"),
  PearsonCorr   = function(x) as.dist(1 - cor(t(x), method = "pearson")),
  SpearmanCorr  = function(x) as.dist(1 - cor(t(x), method = "spearman"))
  #Canberra      = function(x) dist(x, method = "canberra"),
  ## Time series measures only make sense if we detrended the data
  #ACF           = function(x) diss(t(x), "ACF"),     # autocorrelation-based
  #PACF          = function(x) diss(t(x), "PACF"),    # partial autocorrelation
  #Wavelet       = function(x) diss(t(x), "DWT"),     # wavelet-based
  #TSCorr        = function(x) diss(t(x), "COR")     # TS correlation
  #DTW           = function(x) proxy::dist(x, method = "dtw_basic")
)

# Generate dendrogram plots
plot_list <- lapply(names(distance_list), function(name) {
  message("Computing distance: ", name)
  dist_matrix <- distance_list[[name]](scaled_data_exp)
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  fviz_dend(hc,
            k = 10, horiz = TRUE, rect = TRUE, rect_fill = TRUE,
            main = paste("Clustering -", name),
            cex = 0.6, lwd = 0.4)
})

# Arrange in a grid
grid.arrange(grobs = plot_list, ncol = 2)


############################################

# Plot dendrogram
dend <- as.dendrogram(hc_exp)
plot(dend, main = "Hierarchical Clustering of Government Functions",
     horiz = TRUE)

hc_comp <- hc_exp
hc_comp$height <- log1p(hc_comp$height)

fviz_dend(hc_comp,
          k = 5,                    # number of clusters (optional)
          horiz = TRUE,              # horizontal layout
          rect = TRUE,               # draw rectangles for clusters
          rect_border = "jco",       # cluster colours
          rect_fill = TRUE,
          cex = 0.8,
          lwd = 0.4) +
  labs_e61(title = "Hierarchical Clustering of Government Functions",subtitle = "",y="",x="") + 
  theme_e61()



# Cut tree into k clusters

clusters <- cutree(hc_exp, k = k)

clustered_exp_dt <- data.table(cofog_group_name = rownames(scaled_data_exp), cluster = clusters)
clustered_exp_dt[order(cluster)]

group_exp_dt <- merge(group_exp_dt, clustered_exp_dt, by = "cofog_group_name")

## For cross checks on 3-digit. Not happy with 3-digit for this due to definitional changes/new R&D categories etc.
# group_exp_dt[cluster == 3 & fin_year %in% c(2016,2017,2018)]
# group_exp_dt[cluster == 1 & fin_year %in% c(2016,2017,2018)]
# 
# 
# group_exp_dt[cluster == 1 & fin_year %in% c(2013,2014,2015)][order(nom_expense)]
# 
# ggplot(group_exp_dt[cofog_group_name == "053 Pollution abatement"],aes(x=fin_year,y=nom_expense)) +
#   geom_line() + 
#   labs_e61(title = "Pollution abatement funding")

group_exp_dt[, .(cofog_list = unique(cofog_group_name)), by = cluster]

## Cluster plots
# 1. Line plots of spending trends by cluster
ggplot(group_exp_dt, aes(x = fin_year, y = nom_expense, group = cofog_group_name, colour = as.factor(cluster))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~cluster, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Government Function Spending Trends by Cluster",
       x = "Financial Year", y = "Nominal Expense (mn)",
       colour = "Cluster") 

# 2. Boxplots of total spending per cluster
total_exp_cluster <- group_exp_dt[, .(total_expense = sum(nom_expense, na.rm = TRUE)), by = .(cofog_group_name, cluster)]
ggplot(total_exp_cluster, aes(x = as.factor(cluster), y = total_expense)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Total Spending by Cluster",
       x = "Cluster", y = "Total Nominal Expense (mn)") +
  theme_minimal()

# 3. PCA biplot coloured by cluster
pca_res <- PCA(scaled_data_exp, graph = FALSE)
fviz_pca_biplot(pca_res,
                label = "none",
                habillage = as.factor(clusters),
                palette = "jco",
                addEllipses = TRUE,
                ellipse.level = 0.95,
                title = "PCA Biplot of Government Functions coloured by Cluster")

# 4. Alluvial plot comparing k=4 and k=k clusters
clusters_k4 <- cutree(hc_exp, k = 4)
cluster_compare_dt <- data.table(cofog_group_name = rownames(scaled_data_exp),
                                 k4_cluster = clusters_k4,
                                 k6_cluster = clusters)
ggplot(cluster_compare_dt,
       aes(axis1 = as.factor(k4_cluster), axis2 = as.factor(k6_cluster))) +
  geom_alluvium(aes(fill = as.factor(k4_cluster)), width = 0.2) +
  geom_stratum(width = 0.2, fill = "grey") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("k=4", "k=k"), expand = c(.05, .05)) +
  labs(title = paste0("Cluster Membership Changes from k=4 to k=",k),
       x = "Cluster Count", y = "Government Functions") 

# 5. Heatmap of scaled data by cluster
ordered_data <- scaled_data_exp[order(clusters), ]
pheatmap(ordered_data,
         cluster_rows = FALSE, cluster_cols = TRUE,
         show_rownames = TRUE,
         annotation_row = data.frame(Cluster = as.factor(clusters)),
         main = "Heatmap of Scaled Spending by Function and Cluster")

group_exp_dt[, .(cofog_list = unique(cofog_group_name)), by = cluster]

# ---- 
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
# pca_scores[, cofog_group_name := rownames(scaled_data_exp)]
# 
# head(pca_result$eig)
# 
# ## PCA on growth rates ----
# 
# wide_exp_growth_dt <- wide_exp_dt %>%
#   mutate(across(everything(),
#                 ~ (.- dplyr::lag(.)) / dplyr::lag(.),
#                 .names = "diff_{.col}")) %>%
#   select(starts_with("diff_")) %>%
#   select(-1)
# 
# colnames(wide_exp_growth_dt)
# 
# # The above keeps rownames from wide_exp_dt, so:
# rownames(wide_exp_growth_dt) <- row_names
# 
# # Remove any rows with NA (first year drops out)
# wide_exp_growth_dt <- wide_exp_growth_dt[complete.cases(wide_exp_growth_dt), ]
# 
# # Scale the growth rates
# scaled_data_growth <- scale(wide_exp_growth_dt)
# 
# # PCA on growth rates
# pca_growth <- PCA(scaled_data_growth, graph = FALSE)
# 
# # Scree plot for growth rate PCA
# fviz_eig(pca_growth, addlabels = TRUE, ylim = c(0, 50)) +
#   labs(title = "PCA on Growth Rates: Variance Explained")
# 
# # Biplot for growth rate PCA
# fviz_pca_biplot(pca_growth,
#                 repel = TRUE,
#                 col.var = "blue",
#                 col.ind = "red",
#                 title = "PCA Biplot: Growth Rates")
# 
# # Import the economic variables we want - OLS regression of those on components, tells us what the components reflect economically.

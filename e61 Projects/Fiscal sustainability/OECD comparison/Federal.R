# Topic: Making some high level Federal plots to motivate investigation
# Author: Matt Nolan
# Created: 8/7/2025
# Last edit: 8/7/2025
# Last editor: Matt Nolan

#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(Hmisc)
library(tidysynth)

## Import data ----

Fed_spend <- Consolidated_Fiscal_Position <- read_excel("Consolidated Fiscal Position.xlsx", 
                                                        sheet = "long_exp")

setDT(Fed_spend)

Fed_spend_dt <- Fed_spend[, c(
  .SD[, lapply(.SD, function(x) x / .SD[[ncol(.SD)]]), .SDcols = 2:(ncol(Fed_spend))],
  .(Year = .SD[[1]])
)]

#Fed_spend_dt[,":=" (`Total expenses` = NULL,`Implied expenses` = NULL)]
Fed_spend_dt <- Fed_spend_dt[,.(`General public services`,`Defence`,`Public order and safety`,`Education`,`Health`,`Social security and welfare`,`Housing and community amenities`,`Recreation and culture`,`Econ Purposes`,Year
)]

Fed_spend_dt_long <- melt(Fed_spend_dt,id.vars = "Year",variable.name = "Category",value.name = "value")

ggplot(Fed_spend_dt_long[Year >= 2008],aes(x=Year,y=value*100,colour=Category)) + geom_line() +
  theme_e61(legend = "bottom") + geom_vline(xintercept = 2024) +
  labs_e61(title = "Share of Expenditure",
           y="%")

## Add Federal Revenue plots

cash_rev <- read_excel("PBO Historical fiscal data - 2025-26 Budget update.xlsx",sheet = "Table 6",skip=2)

setDT(cash_rev)

cash_rev <- cash_rev[!is.na(Units)]
colnames(cash_rev)[1] <- "Category"

nom_tax <- melt(cash_rev[Category %in% c("Goods and services tax","Income taxation receipts total")][,":=" (Units = NULL,Note = NULL)],id.vars = "Category")

years <- sprintf("%d-%02d", 2000:2024, (2001:2025) %% 100)

nom_tax <-nom_tax[variable %in% years]
nom_tax[,value := as.numeric(value)]

NGDP <- read_excel("PBO Historical fiscal data - 2025-26 Budget update.xlsx",sheet = "Table 1", range = "A5:BH6")
setDT(NGDP)

NGDP_number <- unlist(NGDP[, (ncol(NGDP)-24):ncol(NGDP)], use.names = FALSE)

NGDP_dt <- data.table(variable = years,NGDP_data = NGDP_number)


tax_dt <- nom_tax[NGDP_dt,on=.(variable)][,tax_GDP := value/NGDP_data]

breaks_seq <- unique(tax_dt$variable)[seq(1, length(unique(tax_dt$variable)), by = 5)]

ggplot(tax_dt,aes(x=variable,y=tax_GDP*100,colour=Category,group=Category)) + geom_line() +
  scale_y_continuous_e61(limits=c(0,20,4)) +
  scale_x_discrete(breaks = breaks_seq) +
  labs_e61(title = "Revenue as a % GDP",
           sources = c("PBO"),
           y = "%",
           x = "") +
  plab(c("GST","Income tax"),x=c(1,1),y=c(9,11))

save_e61("Rev_history.png",res=2)

ggplot(tax_dt,aes(x=variable,y=tax_GDP*100,colour=Category,group=Category)) + geom_line() +
  scale_y_continuous_e61(limits=c(0,20,4)) +
  scale_x_discrete(breaks = breaks_seq) +
  labs_e61(subtitle = "Revenue as a % GDP",
           sources = c("PBO"),
           y = "%",
           x = "") +
  plab(c("GST","Income tax"),x=c(1,1),y=c(9,11))

save_e61("Rev_history.pdf")

## VRR 2022

data_TJwLC <- read_csv("data-TJwLC.csv")
setDT(data_TJwLC)
colnames(data_TJwLC) <- c("country","VAT_share_tax")

data_TJwLC[, country := factor(country, levels = country[order(VAT_share_tax)])]

ggplot(data_TJwLC,aes(x=country,y=VAT_share_tax)) + geom_col() + coord_flip()


VRR <- read_excel("c49hi7.xlsx", range = "A29:C67")
setDT(VRR)

VRR[, Country := factor(Country, levels = Country[order(VRR)])]

VRR <- VRR[!Country %in% c("Estonia","Latvia","Hungary","Lithuania","Slovenia","Slovak Republic","Iceland","Costa Rica","Colombia","Belgium","Korea","Czechia","Portugal","Netherlands","Israel","Greece")]

ggplot(VRR, aes(x = Country, y = VRR, 
                fill = Country == "Australia")) +
  geom_col() +
  scale_fill_manual(values = c("FALSE" = palette_e61(2)[2], "TRUE" = "gold")) +
  coord_flip() + 
  scale_y_continuous_e61(limits = c(0,1,0.25)) +
  labs_e61(title = "VAT revenue ratio (VRR) in 2022",
           sources = c("OECD"),
           footnotes = c("VRR refers to the ratio of the revenue raised from the VAT/GST system relative to a flat rate applied to final consumption."))

save_e61("VRR.png",res=2)

ggplot(VRR, aes(x = Country, y = VRR, 
                fill = Country == "Australia")) +
  geom_col() +
  scale_fill_manual(values = c("FALSE" = palette_e61(2)[2], "TRUE" = "gold")) +
  coord_flip() + 
  scale_y_continuous_e61(limits = c(0,1,0.25)) +
  labs_e61(subtitle = "VAT revenue ratio (VRR) in 2022",
           sources = c("OECD"),
           footnotes = c("VRR refers to the ratio of the revenue raised from the VAT/GST system relative to a flat rate applied to final consumption."))

save_e61("VRR.pdf")

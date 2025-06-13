# Last edit: 12/06/2025
# Author: Matt Nolan

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
library(ggtext)

rm(list=ls())
gc()

## Marginal Excess Burdens

MEB <- read_excel("Trends_graph_data.xlsx",
                                sheet = "MEB")

setDT(MEB)

MEB[, Tax := factor(Tax, levels = Tax[order(MEB)])]


ggplot(MEB, aes(x = Tax, y = MEB)) +
  geom_col() +
  coord_flip() +
  format_flip() +
  labs_e61(subtitle = "Marginal Excess Burden (cents per dollar raised)",
           y="%",
           x="",
           sources = c("KPMG Econtech","PBO"),
           footnotes = c("Estimates from KPMG (2010) report for the Australia's Future Tax System Review.","Updated Stamp Duty Estimates used by PBO come from Cao et al (2015)")) +
  scale_y_continuous_e61(limits = c(0,100,20))

save_e61("MEB.pdf",pad_width = 2)

## METR

METR <- read_excel("Trends_graph_data.xlsx",
                  sheet = "METR")
setDT(METR)
colnames(METR) <- c("Asset","First","Second","Third","Fourth","Fifth")


#
# ggplot(melt(METR,id.vars = "Asset",variable.name = "Type",value.name = "value"),aes(x=Asset,y=value*100,fill=Type)) + geom_col(position = "dodge") +
#   scale_y_continuous_e61(limits = c(-60,100,20)) +
#   labs_e61(subtitle = "Marginal Effective Tax Rate",
#            y="%",
#            x="",
#            sources = c("Varela, Breunig, and Sobeck (2020), Table 3.1"),
#            footnotes = c("Tax rates used across brackets are: 0%, 21%, 34.5%, 39%, and 47%")) +
#   plab(c("Tax-free treshold","Second bracket","Third bracket","Top bracket"),y=c(85,75,65,55),x=c(4,4,4,4))
#
# save_e61("METR.pdf")


METR <- METR[!Asset %in% c("Superannuation (Div. 293)")]

METR$Asset[5] <- "Concession Super"

# Melt your METR data
melted <- melt(METR, id.vars = "Asset", variable.name = "Type", value.name = "value")

# Create staggered labels using "\n" (newline) to shift every second label lower
assets <- unique(melted$Asset)
staggered_labels <- ifelse(seq_along(assets) %% 2 == 0, paste0("\n", assets), assets)

# Factor to preserve original order
melted[, Asset_f := factor(Asset, levels = assets)]

# Plot with staggered labels
ggplot(melted, aes(x = Asset_f, y = value * 100, fill = Type)) +
  geom_col(position = "dodge") +
  scale_y_continuous_e61(limits = c(-60, 100, 20)) +
  scale_x_discrete(labels = staggered_labels) +
  labs_e61(
    subtitle = "Marginal Effective Tax Rate",
    y = "%",
    x = "",
    sources = c("Varela, Breunig, and Sobeck (2020), Table 3.1"),
    footnotes = c("Tax rates used across brackets are: 0%, 21%, 34.5%, 39%, and 47%")
  ) +
  plab(c("Tax-free treshold","Second bracket","Third bracket","Top bracket"),y=c(85,75,65,55),x=c(2,2,2,2)) +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.ticks.length.x = unit(5, "pt")
  )

save_e61("METR.pdf")
save_e61("METR.png",res=2)


## Asset ETRs

ETR_asset <- read_excel("Trends_graph_data.xlsx",
                  sheet = "ETR_asset")

setDT(ETR_asset)


ETR_long <- melt(ETR_asset, id.vars = "Asset", variable.name = "Type", value.name = "value")

# Create ordering based on EMTR values
ETR_order <- ETR_long[Type == "EMTR"][order(-value), Asset]

# Reorder Asset as factor
ETR_long[, Asset := factor(Asset, levels = ETR_order)]

# Plot with reordered Assets
ggplot(ETR_long, aes(x = Asset, y = value, fill = Type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  format_flip() +
  scale_y_continuous_e61(limits = c(0,50,10)) +
  labs_e61(subtitle = "Implied effective tax rate for business investment",
           y="%",
           x="",
           sources = c("OECD"),
           footnotes = c("Based on 2015 tax system as modelled in Hanappi (2018)")) +
  plab(c("Average Tax Rate","Marginal Tax Rate"),x=c(9,10),y=c(31,31))

save_e61("ETR_asset.pdf",pad_width = 2)
save_e61("ETR_asset.png",res=2,pad_width = 2)

## Fiscal drag



Creep <- read_excel("Trends_graph_data.xlsx",
                        sheet = "FD")
setDT(Creep)

colnames(Creep) <- c("percentile","FY12","FY22")

Creep[,diff := FY22 - FY12]

avg_diff <- mean(Creep$diff)

ggplot(Creep,aes(x=percentile,y=diff*100)) + geom_line() + geom_hline(yintercept = avg_diff*100,linetype="dashed",colour=palette_e61(3)[3]) + geom_hline(yintercept = 0) +
  labs_e61(subtitle = "By population taxable income percentiles",
           y = "ppt",
           x="",
           sources = c("ATO","e61"),
           footnotes = c("Blue line show the percentage point difference between the tax rate on taxable income for a percentile in 2022 relative to 2012. Orange line is the average change.")) +
  scale_y_continuous_e61(limits = c(-1,4,1)) +
  plab(c("Increase in Tax Rate","Population Average"),x=c(1,1),y=c(3.2,2.7),colour = c(palette_e61(3)[2],palette_e61(3)[3]))

save_e61("Drag.pdf")

### Exploring SIH RR data (addition to Matt M's work).
## Data not stored centrally for security - will need a synthetic version for final paper.
# Author: Matt Nolan
# Date made: 31/12/2024
# Last update: 31/12/2024


library(tidyverse)
library(data.table)
library(theme61)
library(readxl)

# Replace with dataset
RR_dt <- read_excel("C:/Users/OEM/Downloads/ReplacementRates.xlsx")
setDT(RR_dt)


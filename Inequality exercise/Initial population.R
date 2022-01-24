rm(list=ls()) # Clear prior memory

library(tidyverse)
library(readxl)
library(cowplot)

# Goal of this project is to understand measures of income inequality, how to make them, and what they mean.
# This initial file is for creating the population of interest for us to analyse.
# Prior to creating the core population, we will also play around with the shapes of a few different distributions and how an inequality measure works as part of a description of these distributions.

set.seed(123)

# Using different synthetic control estimates

rm(list=ls())

.libPaths(new = 'C:/Rpackage')

#install.packages("pacman")

library(pacman)

p_load(
  tidyverse,
  data.table,
  collapse,
  readabs,
  readxl,
  theme61,
  gsynth,
  Synth
)

SCdata <- read_csv("SCdata.csv")[,1:8]

##### Raw levels

### Synth example

basedata <- as.data.frame(SCdata) %>% mutate(time_index = rep(seq(1,144,by=1),times=11))

balancecheck <- basedata %>% pivot_wider(names_from = age,values_from = N)

dataprep.out <- 
  dataprep(
    basedata,
    dependent = "N",
    predictors = c("gender1ratio","NSWratio","VICratio","Queenratio","eduratio"),
    time.predictors.prior = 1:72, #as.Date("2006-07-01"):as.Date("2012-06-01"),
    unit.variable = "age",
    time.variable = "time_index",
    treatment.identifier = 21,
    controls.identifier = c(22:28),
    time.optimize.ssr = 1:72,
    time.plot = 1:144 #as.Date("2006-07-01"):as.Date("2018-06-01")
  )

synth.out <- synth(data.prep.obj = dataprep.out)

pdf("templevel.pdf")
path.plot(synth.out, dataprep.out)

abline(v   = 73,
       lty = 2)

abline(v   = 85,
       lty = 2)
dev.off()

pdf("templevelgap.pdf")
gaps.plot(synth.out, dataprep.out)

abline(v   = 73,
       lty = 2)

abline(v   = 85,
       lty = 2)

abline(h = -15000,
       lty = 3)
dev.off()

## Below is only in SCtools now - which isn't working https://bcastanho.github.io/install_sctools
# install.packages(c("cvTools", "furrr", "stringr","ggplot2",
#                     "stats","dplyr","magrittr","purrr"))

library(devtools)
install_github('bcastanho/SCtools', repos =NULL, force = T)

library(SCtools)

placebos <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 3)

plot_placebos(placebos)

mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)

### gsyth example

gbasedata <- basedata %>% mutate(treatment = ifelse(age == 21,1,0), post = ifelse(month_year >= "2012-07-01",1,0)*treatment)

gsynth_output <- gsynth(value ~ post,data=gbasedata,index=c("N","month_year"))


##### Index
# Part of the weighting is due to the level similarity between different groups - also useful to look at how these change if you use the proportion.

Base <- basedata %>% filter(month_year == "2006-07-01") %>% select(age,N)
basedata1 <- basedata %>% left_join(Base, by="age") %>% mutate(Prop = N.x/N.y)

### Synth example

dataprep.out2 <- 
  dataprep(
    basedata1,
    dependent = "Prop",
    predictors = c("gender1ratio","NSWratio","VICratio","Queenratio","eduratio"),
    time.predictors.prior = 1:72, #as.Date("2006-07-01"):as.Date("2012-06-01"),
    unit.variable = "age",
    time.variable = "time_index",
    treatment.identifier = 21,
    controls.identifier = c(22:28),
    time.optimize.ssr = 1:72,
    time.plot = 1:144 #as.Date("2006-07-01"):as.Date("2018-06-01")
  )

synth.out2 <- synth(data.prep.obj = dataprep.out2)
pdf("tempprop.pdf")
path.plot(synth.out2, dataprep.out2)

abline(v   = 73,
       lty = 2)

abline(v   = 85,
       lty = 2)
dev.off()

## Undertake placebo tests on the constrained estimate.
# Test removing 22 year olds, adding 20 year olds etc

dataprep.out2wide <- 
  dataprep(
    basedata1,
    dependent = "Prop",
    predictors = c("gender1ratio","NSWratio","VICratio","Queenratio","eduratio"),
    time.predictors.prior = 1:72, #as.Date("2006-07-01"):as.Date("2012-06-01"),
    unit.variable = "age",
    time.variable = "time_index",
    treatment.identifier = 21,
    controls.identifier = c(19:20,22:28),
    time.optimize.ssr = 1:72,
    time.plot = 1:144 #as.Date("2006-07-01"):as.Date("2018-06-01")
  )

synth.out2wide <- synth(data.prep.obj = dataprep.out2wide)

path.plot(synth.out2wide, dataprep.out2wide)

abline(v   = 73,
       lty = 2)

abline(v   = 85,
       lty = 2)



# Placebo in space (between units)

placebos2 <- generate.placebos(dataprep.out2, synth.out2, Sigf.ipop = 3)

plot_placebos(placebos2)

gap <- unique(basedata$month_year)

for (i in 1:7){
  temp <- placebos2$df[i] - placebos2$df[i+7]
  gap <- cbind(gap,temp)
}

gap <- cbind(gap,s21 = (placebos2$df[15]-placebos2$df[16]))

colnames(gap)[1] <- "date"

gapdf <- gap %>% pivot_longer(!date,names_to = "variable",values_to = "value")

# Different visulation of the placebo in space tests
ggplot(gapdf,aes(x=date,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = unique(gap$date)[73],linetype = 2,colour="red")+ geom_vline(xintercept = unique(gap$date)[85],linetype = 2,colour="red") + geom_hline(yintercept = 0)


# Placebo in time (checking intervention date)
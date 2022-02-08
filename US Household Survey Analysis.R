
rm(list=ls()) # Clear prior memory

setwd("D:/CPS data")

# Import data

AES <- as.data.frame(read.csv(file="cps_00003.csv",head=TRUE)) # US Age, EDUC (as recommended in documentation above HIGRADE), gender, and the preloaded technical variables
AESdf <- AES[!(AES$YEAR == 2021),] # Removing 2021

library(readxl)

CPI <- read_excel("r-cpi-u-rs-allitems.xlsx",skip=5) # US CPI based on BLS consistent methodology (all items)

CPI2 <- unlist(t(CPI[4:44,]))


CPIyear <- CPI2[seq(4, nrow(CPI2), 12), ] # As the data only refers to March, as per its link to the March basic survey. An alternative is to take the annual average CPI to March.
# plot(CPIyear)

library(tidyverse)
library(spatstat)

### Question 3 (real median income)

## Test code for checking totals for consistency prior to looking at sex

#RealIncomes <- AESdf %>% group_by(YEAR) %>%
#summarise(medInc = weighted.median(INCWAGE,w=ASECWT)) %>%
#  mutate(medRealincome = medInc / CPIyear * CPIyear[41])

#g1 <- ggplot(RealIncomes, aes(x=YEAR, y=medRealincome,colour = "Real median income")) + geom_line(size = 1.5)+ #expand_limits(y=10000) + geom_smooth(aes(colour="Regression"),method = "lm",se=FALSE,linetype = "dashed") + #geom_smooth(aes(colour="Trendline"),se=FALSE,linetype = "dashed")

#g1 <- g1 + ggtitle("Real median income") + labs(y="Real median income", x = "Year")

#g1

repCPI <- rep(CPIyear,each=2) 

RealIncomes <- AESdf %>% group_by(YEAR,SEX) %>%
  summarise(medInc = weighted.median(INCWAGE,w=ASECWT)) %>%
  mutate(SEX = recode(SEX, "1" = "Male", "2" = "Female")) %>%
  ungroup() %>%
  mutate(medRealincome = medInc / repCPI * repCPI[82]) 

g1a <- ggplot(RealIncomes, aes(x=YEAR, y=medRealincome,colour = "Real median income")) + geom_line(size = 1.5)+ expand_limits(y=10000) + geom_smooth(aes(colour="Regression"),method = "lm",se=FALSE,linetype = "dashed") + geom_smooth(aes(colour="Trendline"),se=FALSE,linetype = "dashed")

g1a <- g1a + ggtitle("Real median income, by sex") + labs(y="Real median income (2020 dollars)", x = "Year") + facet_wrap(facets =  vars(SEX)) + scale_y_continuous(labels=scales::dollar_format())

png(file="D:/CPS data/g1a.png",
    width=600, height=350)
g1a
dev.off()



### Question 3 (Weighted counts)

## Check on totals
#a <- count(AESdf,YEAR, zeroinc = INCWAGE == 0, wt= ASECWT)
#Prop <- a[a$zeroinc == TRUE,3] / count(AESdf,YEAR,wt= ASECWT)[,2]
#IncZeroProp <- as_tibble(cbind(YEAR = count(AESdf,YEAR,wt= ASECWT)[,1],Prop))

IncZeroProp <- AESdf %>%
  group_by(YEAR,SEX) %>%
  count(YEAR, SEX, zeroinc = INCWAGE == 0, wt= ASECWT)  %>%
  mutate(SEX = recode(SEX, "1" = "Male", "2" = "Female")) %>%
  mutate(total = sum(n,YEAR)) %>%
  filter(zeroinc != FALSE) %>%
  mutate(Prop = n  / total) %>%
  group_by(SEX)

g2a <- ggplot(IncZeroProp,aes(YEAR,Prop,colour = "Proportion")) + geom_line(size = 1.5)+ expand_limits(y=0.15) + geom_smooth(aes(colour="Regression"),method = "lm",se=FALSE,linetype = "dashed") + geom_smooth(aes(colour="Trendline"),se=FALSE,linetype = "dashed")

g2a <- g2a + ggtitle("Proportion of individuals with zero labour income") + labs(y="Proportion", x = "Year") + facet_wrap(facets =  vars(SEX)) + scale_y_continuous(labels = scales::percent)

png(file="D:/CPS data/g2a.png",
    width=600, height=350)
g2a
dev.off()

### Question Four - education and labour market outcomes for men and women

repCPIyear <- rep(CPIyear,each=6) 

RealIncomesEdu <- AESdf %>% mutate(EdAttain = ifelse(EDUC <73,"No High School",ifelse(EDUC <111,"High School graduate","College graduate"))) %>%
  group_by(YEAR,SEX,EdAttain) %>%
  summarise(medIncome = weighted.median(INCWAGE,w=ASECWT)) %>%
  mutate(SEX = recode(SEX, "1" = "Male", "2" = "Female")) %>%
  ungroup %>%
  mutate(medRealincome = medIncome / repCPIyear * repCPIyear[246]) %>%
  group_by(SEX,EdAttain)

g3 <- ggplot(RealIncomesEdu, aes(x=YEAR, y=medRealincome, group = EdAttain, colour = EdAttain)) + geom_line(size = 1.5)+ expand_limits(y=0)

g3 <- g3 + ggtitle("Real median Income, by Education type") + labs(y="Real median income (2020 dollars)", x = "Year") + facet_wrap(facets =  vars(SEX)) + scale_y_continuous(labels=scales::dollar_format())

png(file="D:/CPS data/g3.png",
    width=600, height=350)
g3
dev.off()

IncZeroPropEdu <- AESdf %>% mutate(EdAttain = ifelse(EDUC <73,"No High School",ifelse(EDUC <111,"High School graduate","College graduate"))) %>%
  group_by(YEAR,SEX,EdAttain) %>%
  count(YEAR, SEX, EdAttain, zeroinc = INCWAGE == 0, wt= ASECWT)  %>%
  mutate(SEX = recode(SEX, "1" = "Male", "2" = "Female")) %>%
  mutate(total = sum(n,YEAR)) %>%
  filter(zeroinc != FALSE) %>%
  mutate(PropEdu = n  / total) %>%
  group_by(SEX,EdAttain)

g4 <- ggplot(IncZeroPropEdu, aes(x=YEAR, y=PropEdu, group = EdAttain, colour = EdAttain)) + geom_line(size = 1.5)+ expand_limits(y=0)

g4 <- g4 + ggtitle("Proportion with zero income, by Sex and Educational Attainment") + labs(y="Proportion by Education type", x = "Year") + facet_wrap(facets =  vars(SEX)) + scale_y_continuous(labels = scales::percent)

png(file="D:/CPS data/g4.png",
    width=600, height=350)
g4
dev.off()

### Question 5 - changes in educational trends

# Check to confirm totals
Eduattainment <- AESdf %>% mutate(EdAttain = ifelse(EDUC <73,"No High School",ifelse(EDUC <111,"High School graduate","College graduate"))) %>%
  group_by(YEAR,EdAttain) %>%
  count(YEAR, EdAttain, wt= ASECWT)  %>%
  ungroup %>%
  group_by(YEAR) %>%
  mutate(total = sum(n,YEAR=YEAR)) %>%
  mutate(PropEdA = n /total)

g5 <- ggplot(Eduattainment, aes(x=YEAR, y=PropEdA, group = EdAttain, colour = EdAttain)) + geom_line(size = 1.5)+ expand_limits(y=0)

g5 <- g5 + ggtitle("Trends in Educational Attainment") + labs(y="Proportion", x = "Year") + scale_y_continuous(labels = scales::percent)

png(file="D:/CPS data/g5.png",
    width=600, height=350)
g5
dev.off()

## Given the importance of the break, decided to report on it with the aggregate. 

#Eduattainment <- AESdf %>% mutate(EdAttain = ifelse(EDUC <73,"No high school",ifelse(EDUC <111,"High School graduate","College graduate"))) %>%
#  group_by(YEAR,SEX,EdAttain) %>%
#  count(YEAR, SEX, EdAttain, wt= ASECWT)  %>%
#  ungroup %>%
#  group_by(YEAR,SEX) %>%
#  mutate(total = sum(n,YEAR=YEAR)) %>%
#  mutate(PropEdA = n /total)

#g5 <- ggplot(Eduattainment, aes(x=YEAR, y=PropEdA, group = EdAttain, colour = EdAttain)) + geom_line(size = 1.5)+ expand_limits(y=0)

#g5 <- g5 + ggtitle("Education trends") + labs(y="Proportion with given Educational Attainment, by Sex", x = "Year") + facet_wrap(facets =  vars(SEX))

#g5

### Question Six - labour market outcomes for young workers

repCPI2year <- rep(CPIyear,each=3) 
# plot(repCPI2year)

EduLMtrend1 <- AESdf %>% mutate(EdAttain = ifelse(EDUC <73,"No High School",ifelse(EDUC <111,"High School graduate","College graduate"))) %>%
  filter(AGE < 31) %>%
  filter(SEX == 1) %>%
  group_by(YEAR,EdAttain) %>%
  summarise(medIncome = weighted.median(INCWAGE,w=ASECWT)) %>%
  ungroup %>%
  mutate(YmedRealincome = medIncome / repCPI2year * repCPI2year[123])

EduLMtrend2 <- AESdf %>% mutate(EdAttain = ifelse(EDUC <73,"No High School",ifelse(EDUC <111,"High School graduate","College graduate"))) %>%
  filter(AGE < 31) %>%
  filter(SEX == 1) %>%
  group_by(YEAR,EdAttain) %>%
  count(YEAR, EdAttain, zeroinc = INCWAGE == 0, wt= ASECWT)  %>%
  mutate(total = sum(n,YEAR)) %>%
  filter(zeroinc != FALSE) %>%
  mutate(YPropEdu = n  / total)
  

g6 <- ggplot(EduLMtrend1, aes(x=YEAR, y=YmedRealincome, group = EdAttain, colour = EdAttain)) + geom_line(size = 1.5)+ expand_limits(y=0)

g6 <- g6 + ggtitle("Real median income (Young group), by Education type") + labs(y="Real median income (2020 dollars)", x = "Year")  + scale_y_continuous(labels=scales::dollar_format())

png(file="D:/CPS data/g6.png",
    width=600, height=350)
g6
dev.off()

g7 <- ggplot(EduLMtrend2, aes(x=YEAR, y=YPropEdu, group = EdAttain, colour = EdAttain)) + geom_line(size = 1.5)+ expand_limits(y=0)

g7 <- g7 + ggtitle("Proportion of zero incomes (Young group), by Education type") + labs(y="Proportion", x = "Year") + scale_y_continuous(labels = scales::percent)

png(file="D:/CPS data/g7.png",
    width=600, height=350)
g7
dev.off()

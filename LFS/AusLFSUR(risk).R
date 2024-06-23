# Unemployment risk from ABS website

rm(list=ls())

.libPaths(new = 'C:/Rpackage')

#devtools::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(readabs)
library(tidyverse)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)

## Note that the base figures pulled in are already weighted, so this is the distribution of weighted values.

UQ2a <- read_excel("UQ2a.xlsx", sheet = "Data 1", skip = 3)
colnames(UQ2a)[1] <- "dates"
colnames(UQ2a)[4] <- "Industry"
colnames(UQ2a)[5] <- "UnemFT"
colnames(UQ2a)[6] <- "UnemPT"
colnames(UQ2a)[7] <- "Duration"
Un <- UQ2a %>% group_by(dates,Industry) %>% summarise(Unemp = sum(UnemFT) + sum(UnemPT))
Un2 <- UQ2a %>% filter(`Reason left or lost last job` == "Lost last job") %>% group_by(dates,Industry) %>% summarise(Unemp = sum(UnemFT) + sum(UnemPT))

Emp <- read_excel("6291004Emp.xlsx", sheet = "Original")
colnames(Emp)[1] <- "dates"
Emp <- Emp[as.Date(Emp$dates) > as.Date(1,origin="1990-12-01"),]
Emp <- Emp[,1:20] # Drop the total


EmpPL <- Emp %>% pivot_longer(!dates,names_to= "Industry",values_to="Employment")
EmpPL$Industry <- substr(EmpPL$Industry,1,nchar(EmpPL$Industry)-20)

Em <- EmpPL
Un <- Un %>% filter(Industry %in% Em$Industry)
Un2 <- Un2 %>% filter(Industry %in% Em$Industry)

unique(Em$Industry)
unique(Un$Industry)

df <- merge(Un, Em, by = c("dates","Industry"), all= TRUE)

TotalAnnUR <- df %>% mutate(year = as.numeric(substr(dates,1,4))) %>% group_by(year) %>% summarise(Unemployment = sum(Unemp),Employment = sum(Employment)) %>% mutate(Total = Unemployment/(Unemployment + Employment))

df <- df %>% mutate(UR = Unemp/(Unemp + Employment))

dfAnn <- merge(Un, Em, by = c("dates","Industry"), all= TRUE) %>% mutate(year = as.numeric(substr(dates,1,4))) %>% group_by(year,Industry) %>% summarise(Unemployment = sum(Unemp),Employment = sum(Employment)) %>% mutate(UR = Unemployment/(Unemployment + Employment))

dfAnn %>% group_by(Industry) %>% summarise(mean(UR))

dfAnn2 <- dfAnn %>% pivot_wider(year, names_from = Industry,values_from = UR) 
dfAnn2 <- as.tibble(cbind(dfAnn2,"Total" = TotalAnnUR[,4])) # mutate not working for some reason
dfAnn2 <- dfAnn2 %>% pivot_longer(!year,names_to = "Industry",values_to="UR")


#unique(df$Employment)

head(df)

ggplot(df,aes(y=UR,x=dates,colour=Industry)) + geom_line()

ggplot(df %>% filter(as.Date(dates) > as.Date(1,origin="2001-12-01")),aes(y=UR,x=dates,colour=Industry)) + geom_line() + gghighlight(max(UR))+ labs(
  title = "Quarterly unemployment by industry",
  subtitle = "UR by industry of most recent employment",
  caption = "Source: ABS LFS"
)

ggplot(dfAnn %>% filter(year > 2002),aes(y=UR,x=year,colour=Industry)) + geom_line() + gghighlight() + 
  facet_wrap(~ Industry == "Wholesale Trade") + labs(
  title = "Annual unemployment by industry",
  subtitle = "UR by industry of most recent employment",
  caption = "Source: ABS LFS"
)

dfAnn$Industry[which.min(dfAnn$UR)]

ggplot(dfAnn2 %>% filter(year > 2002),aes(y=UR,x=year,colour=Industry)) + geom_line(size = 0.8) + gghighlight(Industry %in% c("Accommodation and Food Services","Retail Trade","Arts and Recreation Services","Total")) + labs(
    title = "Annual unemployment by industry",
    subtitle = "UR by industry of most recent employment",
    caption = "Source: ABS LFS"
) + theme_e61(base_family = "Quattrocento Sans", legend = "right", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"))


ggplot(dfAnn2 %>% filter(year > 2002),aes(y=UR,x=year,colour=Industry)) + geom_line(size = 0.8) + gghighlight() + facet_wrap(~ Industry) + labs(
  title = "Annual unemployment by industry",
  subtitle = "UR by industry of most recent employment",
  caption = "Source: ABS LFS"
) 


#highrisk <- c("Accommodation and Food Services","Retail Trade","Arts and Recreation Services")
#lowrisk <- c("Mining","Transport, Postal and Warehousing","Public Administration and Safety")

highrisk <- dfAnn %>% group_by(Industry) %>% summarise(mean(UR)) %>% top_n(5) %>% select(Industry)
highrisk <- as.list(highrisk)

#type <- ifelse(dfAnn2$Industry %in% highrisk,1,ifelse(dfAnn2$Industry %in% lowrisk,2,0))

#dfAnn2a <- dfAnn2 %>% mutate(type = type)

#ggplot(dfAnn2a %>% filter(year > 2002),aes(y=UR,x=year,colour=Industry)) + geom_line(size = 0.8) + gghighlight(use_direct_label = FALSE) + facet_wrap(~ type) + labs(
#  title = "Annual unemployment by industry",
#  subtitle = "Unemployment rate by industry of most recent employment",
#  caption = "Source: ABS, e61"
#) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "Year", y= "") 


#+ theme_e61(base_size = 10,base_family = "Quattrocento Sans", legend = "bottomn", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"))

### Underemployment rates by industry to throw in as well.


UU <- read_excel("UnderU.xlsx", sheet = "Data")
colnames(UU)[1] <- "dates"

UUPL <- UU %>% pivot_longer(!dates,names_to = "Industry",values_to = "Underemployment")
UUPL$Industry <- substr(UUPL$Industry,1,nchar(UUPL$Industry)-36)

UUl <- UUPL %>% filter(Industry %in% Em$Industry)

unique(Em$Industry)
unique(Un$Industry)
unique(UUl$Industry)

df2 <- merge(Un, Em, by = c("dates","Industry"), all= TRUE)
df2 <- merge(df2, UUPL) %>% mutate(UnderUnem = Underemployment + Unemp)

#type <- ifelse(df2$Industry %in% highrisk,"High Risk",ifelse(df2$Industry %in% lowrisk,"Low Risk","Moderate risk"))
type <- ifelse(df2$Industry %in% highrisk,"High Risk",ifelse(df2$Industry %in% lowrisk,"Low Risk","Moderate risk"))

TotalAnnUU <- df2 %>% mutate(year = as.numeric(substr(dates,1,4))) %>% group_by(year) %>% summarise(Unemployment = sum(Unemp),Employment = sum(Employment),Underemployment = sum(Underemployment)) %>% mutate(Total = Underemployment/(Unemployment + Employment))

dfUUAnn <- df2 %>% mutate(year = as.numeric(substr(dates,1,4))) %>% group_by(year,Industry) %>% summarise(Unemployment = sum(Unemp),Employment = sum(Employment),Underemployment = sum(Underemployment)) %>% mutate(UU = Underemployment/(Unemployment + Employment))

dfUUAnn2 <- dfUUAnn %>% pivot_wider(year, names_from = Industry,values_from = UU) 
dfUUAnn2 <- as.tibble(cbind(dfUUAnn2,"Total" = TotalAnnUU[,5])) # mutate not working for some reason
dfUUAnn2 <- dfUUAnn2 %>% pivot_longer(!year,names_to = "Industry",values_to="UU")

ggplot(dfUUAnn2 %>% filter(year > 2002),aes(y=UU,x=year,colour=Industry)) + geom_line(size = 0.8) + gghighlight(Industry %in% c("Accommodation and Food Services","Public Administration and Safety","Total")) + labs(
  title = "Annual underemployment by industry",
  subtitle = "Underemployment by industry of employment",
  caption = "Source: ABS LFS"
) + theme_e61(base_family = "Quattrocento Sans", legend = "right", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"))


g1 <- ggplot(dfAnn2 %>% filter(year > 2002),aes(y=UR,x=year,colour=Industry)) + geom_line(size = 0.8) + gghighlight(Industry %in% highrisk,use_direct_label = FALSE) + labs(
  title = "High risk: Annual unemployment by industry",
  subtitle = "Unemployment rate by industry of most recent employment",
  caption = "Source: ABS, e61"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "Year", y= "") + scale_color_manual(values=c("#E69F00","#56B4E9","#009E73"))

g2 <- ggplot(dfAnn2 %>% filter(year > 2002),aes(y=UR,x=year,colour=Industry)) + geom_line(size = 0.8) + gghighlight(Industry %in% lowrisk,use_direct_label = FALSE) + labs(
  title = "Low risk: Annual unemployment by industry",
  subtitle = "Unemployment rate by industry of most recent employment",
  caption = "Source: ABS, e61"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "Year", y= "") + scale_color_manual(values=c("#E69F00","#56B4E9","#009E73"))

g3 <- ggplot(dfUUAnn2 %>% filter(year > 2002),aes(y=UU,x=year,colour=Industry)) + geom_line(size = 0.8) + gghighlight(Industry %in% highrisk,use_direct_label = FALSE) + labs(
  title = "High risk: Annual underemployment by industry",
  subtitle = "Underemployment by industry of employment",
  caption = "Source: ABS LFS"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "Year", y= "") + scale_color_manual(values=c("#E69F00","#56B4E9","#009E73"))

g4 <- ggplot(dfUUAnn2 %>% filter(year > 2002),aes(y=UU,x=year,colour=Industry)) + geom_line(size = 0.8) + gghighlight(Industry %in% lowrisk,use_direct_label = FALSE) + labs(
  title = "Low risk: Annual underemployment by industry",
  subtitle = "Underemployment by industry of employment",
  caption = "Source: ABS LFS"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "Year", y= "") + scale_color_manual(values=c("#E69F00","#56B4E9","#009E73"))

# High, middle, low only

#highrisk <- c("Accommodation and Food Services","Retail Trade","Arts and Recreation Services")
#lowrisk <- c("Mining","Transport, Postal and Warehousing","Public Administration and Safety")

highrisk <- dfAnn %>% group_by(Industry) %>% summarise(mean(UR)) #%>% top_n(10) %>% select(Industry)
highrisk <- as.list(highrisk) # This isn't working - check more later
highrisk <- c("Accommodation and Food Services","Administrative and Support Services","Agriculture, Forestry and Fishing","Construction","Manufacturing") 
highrisk2 <- c("Accommodation and Food Services","Administrative and Support Services","Agriculture, Forestry and Fishing","Construction","Manufacturing","Arts and Recreation Services","Retail Trade") # Question of whether to include arts and rec, retail.

#type <- ifelse(df2$Industry %in% highrisk,"High Risk",ifelse(df2$Industry %in% lowrisk,"Low Risk","Moderate risk"))
type <- ifelse(df2$Industry %in% highrisk,"High Risk","Other Industries")

df3 <- as.tibble(cbind(df2,type)) %>% mutate(year = as.numeric(substr(dates,1,4))) %>% group_by(year,type) %>% summarise(Unemployment = sum(Unemp),Employment = sum(Employment),Underemployment = sum(Underemployment)) %>% mutate(UR = Unemployment/(Unemployment + Employment)) %>% mutate(UU = Underemployment/(Unemployment + Employment))

g5 <- ggplot(df3 ,aes(y=UR,x=year,colour=type)) + geom_line(size = 0.8) + labs(
  title = "Annual unemployment by industry",
  subtitle = "Unemployment by industry of prior employment",
  caption = "High Risk Industries refer to the top five industries by average unemployment over the past 30 years: 
  Accommodation and Food Services, Administrative and Support Services, 
  Agriculture, Forestry and Fishing, Construction, and Manufacturing. 
  
  Source: ABS, e61"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),plot.title = element_text(size=16),plot.subtitle= element_text(size=10)) + labs(x = "Year", y= "") + scale_color_manual(values=c("#E69F00","#56B4E9","#009E73"))

g6 <- ggplot(df3 ,aes(y=UU,x=year,colour=type)) + geom_line(size = 0.8) + labs(
  title = "Annual underemployment by industry",
  subtitle = "Underemployment by industry of employment",
  caption = "High Risk Industries refer to the top five industries by average unemployment over the past 30 years: 
  Accommodation and Food Services, Administrative and Support Services,
  Retail Trade, Arts and Recreation Services,
  Agriculture, Forestry and Fishing, Construction, and Manufacturing. 
  
  Source: ABS LFS"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "Year", y= "") + scale_color_manual(values=c("#E69F00","#56B4E9","#009E73")) # This was using the separate high risk category - won't run that automatically so the capiton is inaccurate.


df4 <- merge(Un2, Em, by = c("dates","Industry"), all= TRUE)

df4 %>% group_by(Industry) %>% summarise(mean(Unemp/Employment))

#type <- ifelse(df4$Industry %in% highrisk,"High Risk",ifelse(df4$Industry %in% lowrisk,"Low Risk","Moderate risk"))
type <- ifelse(df4$Industry %in% highrisk,"High Risk","Other Industries")

LFAUR <- as.tibble(cbind(df4,type)) %>% mutate(year = as.numeric(substr(dates,1,4))) %>% group_by(year,type) %>% summarise(Unemployment = sum(Unemp,na.rm=TRUE),Employment = sum(Employment,na.rm=TRUE)) %>% mutate(UR = Unemployment/(Unemployment + Employment))

g7 <- ggplot(LFAUR ,aes(y=UR,x=year,colour=type)) + geom_line(size = 0.8) + labs(
  title = "Annual unemployment (lost job) by industry",
  subtitle = "Underemployment by industry of employment",
  caption = "Source: ABS LFS"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "Year", y= "") + scale_color_manual(values=c("#E69F00","#56B4E9","#009E73"))

# %>% filter(year > 2002)

## Now add high and low of both UR and UE to same graph

df3b <- df3 %>% select(year,type,UR,UU) %>% pivot_longer(!c(year,type),names_to = "RateName",values_to = "Rate")

ggplot(df3b ,aes(y=Rate,x=year,colour=type)) + geom_line(size = 0.8) + labs(
  title = "Annual underutilisation by industry",
  subtitle = "By industry of current or prior employment",
  caption = "Source: ABS LFS"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "Year", y= "") + scale_color_manual(values=c("#E69F00","#56B4E9","#009E73"))


### Bar graphs by industry

bdf <- merge(Un, Em, by = c("dates","Industry"), all= TRUE) 

bdf2 <- merge(bdf,Un2, by = c("dates","Industry"), all= TRUE) 

bardf <- bdf2 %>% mutate(year = as.numeric(substr(dates,1,4))) %>% group_by(year,Industry) %>% summarise(Unemployment = sum(Unemp.x),Employment = sum(Employment), LJUnemployed = sum(Unemp.y)) %>% mutate(UR = Unemployment/(Unemployment + Employment),LJUR = LJUnemployed/(Unemployment + Employment)) 

bardf2 <- bardf %>% group_by(Industry) %>% summarise(Unemployment_Rate = mean(UR), Lost_Job_UR = mean(LJUR)) %>% pivot_longer(!Industry,names_to = "Unemployment_Type", values_to = "Rate")

ggplot(bardf2 %>% filter(Unemployment_Type == "Unemployment_Rate"), aes(x = reorder(Industry,Rate), y = Rate, colour = "black", fill = "blue")) + geom_bar(stat = "identity",position = "dodge", colour = "black", fill = "#56B4E9")+ labs(
  title = "Average Unemployment Rates",
  subtitle = "By industry of current or prior employment, 1991-2022",
  caption = "Source: ABS, e61"
) + theme_e61(base_size = 9,legend = "none", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),plot.title = element_text(size=16),plot.subtitle= element_text(size=10)) + labs(x = "", y= "Unemployment Rate") + coord_flip() + geom_hline(yintercept = mean(TotalAnnUR$Total), linetype = "dashed", colour ="black", size=1.2)  #+ facet_wrap(~Unemployment_Type)

#scale_x_discrete(labels = c("Acc & Food", "Administration","Agriculture", "Arts and Recreation", "Construction","Education", "Utilities","Finance","Health Care","Media","Manufacturing","Mining","Other Services","Professional Services","Public Administration","Rental Services","Retail Trade","Transport and Storage","Wholesale Trade"))


#+ geom_hline(yintercept = mean(TotalAnnUR$Total), linetype = "dashed", colour ="black", size=1.2) 

## Bring in wage growth

# Data is only from Nov 1994, so annual average growth based on comparing May 2022 to May 1995.  This also means reconstructing the UR info to be about the same period.

bardf1995 <- bdf2 %>% mutate(year = as.numeric(substr(dates,1,4))) %>% filter(year < 1994) %>% group_by(year,Industry) %>% summarise(Unemployment = sum(Unemp.x),Employment = sum(Employment), LJUnemployed = sum(Unemp.y)) %>% mutate(UR = Unemployment/(Unemployment + Employment),LJUR = LJUnemployed/(Unemployment + Employment)) 

bardf19952 <- bardf %>% group_by(Industry) %>% summarise(Unemployment_Rate = mean(UR), Lost_Job_UR = mean(LJUR)) %>% pivot_longer(!Industry,names_to = "Unemployment_Type", values_to = "Rate")

TotalAnnUR1995 <- TotalAnnUR %>% filter(year > 1994)

ggplot(bardf19952 %>% filter(Unemployment_Type == "Unemployment_Rate"), aes(x = reorder(Industry,Rate), y = Rate, colour = "black", fill = "blue")) + geom_bar(stat = "identity",position = "dodge", colour = "black", fill = "#56B4E9")+ labs(
  title = "Average Unemployment Rates",
  subtitle = "By industry of current or prior employment, 1995-2022",
  caption = "Source: ABS, e61"
) + theme_e61(base_size = 8,legend = "none", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "", y= "Unemployment Rate") + coord_flip() + geom_hline(yintercept = mean(TotalAnnUR1995$Total), linetype = "dashed", colour ="black", size=1.2)  #+ facet_wrap(~Unemployment_Type)

Ind <- unique(bdf2[,2]) # It won't read industry and I don't have time to clean it up right now

barUR <- bardf19952 %>% filter(Unemployment_Type == "Unemployment_Rate") %>% select("Industry","Rate")

WeeklyEarn <- read_excel("WeeklyEarn.xlsx",sheet = "Data")

barWageUR <- merge(barUR,WeeklyEarn,"Industry")
colnames(barWageUR) = c("Industry","Unemployment Rate","Weekly Earnings Growth","Weekly FT Earnings Growth")
barWageURPL <- barWageUR %>% pivot_longer(!Industry,names_to = "variable",values_to = "value")

ggplot(barWageURPL, aes(x = Industry, y = value, fill = variable)) + geom_bar(stat = "identity",position = "dodge")+ labs(
  title = "Average Unemployment Rates",
  subtitle = "By industry of current or prior employment, 1995-2022",
  caption = "Source: ABS, e61"
) + theme_e61(base_size = 8,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%")) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "", y= "Unemployment Rate") + coord_flip() + geom_hline(yintercept = mean(TotalAnnUR1995$Total), linetype = "dashed", colour ="black", size=1.2)  #+ facet_wrap(~Unemployment_Type)

ggplot(barWageUR, aes(x= `Unemployment Rate`, y=`Weekly Earnings Growth`, colour = Industry)) + geom_point() + geom_text(label= barWageUR$Industry)

ggplot(barWageUR, aes(x= `Unemployment Rate`, y=`Weekly Earnings Growth`,label=Industry)) + geom_point() + geom_smooth(method=lm,se=FALSE) + labs(
  title = "Industry Unemployment and Average Earnings Growth",
  subtitle = "Average unemployment by recent industry and average weekly earnings growth by industry, 1995 - 2022",
  caption = "Source: ABS, e61"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),plot.title = element_text(size=16),plot.subtitle= element_text(size=10)) + labs(x = "Unemployment Rate", y= "Earnings Growth") + scale_color_manual(values=c("#E69F00","#56B4E9","#009E73")) + scale_x_continuous(labels=scales::percent_format(scale=100,suffix="%")) + gghighlight(Industry %in% "Accommodation and Food Services",unhighlighted_params = list(colour = NULL, alpha = 0.3)) + geom_label(nudge_y = 0.0015,nudge_x = -0.007) + geom_point(col = "dark red", size = 2)

ggplot(barWageUR, aes(x= `Unemployment Rate`, y=`Weekly FT Earnings Growth`,colour = Industry)) + geom_point()

### Transitions

## Check weights

dfgf <- read_lfs_grossflows()

colnames(dfgf)

length(unique(dfgf$age))

EmployedtoUnemployed <- dfgf %>% group_by(date,lfs_current,lfs_previous) %>% summarise(person = sum(persons)) %>% filter((lfs_previous == "Employed full-time" |  lfs_previous == "Employed part-time") & lfs_current =="Unemployed") %>% ungroup %>% group_by(date) %>% summarise(person = sum(person))

EmployedtoUnemployedAge <- dfgf %>% group_by(date,age,lfs_current,lfs_previous) %>% summarise(person = sum(persons)) %>% filter((lfs_previous == "Employed full-time" |  lfs_previous == "Employed part-time") & lfs_current =="Unemployed") %>% ungroup %>% group_by(date,age) %>% summarise(person = sum(person))

Employment <- dfgf %>% group_by(date,lfs_current,lfs_previous) %>% summarise(person = sum(persons)) %>% filter((lfs_current == "Employed full-time" |  lfs_current == "Employed part-time")) %>% ungroup %>% group_by(date) %>% summarise(person = sum(person))

EmploymentAge <- dfgf %>% group_by(date,age,lfs_current,lfs_previous) %>% summarise(person = sum(persons)) %>% filter((lfs_current == "Employed full-time" |  lfs_current == "Employed part-time")) %>% ungroup %>% group_by(date,age) %>% summarise(person = sum(person))

#age2 <- ifelse(EmployedtoUnemployedAge$age == "15-19 years" | EmployedtoUnemployedAge$age == "20-24 years", "15-24","25 and older")

#EmployedtoUnemployedAge <- as.tibble(cbind(EmployedtoUnemployedAge,age2))
#colnames(EmployedtoUnemployedAge)[4] <- "age2"

age2 <- ifelse(EmploymentAge$age == "15-19 years" | EmploymentAge$age == "20-24 years", "15-24","25 and older")

EmploymentAge <- as.tibble(cbind(EmploymentAge,age2))
colnames(EmploymentAge)[4] <- "age2"

Transitionrate <- EmployedtoUnemployed[2:nrow(EmployedtoUnemployed),]$person/Employment[1:(nrow(Employment)-1),]$person

TransitionrateAge <- EmployedtoUnemployedAge[2:nrow(EmployedtoUnemployedAge),]$person/EmploymentAge[1:(nrow(EmploymentAge)-1),]$person

Transdate <- EmployedtoUnemployed[2:nrow(EmployedtoUnemployed),1]

AllTrans <- as.tibble(cbind(Transdate,Transitionrate))

ggplot(AllTrans, aes(x=Transdate, y=Transitionrate)) + geom_line()

plot(AllTrans)

Transdateage <- merge(EmploymentAge,EmployedtoUnemployedAge, by=c("date","age"), all=TRUE)

TransdateAge <- cbind(Transdateage[(length(unique(dfgf$age))+1):nrow(Transdateage),1:4],Transition = Transdateage[1:(nrow(Transdateage)-length(unique(dfgf$age))),5]) 

TransdateAge <- TransdateAge %>% mutate(TransitionRate = Transition/person.x)
colnames(TransdateAge) <- c("date","age1","Employed","age","Transitioned","Transitionrate")

ggplot(TransdateAge, aes(x=date, y=Transitionrate, colour = age)) + geom_line()  + gghighlight(max(Transitionrate) > 0.03)

TransdateFinal <- TransdateAge %>% select(date,age,Transitionrate) %>% pivot_wider(date, names_from = age,values_from = Transitionrate) %>% mutate(Total = AllTrans[,2]) 

colnames(TransdateFinal[,ncol(TransdateFinal)]) <- "Total"

TransdateFinal[,ncol(TransdateFinal)] <- as.numeric(TransdateFinal[,ncol(TransdateFinal)])

TransdateFinal <- TransdateFinal %>% pivot_longer(!date,names_to = "age",values_to = "Transitionrate")

ggplot(TransdateFinal, aes(x=date, y=Transitionrate, colour = age)) + geom_line(size=0.8)  + gghighlight(max(Transitionrate) > 0.04) + labs(
  title = "Monthly transition to unemployment",
  subtitle = "Proportion of employment, by age group",
  caption = "Source: ABS LFS, readabs R package"
)

# Lets make an annual transition series

AnnTransdateAge <- merge(EmploymentAge,EmployedtoUnemployedAge, by=c("date","age"), all=TRUE)

AnnTransdateAge <- cbind(Transdateage[(length(unique(dfgf$age))+1):nrow(Transdateage),1:4],Transition = Transdateage[1:(nrow(Transdateage)-length(unique(dfgf$age))),5]) 

AnnTransdateAge <- AnnTransdateAge %>% mutate(year = as.numeric(substr(date,1,4))) %>% group_by(year,age2) %>% summarise(AnnTransition = sum(Transition,na.rm=TRUE), AnnEmp = sum(person.x,na.rm=TRUE)) %>% mutate(AnnTransitionRate = AnnTransition/AnnEmp)

ggplot(AnnTransdateAge %>% filter(year > 2002), aes(x=year, y=AnnTransitionRate, colour = age2)) + geom_line(size=0.8) + labs(
  title = "Annual average monthly transition to unemployment",
  subtitle = "Proportion of employment, by age group",
  caption = "Source: ABS, e61, readabs R package"
) +  theme_e61(base_size = 9,legend = "bottom", legend_title = FALSE) + scale_y_continuous(labels=scales::percent_format(scale=100,suffix="%"),sec.axis = dup_axis()) + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),plot.title = element_text(size=14),plot.subtitle= element_text(size=9)) + labs(x = "Year", y= "") + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + labs(x = "Year", y= "") + scale_color_manual(values=c("#E69F00","#56B4E9"))




### Goal is to create a script that constructs trend separation and job finding rates.
# Here the flows relate to all labour market transitions, not just EU and UE

# Clearing memory and setting up packages
rm(list=ls())

.libPaths(new = 'C:/Rpackage')

remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

#install.packages("pacman")

library(pacman)

p_load(
  tidyverse,
  data.table,
  collapse,
  readabs,
  readr,
  readxl,
  mFilter,
  zoo,
  theme61
)

# Pull in the gross flows data using readabs
# First do the current weights UR for a comparison

lfd <- read_lfs_grossflows(weights = "current")

lfd <- lfd %>% filter(!age == "65 years and over") %>% mutate(age2 = case_when(
  substr(age,1,5) %in% c("15-19","20-24") ~ "15-24",
  substr(age,1,5) %in% c("25-29","30-34") ~ "25-34",
  substr(age,1,5) %in% c("35-39","40-44") ~ "35-44",
  substr(age,1,5) %in% c("45-49","50-54") ~ "45-54",
  substr(age,1,5) %in% c("55-59","60-64") ~ "55-64",
))

lf <- lfd %>% filter(lfs_current %in% c("Employed full-time","Employed part-time","Unemployed")) %>% filter(!age == "65 years and over") %>% group_by(date) %>% summarise(number = sum(persons))

unemployed <- lfd %>% filter(lfs_current %in% c("Unemployed")) %>% filter(!age == "65 years and over") %>% group_by(date) %>% summarise(number_une = sum(persons))

UR <- left_join(unemployed,lf,by="date") %>% mutate(UR = number_une/number) %>% select(date,UR)

ggplot(lf,aes(x=date,y=number)) + geom_line()

ggplot(UR,aes(x=date,y=UR)) + geom_line()

lf_curr_age <- lfd %>% filter(lfs_current %in% c("Employed full-time","Employed part-time","Unemployed")) %>% filter(!age == "65 years and over") %>% group_by(date,age2) %>% summarise(number = sum(persons))

unemployed_curr_age <- lfd %>% filter(lfs_current %in% c("Unemployed")) %>% filter(!age == "65 years and over") %>% group_by(date,age2) %>% summarise(number_une = sum(persons))

UR_curr_age <- left_join(unemployed_curr_age,lf_curr_age,by=c("date","age2")) %>% mutate(UR = number_une/number) %>% ungroup() %>% group_by(date) %>% mutate(total = sum(number),LF_share = number/total)

UR_ann <- UR_curr_age %>% mutate(year=substr(date,1,4)) %>% group_by(age2,year) %>% summarise(U = sum(number_une),LF=sum(number),UR = U/LF) %>% rename(age = age2,date = year,"Unemployment Rate" = UR) %>% select(!c("LF","U"))

setDT(UR_ann)


### Do a shift share analysis of raw unemployment rate between June 2005 and June 2018 - purpose of raw is that it is linearly decomposable


# [To be coded]




### Now use job flows data to look at trends and implied secular unemployment rate

# We have Full and Part-Time transitions as well as to NILF and unemployed. These counts are by date, age, state, and sex - allowing a decomposition of all of them using public data.
dfgf <- read_lfs_grossflows(weights = "previous") 

# Dataset focused on the five year age groups only

gfage <- dfgf %>% filter(!age == "65 years and over") %>% mutate(age2 = case_when(
  substr(age,1,5) %in% c("15-19","20-24") ~ "15-24",
  substr(age,1,5) %in% c("25-29","30-34") ~ "25-34",
  substr(age,1,5) %in% c("35-39","40-44") ~ "35-44",
  substr(age,1,5) %in% c("45-49","50-54") ~ "45-54",
  substr(age,1,5) %in% c("55-59","60-64") ~ "55-64",
)) %>% group_by(date,lfs_current,lfs_previous,age2) %>% summarise(number = sum(persons), .groups = 'drop')

# Want to generate the respective flows for the steady state UR estimate as per Shimer 2012
# Need to construct three labour market states and transitions between them.
# As we are using previous weights the "unmatched" and "outgoing" categories are empty.

lfdf <- gfage %>% mutate(l_curr = case_when(
  substr(lfs_current,1,3) == "Emp" ~ "Employed",
  substr(lfs_current,1,3) == "Une" ~ "Unemployed",
  substr(lfs_current,1,3) == "Not" ~ "Other",
  TRUE ~ "Missing"
)) %>% mutate(l_pre = case_when(
  substr(lfs_previous,1,3) == "Emp" ~ "Employed",
  substr(lfs_previous,1,3) == "Une" ~ "Unemployed",
  substr(lfs_previous,1,3) == "Not" ~ "Other",
  TRUE ~ "Missing"
)) %>% group_by(date,l_pre,l_curr,age2) %>% summarise(number = sum(number), .groups = 'drop')

#### Separation probabilities
# EU
# EN

sepage <- lfdf %>% filter(l_pre == "Employed") # Construct all those previously employed

# Get all the people who were previously employed - irrespective of where they transition
preemp_emp1 <- sepage %>% group_by(date,l_pre,age2) %>% summarise(number = sum(number), .groups = 'drop') # Numbers previously employed by age and date

preemp_totals <- sepage %>%
  group_by(date, l_pre) %>%
  summarise(age2 = "Total", number = sum(number), .groups = 'drop') # Total employed by date

preemp_emp <- bind_rows(preemp_emp1, preemp_totals) # Adding a total category

preemp_emp <- preemp_emp %>% arrange(date, l_pre, age2)

setDT(preemp_emp)


# Get people previously employed who are now unemployed (EU)
preemp_une1 <- sepage %>% filter(l_curr == "Unemployed") %>% group_by(date,l_pre,age2) %>% summarise(number = sum(number), .groups = 'drop') # Numbers previously employed who are currently unemployed by age and date.

preemp_utotals <- sepage %>%
  filter(l_curr == "Unemployed") %>%
  group_by(date, l_pre) %>%
  summarise(age2 = "Total", number = sum(number), .groups = 'drop') # Total currently unemployed who were previously employed by date.

preemp_une <- bind_rows(preemp_une1, preemp_utotals) # Adding a total category

preemp_une <- preemp_une %>% arrange(date, l_pre, age2)

setDT(preemp_une)

preemp_EU <- preemp_une[preemp_emp,on = .c("date","age2")] %>% mutate(EU_rate = number/i.number) # Calculating the separation rate as the ratio between the number that separate as proportion of those employed,

preemp_ann_EU <- preemp_EU %>% mutate(year=substr(date,1,4)) %>% group_by(age2,year) %>% summarise(EU = sum(number),E=sum(i.number),EU_ann = EU/E, .groups = 'drop') # Annual average

ggplot(preemp_EU,aes(x=date,y=EU_rate,colour=age2)) + geom_line() + labs(title="Separation rate by age")

ggplot(preemp_ann_EU,aes(x=as.numeric(year),y=EU_ann,colour=age2)) + geom_line() + labs(title="Annual average separation rate by age")


# Get people previously employed who are now NILF (EN)

preemp_nil1 <- sepage %>% filter(l_curr == "Other") %>% group_by(date,l_pre,age2) %>% summarise(number = sum(number), .groups = 'drop') # Number previously employed who are currently NILF by age and date

preemp_ntotals <- sepage %>%
  filter(l_curr == "Other") %>%
  group_by(date, l_pre) %>%
  summarise(age2 = "Total", number = sum(number), .groups = 'drop') # Total number who were previously employed and currently NILF

preemp_nil <- bind_rows(preemp_nil1, preemp_ntotals) # Combining the age data with totals

preemp_nil <- preemp_nil %>% arrange(date, l_pre, age2)

setDT(preemp_nil)

preemp_EN <- preemp_nil[preemp_emp,on = .c("date","age2")] %>% mutate(EN_rate = number/i.number)

preemp_ann_EN <- preemp_EN %>% mutate(year=substr(date,1,4)) %>% group_by(age2,year) %>% summarise(EN = sum(number),E=sum(i.number),EN_ann = EN/E) %>% ungroup()

ggplot(preemp_EN,aes(x=date,y=EN_rate,colour=age2)) + geom_line() + labs(title="Separation rate by age")

ggplot(preemp_ann_EN,aes(x=as.numeric(year),y=EN_ann,colour=age2)) + geom_line() + labs(title="Annual average separation to NILF rate by age")

#### Transitions from unemployment
# UE
# UN

# Get all people who were previously unemployed
uxage <- lfdf %>% filter(l_pre == "Unemployed") 

preune_une1 <- uxage %>% group_by(date,l_pre,age2) %>% summarise(number = sum(number), .groups = 'drop')

preune_totals <- uxage %>%
  group_by(date, l_pre) %>%
  summarise(age2 = "Total", number = sum(number), .groups = 'drop')

preune_une <- bind_rows(preune_une1, preune_totals)

preune_une <- preune_une %>% arrange(date, l_pre, age2)

setDT(preune_une)

# Get all people who were previously unemployed and became employed (UE)
preune_emp1 <- uxage %>% filter(l_curr == "Employed") %>% group_by(date,l_pre,age2) %>% summarise(number = sum(number), .groups = 'drop')

preune_etotals <- uxage %>%
  filter(l_curr == "Employed") %>%
  group_by(date, l_pre) %>%
  summarise(age2 = "Total", number = sum(number), .groups = 'drop')

preune_emp <- bind_rows(preune_emp1, preune_etotals)

preune_emp <- preune_emp %>% arrange(date, l_pre, age2)

setDT(preune_emp)

preune_UE <- preune_emp[preune_une,on = .c("date","age2")] %>% mutate(UE_rate = number/i.number)

preune_ann_UE <- preune_UE %>% mutate(year=substr(date,1,4)) %>% group_by(age2,year) %>% summarise(UE = sum(number),U=sum(i.number),UE_ann = UE/U) %>% ungroup()

ggplot(preune_UE,aes(x=date,y=UE_rate,colour=age2)) + geom_line() + labs(title="Job-finding rate by age")

ggplot(preune_ann_UE,aes(x=as.numeric(year),y=UE_ann,colour=age2)) + geom_line() + labs(title="Annual average U job-finding rate by age")

# Now look at all the people who transitioned from unemployed to NILF (UN)
preune_nil1 <- uxage %>% filter(l_curr == "Other") %>% group_by(date,l_pre,age2) %>% summarise(number = sum(number), .groups = 'drop')

preune_ntotals <- uxage %>%
  filter(l_curr == "Other") %>%
  group_by(date, l_pre) %>%
  summarise(age2 = "Total", number = sum(number), .groups = 'drop')

preune_nil <- bind_rows(preune_nil1, preune_ntotals)

preune_nil <- preune_nil %>% arrange(date, l_pre, age2)

setDT(preune_nil)

preune_UN <- preune_emp[preune_une,on = .c("date","age2")] %>% mutate(UN_rate = number/i.number)

preune_ann_UN <- preune_UN %>% mutate(year=substr(date,1,4)) %>% group_by(age2,year) %>% summarise(UE = sum(number),U=sum(i.number),UN_ann = UE/U) %>% ungroup()

ggplot(preune_UN,aes(x=date,y=UN_rate,colour=age2)) + geom_line() + labs(title="Job-finding rate by age")

ggplot(preune_ann_UN,aes(x=as.numeric(year),y=UN_ann,colour=age2)) + geom_line() + labs(title="Annual average U to NILF rate by age")

ggplot(preune_ann_UN %>% filter(age2 %in% c("15-24","Total")),aes(x=as.numeric(year),y=UN_ann,colour=age2)) + geom_line() + theme_e61(legend = "bottom") + labs_e61(
  title="Unemployed to NILF rate",
  subtitle = "Annual average",
  y ="  ",
  sources = c("ABS","e61")) + scale_colour_e61(n=6) + scale_y_continuous_e61(limits = c(0.1,0.25),labels=scales::percent_format())


#### Transitions from NILF
# NE
# NU

# Get all people who were previously NILF
nilage <- lfdf %>% filter(l_pre == "Other") 

prenil_nil1 <- nilage %>% group_by(date,l_pre,age2) %>% summarise(number = sum(number), .groups = 'drop')

prenil_totals <- nilage %>%
  group_by(date, l_pre) %>%
  summarise(age2 = "Total", number = sum(number), .groups = 'drop')

prenil_nil <- bind_rows(prenil_nil1, prenil_totals)

prenil_nil <- prenil_nil %>% arrange(date, l_pre, age2)

setDT(prenil_nil)


# Get all people who were previously NILF and became employed (NE)
prenil_emp1 <- nilage %>% filter(l_curr == "Employed") %>% group_by(date,l_pre,age2) %>% summarise(number = sum(number), .groups = 'drop')

prenil_etotals <- nilage %>%
  filter(l_curr == "Employed") %>%
  group_by(date, l_pre) %>%
  summarise(age2 = "Total", number = sum(number), .groups = 'drop')

prenil_emp <- bind_rows(prenil_emp1, prenil_etotals)

prenil_emp <- prenil_emp %>% arrange(date, l_pre, age2)

setDT(prenil_emp)

prenil_NE <- prenil_emp[prenil_nil,on = .c("date","age2")] %>% mutate(NE_rate = number/i.number)

prenil_ann_NE <- prenil_NE %>% mutate(year=substr(date,1,4)) %>% group_by(age2,year) %>% summarise(NE = sum(number),N=sum(i.number),NE_ann = NE/N, .groups = 'drop')

ggplot(prenil_NE,aes(x=date,y=NE_rate,colour=age2)) + geom_line() + labs(title="Job-finding rate for NILF by age")

ggplot(prenil_ann_NE,aes(x=as.numeric(year),y=NE_ann,colour=age2)) + geom_line() + labs(title="Annual average N job-finding rate by age")

# Get all people who were previously NILF and became unemployed (NU)
prenil_une1 <- nilage %>% filter(l_curr == "Unemployed") %>% group_by(date,l_pre,age2) %>% summarise(number = sum(number), .groups = 'drop')

prenil_utotals <- nilage %>%
  filter(l_curr == "Unemployed") %>%
  group_by(date, l_pre) %>%
  summarise(age2 = "Total", number = sum(number), .groups = 'drop')

prenil_une <- bind_rows(prenil_une1, prenil_utotals)

prenil_une <- prenil_une %>% arrange(date, l_pre, age2)

setDT(prenil_une)

prenil_NU <- prenil_une[prenil_nil,on = .c("date","age2")] %>% mutate(NU_rate = number/i.number)

prenil_ann_NU <- prenil_NU %>% mutate(year=substr(date,1,4)) %>% group_by(age2,year) %>% summarise(NU = sum(number),N=sum(i.number),NU_ann = NU/N, .groups = 'drop')

ggplot(prenil_NE,aes(x=date,y=NU_rate,colour=age2)) + geom_line() + labs(title="Job-finding rate for NILF by age")

ggplot(prenil_ann_NU,aes(x=as.numeric(year),y=NU_ann,colour=age2)) + geom_line() + labs(title="Annual average N job-finding rate by age")



###### Link the varying rates

rates <- data.frame(date = preemp_EU$date, age = preemp_EU$age2,EU = preemp_EU$EU_rate,EN = preemp_EN$EN_rate,UE = preune_UE$UE_rate,UN = preune_UN$UN_rate,NE = prenil_NE$NE_rate,NU = prenil_NU$NU_rate)


SUR <- rates %>% transmute(date = date,age=age,SUR = (EN*NU + NE*EU + NU*EU)/(EN*NU + NE*EU +NU*EU + UN*NE + NU*UE + NE*UE))

ggplot(SUR,aes(x=date,y=SUR,colour=age)) + geom_line()

ann_rates <- data.frame(date = preemp_ann_EU$year, age = preemp_ann_EU$age2,EU = preemp_ann_EU$EU_ann,EN = preemp_ann_EN$EN_ann,UE = preune_ann_UE$UE_ann,UN = preune_ann_UN$UN_ann,NE = prenil_ann_NE$NE_ann,NU = prenil_ann_NU$NU_ann)

SUR_ann <- ann_rates %>% transmute(date = date,age=age,SUR = (EN*NU + NE*EU + NU*EU)/(EN*NU + NE*EU +NU*EU + UN*NE + NU*UE + NE*UE))

setDT(SUR_ann)

ggplot(SUR_ann,aes(x=as.numeric(date),y=SUR,colour=age)) + geom_line() + labs(title = "Steady State UR")

# Set up "fixed at 2004" (first full year) values.

fixed_values <- ann_rates %>%
  filter(date == 2004) %>%
  select(!c(date))

ann_rates <- ann_rates %>% left_join(fixed_values,by="age",suffix = c("","_2004"))

SUR_ann_fix_EU <- ann_rates %>% transmute(date = date,age=age,SUR_EU = (EN_2004*NU_2004 + NE_2004*EU + NU_2004*EU)/(EN_2004*NU_2004 + NE_2004*EU +NU_2004*EU + UN_2004*NE_2004 + NU_2004*UE_2004 + NE_2004*UE_2004)) 

setDT(SUR_ann_fix_EU)

ggplot(SUR_ann_fix_EU,aes(x=as.numeric(date),y=SUR_EU,colour=age)) + geom_line() + labs(title = "Steady State UR - only SR varied")

SUR_ann_fix_UE <- ann_rates %>% transmute(date = date,age=age,SUR_UE = (EN_2004*NU_2004 + NE_2004*EU_2004 + NU_2004*EU_2004)/(EN_2004*NU_2004 + NE_2004*EU_2004 +NU_2004*EU_2004 + UN_2004*NE_2004 + NU_2004*UE + NE_2004*UE)) 

setDT(SUR_ann_fix_UE)

ggplot(SUR_ann_fix_UE,aes(x=as.numeric(date),y=SUR_UE,colour=age)) + geom_line() + labs(title = "Steady State UR - only JFR varied")

SUR_ann_fix_UE_EU <- ann_rates %>% transmute(date = date,age=age,SUR_EUE = (EN_2004*NU_2004 + NE_2004*EU + NU_2004*EU)/(EN_2004*NU_2004 + NE_2004*EU +NU_2004*EU + UN_2004*NE_2004 + NU_2004*UE + NE_2004*UE)) 

setDT(SUR_ann_fix_UE_EU)

SUR[age == "15-24" & SUR == max(SUR)]


###

SURdata <- SUR_ann[SUR_ann_fix_EU,on=.(date,age)][SUR_ann_fix_UE,on=.(date,age)][UR_ann,on=.(date,age)] 

SURdf <- SURdata %>% rename("Structural Unemployment Rate" = SUR, "Only Separation Varies" = SUR_EU, "Only Job-Finding Varies" = SUR_UE) %>% pivot_longer(!c(date,age),names_to = "variable",values_to = "value")

ggplot(SURdf,aes(x=as.numeric(date),y=value,colour=age)) + geom_line() + facet_wrap("variable", nrow = 2)

# Gap between structural rates and actual through time - to see the issues associated with using these measures

Diff_UR <- SUR_ann[UR_ann, diff := SUR - `Unemployment Rate`, on=.(date,age)] %>% select(!SUR)

ggplot(Diff_UR,aes(x=as.numeric(date),y=diff,colour=age)) + geom_line() + geom_hline(yintercept = 0)


# Change in SR as contributor to change in UR

SURdata_diff <- SURdata %>% mutate(EU_gap = SUR - SUR_EU,UE_gap = SUR - SUR_UE) %>% select(date,age,EU_gap,UE_gap)

SUR_compare_1524 <- SURdata[SUR_2004,on=.(age)] %>% filter(age == "15-24") %>% select(date,SUR,SUR_EU,SUR_UE,i.SUR) %>% pivot_longer(!date,names_to = "variable",values_to = "value")

ggplot(SUR_compare_1524,aes(x=as.numeric(date),y=value,colour=variable)) + geom_line()

plot(SURdata_diff$EU_gap)
plot(SURdata_diff$UE_gap)

# Contribution is relative to staying at 2004 level
SUR_2004 <- SURdata %>% filter(date == 2004) %>% select(date,age,SUR)
SURcont <- SURdata[SUR_2004,on=.(age)] %>% mutate(EU_cont = (SUR - SUR_EU)/(SUR - i.SUR),UE_cont = (SUR - SUR_UE)/(SUR - i.SUR)) %>% filter(date > 2004) %>% select(date,age,EU_cont,UE_cont) %>% pivot_longer(!c(date,age),names_to = "variable",values_to = "value")

ggplot(SURcont,aes(x=as.numeric(date),y=value,colour=variable)) + geom_line() + facet_wrap("age")

# Also check scaled by the change

SURcont_absdiff <- SURdata[SUR_2004,on=.(age)] %>% mutate(EU_cont = (SUR - SUR_EU),UE_cont = (SUR - SUR_UE),Actual = (SUR - i.SUR)) %>% filter(date > 2004 & date < 2020) %>% select(date,age,EU_cont,UE_cont,Actual) %>% rename("Separation unchanged" = EU_cont,"Job-Finding unchanged" = UE_cont) %>% pivot_longer(!c(date,age),names_to = "variable",values_to = "value") 

SURcont_absdiff$label <- ifelse(SURcont_absdiff$age == "15-24", "Unemployment Rate", NA)
SURcont_absdiff$label2 <- ifelse(SURcont_absdiff$age == "15-24", "Job-Finding unchanged", NA)
SURcont_absdiff$label3 <- ifelse(SURcont_absdiff$age == "15-24", "Separation unchanged", NA)

ggplot(SURcont_absdiff %>% filter(!age == "55-64"),aes(x=as.numeric(date),y=value,colour=variable,linetype=variable)) + geom_line() + 
  geom_label(data = subset(SURcont_absdiff, !is.na(label)),aes(label = label, x = 2015, y = -0.015),colour = e61_palette(3)[1],inherit.aes = FALSE) + geom_label(data = subset(SURcont_absdiff, !is.na(label2)),aes(label = label2, x = 2014, y = -0.025),colour = e61_palette(3)[2],inherit.aes = FALSE) +
  geom_label(data = subset(SURcont_absdiff, !is.na(label3)),aes(label = label3, x = 2014, y = 0.04),colour = e61_palette(3)[3],inherit.aes = FALSE) +
  facet_wrap("age") + labs(
  title = "Change in the UR from 2004 level",
  y = "%   ",
  x = "Year",
  caption = "Source: ABS DOMINO, e61") + theme_e61() + scale_y_continuous_e61(limits=c(-0.04,0.05,by=0.02),labels = scales::percent_format()) + scale_colour_e61(n=3) + geom_hline(yintercept = 0)

save_e61("C:/Users/MattNolan/e61 Institute Ltd/e61042022 - COVID Income Support and Spending - General/Labour supply/Risk/PreCOVID.pdf",height=20,width=20)


# Just removed the COVID constraint
SURcont_absdiff <- SURdata[SUR_2004,on=.(age)] %>% mutate(EU_cont = (SUR - SUR_EU),UE_cont = (SUR - SUR_UE),Actual = (SUR - i.SUR)) %>% filter(date > 2004) %>% select(date,age,EU_cont,UE_cont,Actual) %>% rename("Separation unchanged" = EU_cont,"Job-Finding unchanged" = UE_cont) %>% pivot_longer(!c(date,age),names_to = "variable",values_to = "value") 

SURcont_absdiff$label <- ifelse(SURcont_absdiff$age == "15-24", "Unemployment Rate", NA)
SURcont_absdiff$label2 <- ifelse(SURcont_absdiff$age == "15-24", "Job-Finding unchanged", NA)
SURcont_absdiff$label3 <- ifelse(SURcont_absdiff$age == "15-24", "Separation unchanged", NA)

ggplot(SURcont_absdiff %>% filter(!age == "55-64"),aes(x=as.numeric(date),y=value,colour=variable,linetype=variable)) + geom_line() + 
  geom_label(data = subset(SURcont_absdiff, !is.na(label)),aes(label = label, x = 2015, y = -0.015),colour = e61_palette(3)[1],inherit.aes = FALSE) + geom_label(data = subset(SURcont_absdiff, !is.na(label2)),aes(label = label2, x = 2014, y = -0.025),colour = e61_palette(3)[2],inherit.aes = FALSE) +
  geom_label(data = subset(SURcont_absdiff, !is.na(label3)),aes(label = label3, x = 2014, y = 0.04),colour = e61_palette(3)[3],inherit.aes = FALSE) +
  facet_wrap("age") + labs(
    title = "Change in the UR from 2004 level",
    y = "%   ",
    x = "Year",
    caption = "Source: ABS DOMINO, e61") + theme_e61() + scale_y_continuous_e61(limits=c(-0.04,0.05,by=0.02),labels = scales::percent_format()) + scale_colour_e61(n=3) + geom_hline(yintercept = 0)

save_e61("C:/Users/MattNolan/e61 Institute Ltd/e61042022 - COVID Income Support and Spending - General/Labour supply/Risk/IncCOVID.pdf",height=20,width=20)

## Create a difference plot from the "actual"

setDT(SURcont_absdiff)

SURcont_absdiff_wide <- dcast(SURcont_absdiff, date + age ~ variable, value.var = "value")

# Calculate differences
SURcont_absdiff_wide[, `:=`(`Only Job Finding Changes` = `Actual` - `Separation unchanged`, `Only Separation Changes` = `Actual` - `Job-Finding unchanged`)]

SURcont_absdiff_wide[, c(`Actual`,`Separation unchanged`,`Job-Finding unchanged`) := NULL]

SURcont_absdiff_long <- melt(SURcont_absdiff_wide, id.vars = c("date", "age"), 
                measure.vars = c("Only Job Finding Changes", "Only Separation Changes"), 
                variable.name = "variable", 
                value.name = "value")

SURcont_absdiff_long$label <- ifelse(SURcont_absdiff_long$age == "15-24", "Only Separation Changes", NA)
SURcont_absdiff_long$label2 <- ifelse(SURcont_absdiff_long$age == "15-24", "Only Job-Finding Changes", NA)

ggplot(SURcont_absdiff_long %>% filter(!age == "55-64"),aes(x=as.numeric(date),y=value,colour=variable)) + geom_line() + 
  geom_label(data = subset(SURcont_absdiff_long, !is.na(label)),aes(label = label, x = 2015, y = -0.02),colour = e61_palette(3)[1],inherit.aes = FALSE) + 
  geom_label(data = subset(SURcont_absdiff_long, !is.na(label2)),aes(label = label2, x = 2014, y = 0.025),colour = e61_palette(3)[2],inherit.aes = FALSE) +
  facet_wrap("age") + labs(
    title = "Contribution of increase in Unemployment Rate from 2004",
    y = "%   ",
    x = "Year",
    caption = "Source: ABS DOMINO, e61") + theme_e61() + scale_y_continuous_e61(limits=c(-0.03,0.03,by=0.01),labels = scales::percent_format()) + scale_colour_e61(n=3) + geom_hline(yintercept = 0)


# Feel like something went wrong with the above - the sum of the two components should equal the actual annual change!
SUR_compare_1524 # Shows that this is equivalent. Issue is that this isn't a decomposition as it is steady states dumbass, they are counterfactual steady states if only that flow changed permanently.

# The above insult isn't quite right.  The SUR is made up of six different flows changing, and those are the contribution of 2 flows - the four other flows account for the residual.

ggplot(SUR_compare_1524,aes(x=as.numeric(date),y=value,colour=variable)) + geom_line()


## Make nicer plots
# split_data <- split(SURcont_absdiff, SURcont_absdiff$age)
# 
# # Create a named list of plots
# plot_list <- lapply(split_data, function(df) {
#   ggplot(df, aes(x = as.numeric(date), y = value, colour = variable)) +
#     geom_line() + theme_e61() +
#     labs(title = paste("Change in UR from 2004 level:", unique(df$age)),
#          y="",
#          x="Year") #+
#     #scale_y_continuous(labels = scales::percent())
# })
# 
# names(plot_list) <- paste0("plot_SUR_age_", unique(SURcont_absdiff$age))
# 
# list2env(plot_list, envir = .GlobalEnv) # This is cool, I would have just assigned the plots in the loop normally!
# 
# mpanel_e61(`plot_SUR_age_15-24`,`plot_SUR_age_25-34`,`plot_SUR_age_35-44`,`plot_SUR_age_45-54`,
#            title = "XXX",
#            subtitle = "Annual average secular unemployment",
#            )




















########### Separate play code for later using ts package
#### Generate Trends
## HP trends

# age_groups <- unique(preune$age2)
# 
# # JFR HP trends
# get_JFR_hpfilter_plot <- function(age_group) {
#   # Get the time series
#   ts_data <- ts(preune[preune$age2 == age_group,]$jf_rate, start = c(2003,6), frequency = 12)
#   
#   # Apply the Hodrick-Prescott filter
#   hp_result <- hpfilter(ts_data, freq = 1600) # adjust freq parameter as necessary
#   
#   # Plot the original series and trend
#   p <- ggplot() +
#     geom_line(aes(x = time(ts_data), y = ts_data), colour = "blue") +
#     geom_line(aes(x = time(hp_result$trend), y = hp_result$trend), colour = "red") +
#     labs(x = "Time", y = "Rate", title = paste("Original Series and HP Filter Trend for age group", age_group)) +
#     theme_minimal()
#   
#   print(p) # print the plot
#   
#   # Return the HP filter result
#   return(hp_result)
# }
# 
# # Use lapply to apply this function to each age group
# JFR_hp_list <- lapply(age_groups, get_JFR_hpfilter_plot)
# 
# # Assign names to the list elements
# names(JFR_hp_list) <- age_groups
# 
# # Sep rate HP
# get_SR_hpfilter_plot <- function(age_group) {
#   # Get the time series
#   ts_data <- ts(preemp[preemp$age2 == age_group,]$sep_rate, start = c(2003,6), frequency = 12)
#   
#   # Apply the Hodrick-Prescott filter
#   hp_result <- hpfilter(ts_data, freq = 1600) # adjust freq parameter as necessary
#   
#   # Plot the original series and trend
#   p <- ggplot() +
#     geom_line(aes(x = time(ts_data), y = ts_data), colour = "blue") +
#     geom_line(aes(x = time(hp_result$trend), y = hp_result$trend), colour = "red") +
#     labs(x = "Time", y = "Rate", title = paste("Original Series and HP Filter Trend for SR and age group", age_group)) +
#     theme_minimal()
#   
#   print(p) # print the plot
#   
#   # Return the HP filter result
#   return(hp_result)
# }
# 
# # Use lapply to apply this function to each age group
# SR_hp_list <- lapply(age_groups, get_SR_hpfilter_plot)
# 
# # Assign names to the list elements
# names(SR_hp_list) <- age_groups
# 
# # Create filters - start with the Burger King filter (Baxter King dipshiz)
# 
# #### Generate secular unemployment rate implied
# 
# # Assume hp_results_list1 and hp_results_list2 are lists of Hodrick-Prescott filter results
# # They should have the same names and the same length
# 
# # Create a function to calculate the ratio
# calc_ratio <- function(age_group) {
#   trend1 <- SR_hp_list[[age_group]]$trend
#   trend2 <- JFR_hp_list[[age_group]]$trend
#   
#   # Calculate the sum of the trends for each time period
#   trend_sum <- trend1 + trend2
#   
#   # Calculate the ratio of the first trend to the sum
#   ratio <- trend1 / trend_sum
#   
#   return(ratio)
# }
# 
# # Use lapply to apply this function to each age group
# ratio_list <- lapply(names(SR_hp_list), calc_ratio)
# 
# # Assign names to the list elements
# names(ratio_list) <- names(SR_hp_list)
# 
# head(ratio_list)
# 
# SUR_ratio <- do.call("cbind", ratio_list) %>% data.frame() %>% mutate(date = unique(dfgf$date)) %>% mutate(UR = UR$UR) %>% pivot_longer(!date,names_to = "variable",values_to = "value")
# 
# ggplot(SUR_ratio,aes(x=date,y=value,colour=variable)) + geom_line() + geom_vline(xintercept = as.Date("2008-09-01")) + geom_vline(xintercept = as.Date("2020-03-01")) + geom_vline(xintercept = as.Date("2005-06-01"),linetype=2) + geom_vline(xintercept = as.Date("2018-06-01"),linetype=2)
# 
# lfage[lfage$date == "2005-06-01",]
# lfage[lfage$date == "2018-06-01",]
# 
# SUR_ratio[SUR_ratio$date== "2005-06-01",]
# SUR_ratio[SUR_ratio$date== "2018-06-01",]




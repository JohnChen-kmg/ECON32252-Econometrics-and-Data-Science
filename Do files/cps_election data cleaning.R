# Prep work ----
setwd("/Users/jiaxiangchen/Desktop/Trial/2.1")
## Load Package
library(haven)
library(readr)
library(tidyverse)

## Import data
cps <- read_dta("CPS_Individual.dta")
elect <- read_csv("county vote share/countypres_2000-2020.csv")

## Links
# https://live.cps.datadownload.ipums.org/web/extracts/cps/2035916/cps_00007.xml


# Data Pocessing ----
## CPS ----
names(cps)
# foodstamp=stampval, hhincome = ftotval
drop <- c("serial", "month", "cpsid", "asecflag", "hflag", "asecwth",
          "footstamp","hhincome", "metarea", "pernum", "faminc", "schlcoll","offpovuniv",
          "relate", "famid")
cps1 <- cps[,!names(cps) %in% drop]
summary(cps1)
names(cps1)

# county FIPS code
options(scipen=100)
cps1$statefip <- formatC(cps1$statefip,flag=0,width=2)
table(cps1$statefip)
cps1$county <-sprintf("%05d",cps1$county)
cps1 <- cps1[!cps1$county=="00000",] # drop obs without county code
table(cps1$statefip) # check state
table(cps1$county)

# year
# cps1$year <- cps1$year-1 # the survey is for last calender year ??? 

#  metro
table(cps1$metro)
cps1$metro <- ifelse(cps1$metro < 2, 1,0) # 1 for non-metro area, 0 for metro
table(cps1$metro)

#  ownershp
table(cps1$ownershp)
cps1$metro <- ifelse(cps1$ownershp == 10, 1,0) # 1 for non-metro area, 0 for metro
table(cps1$ownershp)

# pubhous
table(cps1$pubhous)
cps1$pubhous <- ifelse(cps1$pubhous == 2, 1,0) # 1 for public housing
table(cps1$pubhous)

# rentsub
table(cps1$rentsub)
cps1$rentsub <- ifelse(cps1$rentsub == 2, 1,0) # 1 for rent subsidy
table(cps1$rentsub)

# heatsub
table(cps1$heatsub)
cps1$heatsub <- ifelse(cps1$heatsub == 2, 1,0) # 1 for heat subsidy
table(cps1$heatsub)

# heatval
summary(cps1$heatval)

# stampno
table(cps1$stampno)

# lunchsub
table(cps1$lunchsub)
cps1$lunchsub <- ifelse(cps1$heatsub == 1, 1,0) # 1 for children reciving free launch
table(cps1$lunchsub)

# stampval
summary(cps1$stampval)

# phone
table(cps1$phone)
cps1$phone <- ifelse(cps1$phone == 1, 1,0) # 1 for not having access to phine
table(cps1$phone)

# sex
table(cps1$sex) 
cps1$sex <- ifelse(cps1$sex==1,0,1) # 0 for male, 1 for female

# race
table(cps1$race)
cps1$race <- ifelse(cps1$race==100,0,cps1$race) # 0 for white
cps1$race <- ifelse(cps1$race==200,1,cps1$race) # 1 for black
cps1$race <- ifelse(cps1$race==651,2,cps1$race) # 2 for asian
cps1$race <- ifelse(cps1$race>2,3,cps1$race) # 3 for other
table(cps1$race)

# marst
table(cps1$marst)
cps1$marst <- ifelse(cps1$marst %in% c(1,2,3), 1, 0) # 0 for non-married
table(cps1$marst)

# popstat
table(cps1$popstat)
cps1$popstat <- ifelse(cps1$popstat==3, 1, 0) # 1 for child, 0 for children
table(cps1$popstat)

# famsize
table(cps1$famsize)

# empstat
table(cps1$empstat)
cps1$empstat <- ifelse(cps1$empstat %in% c(1,10,12), 0, 1) # 1 for unemployed 
table(cps1$empstat)

# labforce
table(cps1$labforce)
cps1$labforce <- ifelse(cps1$labforce %in% c(2), 0, 1) # 1 for not in labor force
table(cps1$labforce)

# educ
cps1$HS <- ifelse(cps1$educ >=73, 1,0) # 1 for highschool
cps1$UNI <- ifelse(cps1$educ >=111, 1,0) # 1 for uni level

# schlcoll, drop 

# diffany
table(cps1$diffany)
cps1$diffany <- ifelse(cps1$diffany %in% c(0,1), 0, 1) # 1 for disable
table(cps1$diffany)

# ftoval
summary(cps1$ftotval)

# inctot
summary(cps1$inctot)
cps1$inctot <- ifelse(cps1$inctot>999999997, NA, cps1$inctot)
summary(cps1$inctot)

# offpov
table(cps1$offpov) # note: NA detected,99
cps1 <- cps1[cps1$offpov < 99, ]
cps1$offpov <- ifelse(cps1$offpov==1, 1, 0) # 1 for above official poverty line
table(cps1$offpov)

# offpovuniv # drop, note: all zero disappear, Official Poverty Rate Universe

# offtotval 
# Total Family Income for Replicating Official Poverty Rates
# Treated primary family and also related family
summary(cps1$offtotval)
cps1$offtotval <- ifelse(cps1$offtotval>999999997, NA, cps1$offtotval)
summary(cps1$offtotval)

# foodstamp
# Family market value of food stamps; family level
summary(cps1$foodstamp)

# schllunch
summary(cps1$schllunch)
cps1$schllunch <- ifelse(cps1$schllunch>99998, NA, cps1$schllunch)
summary(cps1$schllunch)

# health
table(cps1$health)

# himcaidly
# have medicaid last year : 1~5
table(cps1$himcaidly) # Note: NA detected
cps1 <- cps1[cps1$himcaidly < 9,]

summary(cps1)
county_select <- unique(cps1$county)

## elect ----
summary(elect)
drop <- c("office","version","mode")
elect1 <- elect[,!names(elect) %in% drop]
elect1 <- elect[elect$year>=2012,]
elect1 <- elect[elect$county_fips %in% county_select,]
elect1$vote_share <- elect1$candidatevotes/elect1$totalvotes

# Compute the county level data
elect_temp <- elect1 %>% select(state, county_fips) %>%
        distinct() %>% arrange(county_fips)

# 2012 
elect_2012 <-  elect1 %>% select(year,state,county_fips, 
                                party, totalvotes, vote_share) %>% filter(year==2012) 
votes_2012 <- elect_2012 %>% select(county_fips, totalvotes) %>%
        distinct()%>% arrange(county_fips) %>% rename(tvotes_2012=totalvotes)
demo_2012 <- elect_2012 %>% filter(party=="DEMOCRAT") %>% 
        select(county_fips, vote_share) %>% 
        distinct()%>% arrange(county_fips) %>% rename(demo_2012=vote_share)
repu_2012 <- elect_2012 %>% filter(party=="REPUBLICAN") %>% 
        select(county_fips, vote_share) %>%
        distinct()%>% arrange(county_fips)%>% rename(repu_2012=vote_share)


# 2016
elect_2016 <-  elect1 %>% select(year,state, county_fips, 
                                 party, totalvotes, vote_share) %>% filter(year==2016) 
votes_2016 <- elect_2016 %>% select(county_fips, totalvotes) %>%
        distinct()%>% arrange(county_fips) %>%  rename(tvotes_2016=totalvotes)
demo_2016 <- elect_2016 %>% filter(party=="DEMOCRAT") %>% 
        select(county_fips, vote_share) %>%
        distinct()%>% arrange(county_fips) %>% rename(demo_2016=vote_share)
repu_2016 <- elect_2016 %>% filter(party=="REPUBLICAN") %>% 
        select(county_fips, vote_share) %>% 
        distinct()%>% arrange(county_fips) %>% rename(repu_2016=vote_share)


# 2020
elect_2020 <-  elect1 %>% select(year,state, county_fips, 
                                 party, totalvotes, vote_share) %>% filter(year==2020) 
votes_2020 <- elect_2020 %>% select(county_fips, totalvotes) %>% 
        distinct()%>% arrange(county_fips) %>% rename(tvotes_2020=totalvotes)
demo_2020 <- elect_2020 %>% filter(party=="DEMOCRAT") %>% 
        select(county_fips, vote_share) %>% 
        distinct() %>% arrange(county_fips) %>% 
        group_by(county_fips) %>%transmute(vote_share=sum(vote_share)) %>% 
        distinct() %>% arrange(county_fips) %>% rename(demo_2020=vote_share)
repu_2020 <- elect_2020 %>% filter(party=="REPUBLICAN") %>% 
        select(county_fips, vote_share) %>%
        distinct()%>% arrange(county_fips) %>% 
        group_by(county_fips) %>% transmute(vote_share=sum(vote_share)) %>% 
        distinct()%>% arrange(county_fips) %>% rename(repu_2020=vote_share)
# Note: 2020 have one county less
elect2 <- plyr::join_all(list(elect_temp,
              votes_2012, demo_2012, repu_2012,
              votes_2016, demo_2016, repu_2016,
              votes_2020, demo_2020, repu_2020), by='county_fips', type='inner')

# ACA particpation status ----
unique(cps1$statefip)
cps1$M2014 <- ifelse(cps1$statefip %in% c("04", # Arizona
                                          "05", #Arkansas
                                          "06", # California
                                          "08", # Colorado 
                                          "09", # Connecticut
                                          "10",# Delaware
                                          "11",# DC
                                          "15",# Hawaii
                                          "17",# Illinois
                                          "19",# Iowa
                                          "21",# Kentucky
                                          "24",# Maryland
                                          "25",# Massachusetts
                                          "26",# Michigan
                                          "27",# Minnesota
                                          "32",# Nevada
                                          "33",# New Hampshire
                                          "34",# New Jersey
                                          "35",# New Mexico 
                                          "36",# New York
                                          "38",# North Dakota
                                          "39",# Ohio
                                          "41",# Oregon
                                          "44",# Rhode Island
                                          "50",# Vermont
                                          "53",# Washington
                                          "54",# West Virginia
                                          "55"# Wisconsion
                                          ), 1, 0) # 1 for particpatation

cps1 <- rename(cps1, county_fips=county)
tr_state <- cps1 %>% select(county_fips, M2014) %>% distinct
data <- left_join(cps1, elect2, by=c("county_fips"))
elect2 <- left_join(elect2, tr_state, by="county_fips")

# Output ----
# Check NA and drop
data[is.na(data$tvotes_2012),] %>% group_by(county_fips) %>% summarize(n=n())
data <- data[!is.na(data$tvotes_2012),]

# Save the data in stata format
# whole data
write_dta(data, "/Users/jiaxiangchen/Desktop/Trial/2.1/data.dta")
# election data on county level
write_dta(elect2, "/Users/jiaxiangchen/Desktop/Trial/2.1/elect2.dta")




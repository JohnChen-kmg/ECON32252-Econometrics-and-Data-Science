# This file is for main analysis of DS
# Prep work
rm(list=ls())
setwd("/Users/jiaxiangchen/Desktop/Trial/2.1")
library(tidyverse)
library(haven)
library(readxl)
library(magrittr)
library(crosstable)
library(AER)
library(ggpubr)
library(reporttools)
library(gmodels)
library(stargazer)
library(Matching)
library(ebal)
library(xtable)

set.seed(1234)

data <- read_dta("data.dta")
elect2 <- read_dta("elect2.dta")
fps <- read_excel("FPS.xlsx")

# data$year <- data$year+1 ??? Year
# View(data)
summary(data)
names(data)
# Variable notes ----
# Key variables:
# 1.foodstmp: reciving foodstamop or not, anyone in the household
# 2.stampval: value of foodstamp, household

# 3.M2014: individual in the treated state
# 4.party2012/party2016: individual in demo county
# 5.ptr: after 2014 data, M2014:ptr indicate treated indivodual

# Data processing ----
data$foodstmp <- ifelse(data$foodstmp==1,0,1)
# Year dummy
data$year2010 <- ifelse(data$year==2010, 1, 0)
data$year2011 <- ifelse(data$year==2011, 1, 0)
data$year2012 <- ifelse(data$year==2012, 1, 0)
data$year2013 <- ifelse(data$year==2013, 1, 0)
data$year2014 <- ifelse(data$year==2014, 1, 0)
data$year2015 <- ifelse(data$year==2015, 1, 0)
data$year2016 <- ifelse(data$year==2016, 1, 0)
data$year2017 <- ifelse(data$year==2017, 1, 0)
data$year2018 <- ifelse(data$year==2018, 1, 0)
data$year2019 <- ifelse(data$year==2019, 1, 0)
data$year2020 <- ifelse(data$year==2020, 1, 0)
data$year2021 <- ifelse(data$year==2021, 1, 0)
data$year2022 <- ifelse(data$year==2022, 1, 0)
# Post treatment
data$ptr <- ifelse(data$year > 2014, 1,0) # after 2014 =1

# Dominant party: Democratics vs Republican, Demo=1 if greater
data$party2012 <- ifelse(data$demo_2012>=data$repu_2012, 1,0)
data$party2016 <- ifelse(data$demo_2016>=data$repu_2016, 1,0)
data$party2020 <- ifelse(data$demo_2020>=data$repu_2020, 1,0)

# Compute the 130 percent and 138 percent
names(fps)
fps <- rename(fps, year=Year)
data <- left_join(data, fps, by=c("year", "famsize"))

data$FPS_1 <- ifelse(data$offtotval >= data$FPS,0,1) # FPS
data$FPS_1.3 <- ifelse(data$offtotval >= data$FPS*1.3,0,1) # FPS 1.3
data$FPS_1.38 <- ifelse(data$offtotval >= data$FPS*1.38,0,1) # FPS 1.38
table(data$FPS_1, data$offpov) # approximate???
table(data$FPS_1.38, data$offpov)
# data <- left_join(data, fps, )

# Individual Control
names(data)
str(data)
data[sapply(data, is.character)]
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                       as.factor)


# Descriptive analysis ----
## Tables ----
## Graphs ----


# Part One: DiD ----
# Note: 2012 ~ 2016, all states

## 1. Pooled: ACA vs No ACA ----
# Baseline
data1 <- data[data$year>2011 & data$year<2017,]
reg1.1 <- lm(foodstmp ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip), data=data1)
summary(reg1.1)
reg1.2 <- lm(stampval ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip)+famsize-1, data=data1)
summary(reg1.2) 

#Baseline + IC
reg1.3 <- lm(foodstmp ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+
                     year2012+year2013+year2014+year2015+year2016+factor(statefip), data=data1)
summary(reg1.3)

reg1.4 <- lm(stampval ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+famsize+
                     year2012+year2013+year2014+year2015+year2016+factor(statefip)-1, data=data1)
summary(reg1.4)



## 2. FPS 130%: ACA vs No ACA ----
# Baseline
data2 <- data[data$year>2011 & data$year<2017 & data$FPS_1.3==1,]
reg2.1 <- lm(foodstmp ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip), data=data2)
summary(reg2.1)

reg2.2 <- lm(stampval ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip)+famsize-1, data=data2)
summary(reg2.2) 

#Baseline + IC
reg2.3 <- lm(foodstmp ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data2)
summary(reg2.3)
reg2.4 <- lm(stampval ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+famsize+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data2)
summary(reg2.4) 



## 3. FPS 138%: ACA vs No ACA ----
# Baseline
data3 <- data[data$year>2011 & data$year<2017 & data$FPS_1.38==1,]
reg3.1 <- lm(foodstmp ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip), data=data3)
summary(reg3.1)

reg3.2 <- lm(stampval ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip)+famsize-1, data=data3)
summary(reg3.2) 

#Baseline + IC
reg3.3 <- lm(foodstmp ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data3)
summary(reg3.3)
reg3.4 <- lm(stampval ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+famsize+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data3)
summary(reg3.4) 



## 4. 2012 Demo with ACA vs 2012 Demo without ACA, FPS138% ----
data4 <- data[data$year>2011 & data$year<2017 & data$FPS_1.38==1&data$party2012==1,]
reg4.1 <- lm(foodstmp ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip)-1, data=data4)
summary(reg4.1)

reg4.2 <- lm(stampval ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip)+famsize-1, data=data4)
summary(reg4.2) 

#Baseline + IC
reg4.3 <- lm(foodstmp ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data4)
summary(reg4.3)
reg4.4 <- lm(stampval ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+famsize+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data4)
summary(reg4.4) 



## 5. 2012 Repu with ACA vs 2012 Repu without ACA, FPS138% ----
data5 <- data[data$year>2011 & data$year<2017 & data$FPS_1.38==1&data$party2012==0,]
reg5.1 <- lm(foodstmp ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip)-1, data=data5)
summary(reg5.1)

reg5.2 <- lm(stampval ~ M2014:ptr+year2013+year2014+year2015+year2016+factor(statefip)+famsize-1, data=data5)
summary(reg5.2) 

#Baseline + IC
reg5.3 <- lm(foodstmp ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data5)
summary(reg5.3)
reg5.4 <- lm(stampval ~ M2014:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+famsize+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data5)
summary(reg5.4)  



## 6. 2012 Demo with ACA vs 2012 Repu with ACA, FPS138% ----
data6 <- data[data$year>2011 & data$year<2017 & data$FPS_1.38==1&data$M2014==1,]
reg6.1 <- lm(foodstmp ~ ptr:party2012+party2012+year2013+year2014+year2015+year2016+factor(statefip), data=data6)
summary(reg6.1)

reg6.2 <- lm(stampval ~ ptr:party2012+party2012+year2013+year2014+year2015+year2016+factor(statefip)+famsize-1, data=data6)
summary(reg6.2) 

#Baseline + IC
reg6.3 <- lm(foodstmp ~ ptr:party2012+party2012+factor(race)+HS+UNI+empstat+age+sex+metro+
                     year2013+year2014+year2015+year2016+factor(statefip), data=data6)
summary(reg6.3)
reg6.4 <- lm(stampval ~ ptr:party2012+party2012+factor(race)+HS+UNI+empstat+age+sex+metro+famsize+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data6)
summary(reg6.4)  



## 7. 2012 Demo without ACA vs 2012 Repu without ACA, FPS138% ----
data7 <- data[data$year>2011 & data$year<2017 & data$FPS_1.38==1&data$M2014==0,]
reg7.1 <- lm(foodstmp ~ ptr:party2012+party2012+year2013+year2014+year2015+year2016+factor(statefip), data=data7)
summary(reg7.1)

reg7.2 <- lm(stampval ~ ptr:party2012+party2012+year2013+year2014+year2015+year2016+factor(statefip)+famsize-1, data=data7)
summary(reg7.2) 

#Baseline + IC
reg7.3 <- lm(foodstmp ~ ptr:party2012+party2012+factor(race)+HS+UNI+empstat+age+sex+metro+
                     year2013+year2014+year2015+year2016+factor(statefip), data=data7)
summary(reg7.3)
reg7.4 <- lm(stampval ~ ptr:party2012+party2012+factor(race)+HS+UNI+empstat+age+sex+metro+famsize+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data7)
summary(reg7.4) 



## 8. 2012 Demo 2016 Demo with ACA vs 2012 Demo 2016 Repu with ACA, FPS138% ----
data8 <- data[data$year>2011 & data$year<2017 &
                      data$FPS_1.38==1&data$party2012==1 &
                      data$M2014==1,]
reg8.1 <- lm(foodstmp ~ party2016:ptr+year2013+year2014+year2015+year2016+factor(statefip), data=data8)
summary(reg8.1)

reg8.2 <- lm(stampval ~ party2016:ptr+year2013+year2014+year2015+year2016+factor(statefip)+famsize-1, data=data8)
summary(reg8.2) 

#Baseline + IC
reg8.3 <- lm(foodstmp ~ party2016:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+
                     year2013+year2014+year2015+year2016+factor(statefip), data=data8)
summary(reg8.3)
reg8.4 <- lm(stampval ~ party2016:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+famsize+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data8)
summary(reg8.4) 



## 9. 2012 Repu 2016 Demo with ACA vs 2012 Repu 2016 Repu with ACA, FPS138% ----
data9 <- data[data$year>2011 & data$year<2017 &
                      data$FPS_1.38==1&data$party2012==0 &
                      data$M2014==1,]
reg9.1 <- lm(foodstmp ~ party2016:ptr+year2013+year2014+year2015+year2016+factor(statefip), data=data9)
summary(reg9.1)

reg9.2 <- lm(stampval ~ party2016:ptr+year2013+year2014+year2015+year2016+factor(statefip)+famsize-1, data=data9)
summary(reg9.2) 

#Baseline + IC
reg9.3 <- lm(foodstmp ~ party2016:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+
                     year2013+year2014+year2015+year2016+factor(statefip), data=data9)
summary(reg9.3)
reg9.4 <- lm(stampval ~ party2016:ptr+factor(race)+HS+UNI+empstat+age+sex+metro+famsize+
                     year2013+year2014+year2015+year2016+factor(statefip)-1, data=data9)
summary(reg9.4)




# Part Two: Matching -----
## 10. FPS 138%: ACA vs No ACA ----
# 2013
data10 <- data[data$year==2013 & data$FPS_1.38==1,]
varc <- c("age", "sex", "race", "HS", "UNI", "empstat","famsize", "region")
mch1.1 <- Match(Y=data10$foodstmp,
              Tr=data10$M2014,
              X=data10[,varc],ties=F, estimand = "ATE")
summary(mch1.1)

mch1.2 <- Match(Y=data10$stampval,
                Tr=data10$M2014,
                X=data10[,varc],ties=F, estimand = "ATE")
summary(mch1.2)

# 2015
data10.1 <- data[data$year==2015 & data$FPS_1.38==1,]
mch1.3 <- Match(Y=data10.1$foodstmp,
                Tr=data10.1$M2014,
                X=data10.1[,varc],ties=F, estimand = "ATE")
summary(mch1.3)

mch1.4 <- Match(Y=data10.1$stampval,
                Tr=data10.1$M2014,
                X=data10.1[,varc],ties=F, estimand = "ATE")
summary(mch1.4)


## 11. 2012 Demo ACA vs 2012 Demo No ACA, FPS 138%: ----
# 2013
data11 <- data[data$year==2013 & data$FPS_1.38==1&data$party2012==1,]
varc <- c("age", "sex", "race", "HS", "UNI", "empstat","famsize", "region")
mch2.1 <- Match(Y=data11$foodstmp,
                Tr=data11$M2014,
                X=data11[,varc],ties=F, estimand = "ATE")
summary(mch2.1)

mch2.2 <- Match(Y=data11$stampval,
                Tr=data11$M2014,
                X=data11[,varc],ties=F, estimand = "ATE")
summary(mch2.2)

# 2015
data11.1 <- data[data$year==2015 & data$FPS_1.38==1&data$party2012==1,]
mch2.3 <- Match(Y=data11.1$foodstmp,
                Tr=data11.1$M2014,
                X=data11.1[,varc],ties=F, estimand = "ATE")
summary(mch2.3)

mch2.4 <- Match(Y=data11.1$stampval,
                Tr=data11.1$M2014,
                X=data11.1[,varc],ties=F, estimand = "ATE")
summary(mch2.4)


## 12. 2012 Repu ACA vs 2012 Repu No ACA, FPS 138%: ----
# 2013
data12 <- data[data$year==2013 & data$FPS_1.38==1&data$party2012==0,]
varc <- c("age", "sex", "race", "HS", "UNI", "empstat","famsize", "region")
mch3.1 <- Match(Y=data12$foodstmp,
                Tr=data12$M2014,
                X=data12[,varc],ties=F, estimand = "ATE")
summary(mch3.1)

mch3.2 <- Match(Y=data12$stampval,
                Tr=data12$M2014,
                X=data12[,varc],ties=F, estimand = "ATE")
summary(mch3.2)

# 2015
data12.1 <- data[data$year==2015 & data$FPS_1.38==1&data$party2012==0,]
mch3.3 <- Match(Y=data12.1$foodstmp,
                Tr=data12.1$M2014,
                X=data12.1[,varc],ties=F, estimand = "ATE")
summary(mch3.3)

mch3.4 <- Match(Y=data12.1$stampval,
                Tr=data12.1$M2014,
                X=data12.1[,varc],ties=F, estimand = "ATE")
summary(mch3.4)



## 13. 2012 Demo ACA vs 2012 Repu ACA, FPS 138%: ----
# 2013
data13 <- data[data$year==2013 & data$FPS_1.38==1&data$M2014==1,]
varc <- c("age", "sex", "race", "HS", "UNI", "empstat","famsize", "region")
mch4.1 <- Match(Y=data13$foodstmp,
                Tr=data13$party2012,
                X=data13[,varc],ties=F, estimand = "ATE")
summary(mch4.1)

mch4.2 <- Match(Y=data13$stampval,
                Tr=data13$party2012,
                X=data13[,varc],ties=F, estimand = "ATE")
summary(mch4.2)

# 2015
data13.1 <- data[data$year==2015 & data$FPS_1.38==1&data$M2014==1,]
mch4.3 <- Match(Y=data13.1$foodstmp,
                Tr=data13.1$party2012,
                X=data13.1[,varc],ties=F, estimand = "ATE")
summary(mch4.3)

mch4.4 <- Match(Y=data13.1$stampval,
                Tr=data13.1$party2012,
                X=data13.1[,varc],ties=F, estimand = "ATE")
summary(mch4.4)


## 14. 2012 Demo 2016 Demo with ACA vs 2012 Demo 2016 Repu with ACA, FPS138% ----
# No change
data14 <- data[data$year %in% c(2013, 2015) &data$FPS_1.38==1 & data$M2014==1 &
                       data$party2012==1 & data$party2016==1,]
varc <- c("age", "sex", "race", "HS", "UNI", "empstat","famsize", "region","offtotval")
mch5.1 <- Match(Y=data14$foodstmp,
                Tr=data14$year2015,
                X=data14[,varc],ties=F, estimand = "ATE")
summary(mch5.1)

mch5.2 <- Match(Y=data14$stampval,
                Tr=data14$year2015,
                X=data14[,varc],ties=F, estimand = "ATE")
summary(mch5.2)

# Change
data14.1 <-data14 <- data[data$year %in% c(2013,2015) &data$FPS_1.38==1 & data$M2014==1 &
                                  data$party2012==1& data$party2016==0,]
mch5.3 <- Match(Y=data14.1$foodstmp,
                Tr=data14.1$year2015,
                X=data14.1[,varc],ties=F, estimand = "ATE")
summary(mch5.3)

mch5.4 <- Match(Y=data14.1$stampval,
                Tr=data14.1$year2015,
                X=data14.1[,varc],ties=F, estimand = "ATE")
summary(mch5.4)



## 15. 2012 Repu 2016 Repu with ACA vs 2012 Repu 2016 Demo with ACA, FPS138% ----
# No change
data15 <- data[data$year %in% c(2013, 2015) &data$FPS_1.38==1 & data$M2014==1 &
                       data$party2012==0 & data$party2016==0,]
varc <- c("age", "sex", "race", "HS", "UNI", "empstat","famsize", "region","offtotval")
mch6.1 <- Match(Y=data15$foodstmp,
                Tr=data15$year2015,
                X=data15[,varc],ties=F, estimand = "ATE")
summary(mch5.1)

mch6.2 <- Match(Y=data15$stampval,
                Tr=data15$year2015,
                X=data15[,varc],ties=F, estimand = "ATE")
summary(mch6.2)

# Change
data15.1 <- data[data$year %in% c(2013,2015) &data$FPS_1.38==1 & data$M2014==1 &
                                  data$party2012==0& data$party2016==1,]
mch6.3 <- Match(Y=data15.1$foodstmp,
                Tr=data15.1$year2015,
                X=data15.1[,varc],ties=F, estimand = "ATE")
summary(mch6.3)

mch6.4 <- Match(Y=data15.1$stampval,
                Tr=data15.1$year2015,
                X=data15.1[,varc],ties=F, estimand = "ATE")
summary(mch6.4)




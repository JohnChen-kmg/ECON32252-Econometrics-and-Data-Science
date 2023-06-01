# This file for DS project
# Rub the main analysis file first
library(tidyverse)
library(stargazer)
library(reporttools)
library(usmap)
library(ggpubr)
library(reporttools)
library(gmodels)
library(stargazer)
library(Matching)
library(ebal)
library(xtable)

d1 <- data[data$year>2011 & data$year<2017 ,]
d2<- data[data$year>2011 & data$year<2017 & data$FPS_1.38==1,]
# Tables ----
# Summary statistics
# CPS
summary(d1$inctot)
d1.1 <- data %>% dplyr::select(stampval,age, famsize, offtotval) 
stargazer(as.data.frame(d1.1))


d1.2 <- data %>% dplyr::select(foodstmp,FPS_1,FPS_1.3,FPS_1.38,sex,race,HS, UNI, empstat, metro) 
d1.2 <- as.data.frame(d1.2)
d1.2 <- d1.2 %>% mutate(foodstmp=as.factor(foodstmp),
                        race=as.factor(race),
                        sex=as.factor(sex),
                        HS=as.factor(HS),
                        UNI=as.factor(UNI),
                        empstat=as.factor(empstat),
                        metro=as.factor(metro))
        

str(d1.2)
summary(d1.2$FPS_1.38)
tableNominal(d1.2, vertical = FALSE, longtable = FALSE)

# ACA
d2 <- data %>% dplyr::select(state,statefip,M2014 ) %>% distinct() %>% arrange(statefip)
xtable(d2, digits=0)

# Election
d3 <- data %>% dplyr::select(year,M2014,
                             demo_2012,repu_2012,
                             demo_2016,repu_2016) %>%
        group_by(year, M2014) %>% distinct() %>% 
        group_by(year,M2014) %>% summarize(demo_12=mean(demo_2012),
                                      repu_12=mean(repu_2012),
                                      demo_16=mean(demo_2016),
                                      repu_16=mean(repu_2016)) %>% distinct()
xtable(d3, digits=3)

# FPS 
head(fps)
d4 <- fps %>% filter(famsize==1) 
d4$add_1 <- c(4720,
              4540,
              4480,
              4420,
              4320,
              4180,
              4165,
              4160,
              4060,
              4020,
              3960)
d4 <- d4 %>% arrange(year) %>% dplyr::select(year, FPS, add_1)
xtable(d4, digits=0)

# Maps
# Selection
d5 <- data %>% dplyr::select(county_fips) %>% distinct %>% rename(fips=county_fips)
d5$values <- 1
d5$values <- as.factor(d5$values)
plot_usmap(data=d5) +
        ggplot2::scale_fill_manual(values = c("#00BA38"), na.value = "white",labels=c("Selected","Not Selected"),name="")


# 2012
d5.1 <- data %>% dplyr::select(county_fips, party2012) %>% distinct %>% rename(fips=county_fips,
                                                                               values=party2012)
d5.1$values <- as.factor(d5.1$values)
plot_usmap(data=d5.1) +
        ggplot2::scale_fill_manual(values = c("#F8766D","#619CFF"), na.value = "white",
                                   labels=c("2012 Republican dominant",
                                            "2012 Democrat dominant",
                                            "Not selected"),name="")

# 2016
d5.2 <- data %>% dplyr::select(county_fips, party2016) %>% distinct %>% rename(fips=county_fips,
                                                                               values=party2016)
d5.2$values <- as.factor(d5.2$values)
plot_usmap(data=d5.2) +
        ggplot2::scale_fill_manual(values = c("#F8766D","#619CFF"), na.value = "white",
                                   labels=c("2016 Republican dominant",
                                            "2016 Democrat dominant",
                                            "Not selected"),name="")

# 12 16 change
d5.3 <- data %>% dplyr::select(county_fips,party2012, party2016) %>% distinct %>% 
        filter(party2012!=party2016)%>% rename(fips=county_fips)
d5.3$values <- 1
d5.3$values <- as.factor(d5.3$values)
plot_usmap(data=d5.3) +
        ggplot2::scale_fill_manual(values = c("purple"), na.value = "white",
                                   labels=c("Partisian Change",
                                            "No Change/NAs"),name="")

# ACA
d5.4 <- data %>% dplyr::select(county_fips, M2014) %>% distinct %>% rename(fips=county_fips, values=M2014)
d5.4$values <- as.factor(d5.4$values)
plot_usmap(data=d5.4) +
        ggplot2::scale_fill_manual(values = c("darkorange","darkseagreen"), na.value = "white",
                                   labels=c("No ACA","ACA","Not selected"),name="")

# Other graphs
# Fstp pt rate
# Before
d6.1 <- data %>% filter(FPS_1.38==1, year==2014) %>% dplyr::select(county_fips,foodstmp,party2012, M2014) %>%
        group_by(county_fips,party2012, M2014) %>% summarize(p_r=mean(foodstmp)) 
d6.1$party2012 <- as.factor(d6.1$party2012)
d6.1$M2014 <- as.factor(d6.1$M2014)
ggplot(d6.1, aes(x=party2012, y=p_r, fill=party2012)) + 
        geom_boxplot() +
        facet_wrap(~M2014,labeller = as_labeller(c("0"='No ACA', "1"='ACA')))+
        scale_x_discrete(labels=c('Republican County', 'Democrat County'))+
        ylab("2013 Foodstamp Partiapation Rate (138%FPS)")+
        xlab("2012 Presidential Election Party Dominance")+ theme(legend.position = "none")
# After
d6.2 <- data %>% filter(FPS_1.38==1, year==2015) %>% dplyr::select(county_fips,foodstmp,party2012, M2014) %>%
        group_by(county_fips,party2012, M2014) %>% summarize(p_r=mean(foodstmp)) 
d6.2$party2012 <- as.factor(d6.2$party2012)
d6.2$M2014 <- as.factor(d6.2$M2014)
ggplot(d6.2, aes(x=party2012, y=p_r, fill=party2012)) + 
        geom_boxplot() +
        facet_wrap(~M2014,labeller = as_labeller(c("0"='No ACA', "1"='ACA')))+
        scale_x_discrete(labels=c('Republican County', 'Democrat County'))+
        ylab("2014 Foodstamp Partiapation Rate (138%FPS)")+
        xlab("2012 Presidential Election Party Dominance")+ theme(legend.position = "none")

# Fstp value
# Before
d6.3 <- data %>% filter(FPS_1.38==1, year==2014) %>% dplyr::select(county_fips,stampval,party2012, M2014) %>%
        group_by(county_fips,party2012, M2014) %>% summarize(value=mean(stampval)) 
d6.3$party2012 <- as.factor(d6.3$party2012)
d6.3$M2014 <- as.factor(d6.3$M2014)
ggplot(d6.3, aes(x=party2012, y=value, fill=party2012)) + 
        geom_boxplot() +
        facet_wrap(~M2014,labeller = as_labeller(c("0"='No ACA', "1"='ACA')))+
        scale_x_discrete(labels=c('Republican County', 'Democrat County'))+
        ylab("2013 Mean Foodstamp Value (138%FPS)")+
        xlab("2012 Presidential Election Party Dominance")+ theme(legend.position = "none")+
        ylim(0,4000)

# After
d6.4 <- data %>% filter(FPS_1.38==1, year==2015) %>% dplyr::select(county_fips,stampval,party2012, M2014) %>%
        group_by(county_fips,party2012, M2014) %>% summarize(value=mean(stampval)) 
d6.4$party2012 <- as.factor(d6.4$party2012)
d6.4$M2014 <- as.factor(d6.4$M2014)
ggplot(d6.4, aes(x=party2012, y=value, fill=party2012)) + 
        geom_boxplot() +
        facet_wrap(~M2014,labeller = as_labeller(c("0"='No ACA', "1"='ACA')))+
        scale_x_discrete(labels=c('Republican County', 'Democrat County'))+
        ylab("2014 Mean Foodstamp Value (138%FPS)")+
        xlab("2012 Presidential Election Party Dominance")+ theme(legend.position = "none")+
        ylim(0,4000)

# Regression output
stargazer(reg2.3, reg3.3, reg2.4,reg3.4)
stargazer(reg4.1, reg4.2, reg4.3,reg4.4)
stargazer(reg5.1, reg5.2, reg5.3,reg5.4)

# Mching balance table ----
# Matching balance table
bal.formula1 <- formula(foodstmp ~ age+sex+race+HS+UNI+empstat+famsize+region)
bal.formula2 <- formula(stampval ~ age+sex+race+HS+UNI+empstat+famsize+region)


# Demo 2013
# Matching balance table 2
# Print a balance table for unmatched
m1<- data[data$year==2013 & data$FPS_1.38==1&data$party2012==1,]
mb.unmatched2 = MatchBalance(bal.formula1, data = m1)
tab.unmatched2 = baltest.collect(mb.unmatched2, var.names = colnames(data.frame(m1[,varc])), after=F)
# results before matching
print(xtable(tab.unmatched2[, 1:8], label='tab:unmatched-bal:Demo ACA',
             caption='Covariate Balance in Unmatched Data(2013 Demo ACA)'),
      caption.placement='top')


# Print a balance table for matched
data11$M2014 <- as.factor(data11$M2014)
mb.matched2 = MatchBalance(bal.formula1, data = m1, match.out = mch2.2)
tab.matched2 = baltest.collect(mb.matched2, var.names = colnames(data.frame( m1[,varc])))
# results after matching
print(xtable(tab.matched2[, 1:8], label='tab:matched-bal:ACA',
             caption='Covariate Balance in Matched Data(2013 Demo ACA)'),
      caption.placement='top')

# Demo 2015
m1<- data[data$year==2015 & data$FPS_1.38==1&data$party2012==1,]
mb.unmatched2 = MatchBalance(bal.formula1, data = m1)
tab.unmatched2 = baltest.collect(mb.unmatched2, var.names = colnames(data.frame(m1[,varc])), after=F)
# results before matching
print(xtable(tab.unmatched2[, 1:8], label='tab:unmatched-bal:Demo ACA',
             caption='Covariate Balance in Unmatched Data(2015 Demo ACA)'),
      caption.placement='top')


# Print a balance table for matched
data11$M2014 <- as.factor(data11$M2014)
mb.matched2 = MatchBalance(bal.formula1, data = m1, match.out = mch2.4)
tab.matched2 = baltest.collect(mb.matched2, var.names = colnames(data.frame( m1[,varc])))
# results after matching
print(xtable(tab.matched2[, 1:8], label='tab:matched-bal:ACA',
             caption='Covariate Balance in Matched Data(2015 Demo ACA)'),
      caption.placement='top')


# Repu 2013
# Matching balance table 2
# Print a balance table for unmatched
m1<- data[data$year==2013 & data$FPS_1.38==1&data$party2012==0,]
mb.unmatched2 = MatchBalance(bal.formula1, data = m1)
tab.unmatched2 = baltest.collect(mb.unmatched2, var.names = colnames(data.frame(m1[,varc])), after=F)
# results before matching
print(xtable(tab.unmatched2[, 1:8], label='tab:unmatched-bal:Repu ACA',
             caption='Covariate Balance in Unmatched Data(2013 Repu ACA)'),
      caption.placement='top')


# Print a balance table for matched
data11$M2014 <- as.factor(data11$M2014)
mb.matched2 = MatchBalance(bal.formula1, data = m1, match.out = mch3.2)
tab.matched2 = baltest.collect(mb.matched2, var.names = colnames(data.frame( m1[,varc])))
# results after matching
print(xtable(tab.matched2[, 1:8], label='tab:matched-bal:ACA',
             caption='Covariate Balance in Matched Data(2013 Repu ACA)'),
      caption.placement='top')

# Repu 2015
m1<- data[data$year==2015 & data$FPS_1.38==1&data$party2012==0,]
mb.unmatched2 = MatchBalance(bal.formula1, data = m1)
tab.unmatched2 = baltest.collect(mb.unmatched2, var.names = colnames(data.frame(m1[,varc])), after=F)
# results before matching
print(xtable(tab.unmatched2[, 1:8], label='tab:unmatched-bal:Repu ACA',
             caption='Covariate Balance in Unmatched Data(2015 Repu ACA)'),
      caption.placement='top')


# Print a balance table for matched
data11$M2014 <- as.factor(data11$M2014)
mb.matched2 = MatchBalance(bal.formula1, data = m1, match.out = mch3.4)
tab.matched2 = baltest.collect(mb.matched2, var.names = colnames(data.frame( m1[,varc])))
# results after matching
print(xtable(tab.matched2[, 1:8], label='tab:matched-bal:ACA',
             caption='Covariate Balance in Matched Data(2015 Repu ACA)'),
      caption.placement='top')
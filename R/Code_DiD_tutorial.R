###########################################
# title: 'DiD Tutorial: Rothbard et al 2023'
# author: "R code by Eleanor J Murray"
# date: "2023-01-14"
#
# 
# This document provides the R code for Rothbard et al 2023, 
# A tutorial on applying the Difference-in-Difference method to health data. 
# For more information, refer to the publication at: 
# or the github repository at: github.com/eleanormurray/DiD_Tutorial
#   
#   Data dictionary:
#   Post_DBT:  0 = "Pre-DBT",  1 = "Post-DBT"
#   DBT_grp: 0 = "Control", 1 = "Intervention"
#   Time: -12 = "Pre-COVID", 0 = "Baseline", 1 = "Follow-up"
#   Strata: 11 = "Sealer, Very Challenging", 12 = "Sealer, Somewhat challenging", 13 = "Sealer, Not challenging",  21 = "Stapler, Very challenging",   22 = "Stapler, Somewhat challenging",  23 =  "Stapler, Not challenging", 31 =  "Other device, Very challenging", 32 = "Other device,Somewhat challenging", 33 = "Other device,Not challenging"
#   Post_COVID: 0 = "Pre-COVID", 1 = "Post-COVID"
###########################################


#For any package that you do not have installed, delete the hash and run the line below:
#install.packages('glm2')
#install.packages('margins')
#install.packages('tidyverse')
#install.packages('stats')
#install.packages('sandwich')
#install.packages('lmtest')
#install.packages('insight')
#install.packages('expss')
#install.packages('modelr')

library('tidyverse')
library('stats')
library('margins')
library('glm2')
library('forcats')
library('sandwich')
library('lmtest')
library('margins')
library('insight')
library('expss')
library('modelr')

###########################################
#Code Section 0: Data set up & formating
###########################################

#Read in data
did_sim <-read.csv(file = '<path>\\did_sim.csv', colClasses= c('factor', rep('numeric',46)))

did_sim = apply_labels(did_sim, 
                       post_DBT = "Exposure (A)",
                       post_DBT = num_lab(
                         "0 Baseline
                          1 Follow-up"
                       ),
                       DBT_grp = "Group (Z)",
                       DBT_grp = num_lab(
                         "0 Control 
                         1 Intervention"
                       ),
                       post_covid = "Baseline period",
                       post_covid = num_lab(
                         "0 Pre-COVID
                          1 Post-COVID " ),
                       time = num_lab("
                                      -12 Pre-COVID Baseline
                                      0 Post-COVID baseline
                                      1 Follow-up")
)

did_sim$post_DBT<-factor(did_sim$post_DBT)
is.factor(did_sim$post_DBT)
did_sim$DBT_grp<-factor(did_sim$DBT_grp)
is.factor(did_sim$DBT_grp)

# Summarize the variable 'tot_notechs'
summary(did_sim$tot_notechs)

# Table of Post-DBT surgeries:0 = "Pre-DBT",  1 = "Post-DBT"
did_sim %>%
  tab_cells(post_DBT)%>%
  tab_stat_cases()%>%
  tab_pivot()


# Table of DBT_group surgeries: 0 = "Control", 1 = "Intervention"
did_sim %>%
  tab_cells(DBT_grp)%>%
  tab_stat_cases()%>%
  tab_pivot()


###########################################
#Code Section 1: Checking Assumptions
###########################################
#Calculate average outcome value in control & intervention departments in each baseline time
did_sim %>%
  tab_cells(tot_notechs)%>%
  tab_rows(post_covid)%>%
  tab_cols(DBT_grp)%>%
  tab_stat_mean()%>%
  tab_pivot()


#Calculate the change in average value between baseline timepoints & control / intervention departments
did_baseline<-did_sim[did_sim$time !=1,]

parallel_test<-lm(tot_notechs ~ post_covid*DBT_grp, data = did_baseline)
summary(parallel_test)
coef(parallel_test)[4]
coefci(parallel_test, vcov.=vcovCL(parallel_test, cluster=~dept))[4,]

did_baseline$p_trend<-parallel_test$coefficients[4]

did_baseline%>%
  tab_cells(tot_notechs)%>%
  tab_cols(post_covid)%>%
  tab_rows(DBT_grp)%>%
  tab_stat_mean()%>%
  tab_pivot() %>% 
  tab_sort_desc()

did_baseline%>%
  tab_cells(p_trend)%>%
  tab_stat_mean()%>%
  tab_pivot



###########################################
#Code Section 2: Unadjusted Models
###########################################

#Code 2.1: Unadjusted linear regression, excluding pre-COVID data
did_trial<-did_sim[did_sim$time !=-12,]

lin_unadj<-lm(tot_notechs ~ post_DBT*DBT_grp,  data = did_trial)

summary(lin_unadj)
coeftest(lin_unadj, vcov.=vcovCL(lin_unadj, cluster = ~dept))
coef(lin_unadj)[4]
coefci(lin_unadj, vcov.=vcovCL(lin_unadj, cluster=~dept))[4,]


#Optional Code 2.1.2: Create table of predicted outcome values & DiD
#Note, that the margins command performs many different linear predictor tasks. 
#The code below is correct for standardization of our DiD estimates for this example 
#but may not be appropriate for other examples. 
#The code below also does not output the robust standard errors
#Use margins with caution!
m<-margins(lin_unadj, at=list(post_DBT=c('Baseline','Follow-up'), DBT_grp = c('Control','Intervention')), variables = c("post_DBT", "DBT_grp"))

m %>%  
  tab_cells(fitted)%>%
  tab_rows(DBT_grp)%>%
  tab_cols(post_DBT)%>%
  tab_stat_mean()%>%
  tab_pivot() %>% 
  tab_sort_desc()%>%
  mutate(difference = `post_DBT|Follow-up`-`post_DBT|Baseline`)%>%
  mutate(DiD = `difference`[1]-`difference`[2])


#Code 2.2: Unadjusted linear regression, using all data

lin_unadj2<-lm(tot_notechs ~ time+ post_DBT*DBT_grp,  data = did_sim)
summary(lin_unadj2)
coeftest(lin_unadj2, vcov. = vcovCL(lin_unadj2, cluster = ~dept))
coef(lin_unadj2)[5]
coefci(lin_unadj2, vcov. = vcovCL(lin_unadj2, cluster = ~dept))[5,]

#Optional Code 2.2.2: Create table of predicted outcome values & DiD
#Note that the addition of time to the model requires a modified margins command
m2<-margins(lin_unadj2, data = did_trial, at=list(post_DBT=c('Baseline','Follow-up'), DBT_grp = c('Control','Intervention')), variables = c("post_DBT", "DBT_grp"))

m2 %>%  
  tab_cells(fitted)%>%
  tab_rows(DBT_grp)%>%
  tab_cols(post_DBT)%>%
  tab_stat_mean()%>%
  tab_pivot() %>% 
  tab_sort_desc()%>%
  mutate(difference = `post_DBT|Follow-up`-`post_DBT|Baseline`)%>%
  mutate(DiD = `difference`[1]-`difference`[2])


#Code 2.3: Unadjusted Poisson model using all timepoints 
pois_unadj<-glm(tot_notechs~time + post_DBT*DBT_grp, family = poisson(), data = did_sim)
exp(cbind(coef(pois_unadj)))

#Code 2.3.2: Create table of predicted outcome values & DiD
#Note, we cannot get the DiD directly from the regression coefficents
#This code is required
m3<-margins(pois_unadj, data = did_trial, at=list(post_DBT=c('Baseline','Follow-up'), DBT_grp = c('Control','Intervention')), variables = c("post_DBT", "DBT_grp"))

m3 %>%  
  tab_cells(fitted)%>%
  tab_rows(DBT_grp)%>%
  tab_cols(post_DBT)%>%
  tab_stat_mean()%>%
  tab_pivot() %>% 
  tab_sort_desc()%>%
  mutate(difference = `post_DBT|Follow-up`-`post_DBT|Baseline`)%>%
  mutate(DiD = `difference`[1]-`difference`[2])


###########################################
#Code Section 3: Adjusted Linear Models
###########################################

# /*Code 3.1: adjusted linear regression model*/
# /*Conditional Difference-in-Difference, holding confounders constant*/
lin_adj<-lm(tot_notechs ~time + post_DBT*DBT_grp + case_challenge_1 + case_challenge_2 + device_1 + device_2 + case_challenge_1*device_1 + case_challenge_1*device_2 + case_challenge_2*device_1 + case_challenge_2*device_2, data= did_sim)
summary(lin_adj)
coef(lin_adj)[9]
coefci(lin_adj, vcov.=vcovCL(lin_adj, cluster=~dept))[9,]


#Optional Code 3.1.1: Output conditional DiD by strata
did_sim<- did_sim%>%
  mutate(strata = device*10+case_challenge)

did_sim = apply_labels(did_sim, 
                       strata = "case_complexity * device strata",
                       strata = num_lab(
                         "11 Sealer, Very Challenging
            12 Sealer, Somewhat challenging
            13 Sealer, Not challenging
            21 Stapler, Very challenging
            22 Stapler, Somewhat challenging   
            23 Stapler, Not challenging
            31 Other device, Very challenging
            32 Other device, Somewhat challenging
            33 Other device, Not challenging"
                       ))
did_sim$strata <-as.factor(did_sim$strata) 

lin_adj2<-lm(tot_notechs ~ time+ post_DBT*DBT_grp +strata,  data = did_sim)
m4<-did_sim[did_sim$time!=-12,]%>%
  mutate(AZ = (as.numeric(post_DBT)-1)*10+(as.numeric(DBT_grp)-1))

m4 = apply_labels(m4,
                  AZ ='Outcome group',
                  AZ = num_lab(
                    '0 Pre-Control
                             1 Pre-Intervention
                             10 Post-Control
                             11 Post-Intervention'
                  ))

m4%>% 
  add_predictions(lin_adj2)%>%
  tab_cells(pred)%>%
  tab_rows(strata)%>%
  tab_cols(AZ)%>%
  tab_stat_mean()%>%
  tab_pivot%>%
  mutate(difference_0 = `Outcome group|Post-Control` - `Outcome group|Pre-Control`)%>%
  mutate(difference_1 = `Outcome group|Post-Intervention` - `Outcome group|Pre-Intervention`)%>%
  mutate(DiD = difference_1 - difference_0)


#Code 3.2: Standardizing the estimated outcome to obtain a Marginal DiD estimator
#Option 1: the long way! Calculating the stratum-specific probabilities & standardizing by hand
#run 3.1.1 to create stratum-specific estimates, then run the code below to get strata distributions

m4 %>%
  tab_cells(strata)%>%
  tab_stat_cases(label = "N")%>%
  tab_stat_cpct(total_row_position="none", label = "col %")%>%
  tab_pivot(stat_position="inside_rows") 


#Code 3.3: Standardizing the estimated outcome to obtain a Marginal DiD estimator
#Option 2: the short way! Using R's built in margins command
#Note that we need to fix time and post_covid to match the stratum-assignment of the copy

m5<-margins(lin_adj2, data = m4, at=list(post_DBT=c('Baseline','Follow-up'), DBT_grp = c('Control','Intervention')), variables = c("post_DBT", "DBT_grp"))
m5 %>%  
  tab_cells(fitted)%>%
  tab_rows(DBT_grp)%>%
  tab_cols(post_DBT)%>%
  tab_stat_mean()%>%
  tab_pivot() %>% 
  tab_sort_desc()%>%
  mutate(difference = `post_DBT|Follow-up`-`post_DBT|Baseline`)%>%
  mutate(DiD = `difference`[1]-`difference`[2])



##############################################################################
#Code Section A3: Adjusted Poisson regression with standardization
##############################################################################                                                                                    

pois_adj<-glm(tot_notechs ~time + post_DBT*DBT_grp + case_challenge_1 + case_challenge_2 + device_1 + device_2 + case_challenge_1*device_1 + case_challenge_1*device_2 + case_challenge_2*device_1 + case_challenge_2*device_2, data= did_sim, family = poisson())

m6<-margins(pois_adj, data = did_trial, at=list(post_DBT=c('Baseline','Follow-up'), DBT_grp = c('Control','Intervention')), variables = c("post_DBT", "DBT_grp"))

m6 %>%  
  tab_cells(fitted)%>%
  tab_rows(DBT_grp)%>%
  tab_cols(post_DBT)%>%
  tab_stat_mean()%>%
  tab_pivot() %>% 
  tab_sort_desc()%>%
  mutate(difference = `post_DBT|Follow-up`-`post_DBT|Baseline`)%>%
  mutate(DiD = `difference`[1]-`difference`[2])

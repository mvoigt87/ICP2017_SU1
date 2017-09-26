### ------------------------------------------------------------------------------------------------- ###
###                     Descriptive Statisitics and individidual level analysis                       ###
### ------------------------------------------------------------------------------------------------- ###
#
 ##### 
### 1 Simple statistical tests for the event variable in combination with potential covariates

### 2 Descriptive tables for men and women
 
### 3 Survival analysis for individuals in the synthetic cohort
 
### 4 Plots and Tables
### ------------------------------------------------------------------------------------------------- ### 

### 0.1. Loading data set
  load("030_RetirementIND.RData")
 
### 0.2 load necessary package
 library(reshape)
 library(tidyverse)
 library(survival)
 library(forcats)
 library(data.table)
 
 
 ### ------------------------------------------------------------------------------------------------- ### 
 
 ### 1.1. Combination of the event 
 
 # 1.1.1 Event distribution by age and sex (plus statistical tests)
 
 # event distribution by sex - table
 s.e.tbl <- table(retire$exit,retire$sex)
 round(prop.table(s.e.tbl,2), digits = 2)
 chisq.test(retire$exit,retire$sex, simulate.p.value = FALSE, B = 20000)
 # X-squared = 4189.8, df = 1, p-value < 2.2e-16
 rm(s.e.tbl)

 #            female  male
 #   censored  0.90% 0.84%
 #   dead      0.10% 0.16%
 
 # bar plot events by sex
 retire %>% ggplot(aes(x=sex,fill=exit))+
   geom_bar(stat = "count")+
   scale_fill_discrete(name = "")
 
 summary(retire$age.exit[retire$exit=="dead"])
 summary(retire$age.exit[retire$exit=="censored"])
   #### age at death
   #   57.26    76.67    82.05   81.19    86.63   94.90         # higher and seems to be more compressed
   #### age at census
   #   56.58    68.42    73.28   74.57    80.02   94.99 
 summary(aov(retire$age.exit ~ retire$exit))
   #                 Df   Sum Sq Mean Sq F value Pr(>F)    
   # retire$exit      1  3511610 3511610   64663 <2e-16 ***
   # Residuals   666125 36174750      54   
 
 # histogram of age at exit distribution
 retire %>% ggplot(aes(x=age.exit,fill=exit))+
   geom_histogram(bins=40)+
   scale_fill_discrete(name = "")
 
 ### ------------------------------------------------------------------------------------------------- ###  
 
# 1.1.2 Event distribution by education, pension income and other variables of social position (statistical tests)
 
 ## Event by educational group
 
 ed.e.tbl <- table(retire$exit, retire$ESREAL)
 round(prop.table(ed.e.tbl,2),digits = 2)
 chisq.test(ed.e.tbl)
 rm(ed.e.tbl)
 
  #            Illiterate  Incomplete  Primary Educ.  Secondary Educ.  Tertiary Educ.         # column percentage
  #  censored       0.77%       0.82%          0.87%            0.91%           0.91%
  #  dead           0.23%       0.18%          0.13%            0.09%           0.09% 
  # 	Pearson's Chi-squared test
  #    data:  table(retire$exit, retire$ESREAL)
  #    X-squared = 10047, df = 4, p-value < 2.2e-16
  
  ### Since we look at a synthetic cohort, these results indicate a social mortality gradient
 
  # bar plot of event distribution by education
  retire %>% ggplot(aes(x=ESREAL,fill=exit)) +
   geom_bar(stat = "count") +
   scale_fill_discrete(name = "")
  # graphic representation of the proportions of events and education groups
  
  ## Event by pension income
  
  pen.e.tbl <- table(retire$exit, retire$pensize)
  round(prop.table(pen.e.tbl,2),digits = 2)
  chisq.test(pen.e.tbl)
  rm(pen.e.tbl)
  
  #             less than 500 Euro  500-999 Euro   1000-1999 Euro     more than 2000          # column percentage
  #   censored         0.83%            0.85%           0.88%               0.93%
  #   dead             0.17%            0.15%           0.12%               0.07%
  #   X-squared = 3405.5, df = 3, p-value < 2.2e-16
  
 
  ## Event by Car ownership
  
  car.e.tbl <- table(retire$exit, retire$car)
  round(prop.table(car.e.tbl,2),digits = 2)
  chisq.test(car.e.tbl)
  rm(car.e.tbl)
  ## same gradient visible
  
  ## Event by Room number and tenency status
  
  ro.e.tbl <- table(retire$exit, retire$room)
  round(prop.table(ro.e.tbl,2),digits = 2)
  chisq.test(ro.e.tbl)
  rm(ro.e.tbl)
  ## tendency that poeple who live in a place with more rooms have lower mortality
  
  retire %>% ggplot(aes(x=room, fill=exit)) +
    geom_bar(stat = "count") + facet_grid(. ~ HousReg)
  
  ## these variables seem not to contribute much
  
  
### ------------------------------------------------------------------------------------------------- ###  
  
  # 1.1.3 Event distribution by pension entry age and a one-two marginal variables (statistical tests)  
  
  pe.e.tbl <- table(retire$exit,retire$ret.age.c)
  round(prop.table(pe.e.tbl,2), digits = 2)
  chisq.test(pe.e.tbl)
  rm(pe.e.tbl)
  # more deaths in early and late retirement
  
  retire %>% ggplot(aes(x=ret.age.c,fill=exit)) +
    geom_bar(position = 'fill')
  ## relatively little differences when it is visually
  
### ------------------------------------------------------------------------------------------------- ### 
  

  ### 2.1 male-female tables
  
  # # For the continuous variables
  # vars1 <- retire[, c("ret.age", "ret.pen", "contrib.years")]
  # vars1 <- vars1 %>% mutate(contrib.years = as.numeric(contrib.years))
  # cap1 <- "Population characteristics by sex"
  # stats <- list("n", "min", "median","max","iqr")
  # tableContinuous(vars = vars1, stats = stats, print.pval = "anova", cap = cap1, longtable = FALSE)
  
  aggregate(retire$ret.age, by=list(retire$sex), FUN=mean)[2]
  
  ### Using data table
  
  dt <- data.table(retire)
  dt[,list(mean=mean(ret.age),median=median(ret.age),iqr=IQR(ret.age)),by=sex]
  #       sex     mean median  iqr
  # 1:   male 63.49202  64.97 3.94
  # 2: female 65.00630  65.01 0.16
  
  # age at death
  dt[,list(mean=mean(age.exit),median=median(age.exit),iqr=IQR(age.exit)),by=.(sex,exit)]  
  
  #       sex     exit     mean median   iqr
  #    male       dead 80.56963  81.37  9.89
  #    male   censored 74.37898  73.16 11.72
  #    female     dead 83.19718  84.42  9.53
  #    female censored 74.92416  73.54 11.41

  # male
  pen2.e.tbl <- table(retire$exit[retire$sex=="male"], retire$pensize[retire$sex=="male"])
  round(prop.table(pen2.e.tbl,2),digits = 2)
  chisq.test(pen2.e.tbl)
  rm(pen2.e.tbl)
  # female
  pen3.e.tbl <- table(retire$exit[retire$sex=="female"], retire$pensize[retire$sex=="female"])
  round(prop.table(pen3.e.tbl,2),digits = 2)
  chisq.test(pen3.e.tbl)
  rm(pen3.e.tbl)
  
  # Visualization who died by contribution years at what age
  retire %>% ggplot(aes(x=contrib.years,y=age.exit))+
    geom_point(aes(color=exit))+ facet_grid(.~ sex)
  
  ## For both sexes the deaths are concentrated in the higher ages (logic, with the exeception of a group of men
  ## dying before 60 - mine and farm workers?) and in the lower contribution years
  
  
### ------------------------------------------------------------------------------------------------- ###   
  
  ### 3 Survival analysis for individuals in the synthetic cohort

  # survival object
  
  # time=age at death

  
  s.o. <- Surv(time=retire$age,time2=retire$age.exit,event = retire$exit)
  
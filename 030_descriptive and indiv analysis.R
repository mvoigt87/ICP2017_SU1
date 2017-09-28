### ------------------------------------------------------------------------------------------------- ###
###                     Descriptive Statisitics and individidual level analysis                       ###
### ------------------------------------------------------------------------------------------------- ###
#
 ##### 
### 1 Simple statistical tests for the event variable in combination with potential covariates

### 2 Descriptive tables for men and women
 
### 3 Survival analysis for individuals in the synthetic cohort and plots
 
### 4 Output-Tables
### ------------------------------------------------------------------------------------------------- ### 

### 0.1. Loading data set
  load("030_RetirementIND.RData")
 
### 0.2 load necessary package
 library(reshape)
 library(tidyverse)
 library(survival)
 library(forcats)
 library(data.table)
 library(broom)
 library(stargazer)
 
 
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
    geom_point(aes(color=exit))+ facet_grid(.~ sex) +
    scale_color_discrete(name = "")
  
  ## For both sexes the deaths are concentrated in the higher ages (logic, with the exeception of a group of men
  ## dying before 60 - mine and farm workers?) and in the lower contribution years
  
  
  
### ------------------------------------------------------------------------------------------------- ###   
  
  ### 3 Survival analysis for individuals in the synthetic cohort

  # 3.1. Create the survival object
  
  # time=age at death

  SO <- Surv(time=retire$age,
             time2=retire$age.exit,
             event = retire$event)
  
  # 3.2. Kaplan Meier and Log Ranks for the main covariates
  ### --------------------- ###
  km1 <- survfit(SO~1)
  km1
  km1.b <- tidy(km1)
  km1.b %>% ggplot() +
    geom_step(mapping=aes(x=time, y=estimate)) +
    scale_y_continuous(name = "Survival Probability")                  +
    scale_x_continuous(name = "Age")
  
  # general non parametric survival curve
  
  ##   records     n.max  n.start   events   median  0.95LCL  0.95UCL 
  ##  641223.0  160208.0      0.0  85797.0     85.6     85.6     85.7 
  rm(km1,km1.b)
  
  ### --------------------- ###
  # 3.2.2 sex differences
  ### --------------------- ###
  km2 <- survfit(SO~sex, data = retire)
  km2
  km2.p <- tidy(km2) %>% mutate(strata = revalue(strata,c("sex=female"="female","sex=male"="male"))) %>%  ggplot() +
    geom_step(mapping = aes(x=time, y=estimate, color=strata))         +
    scale_y_continuous(name = "Survival Probability")                  +
    scale_x_continuous(name = "Age")                                   +
    scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
    theme_minimal()
    
  km2.p          # note: logrank test not possible for left truncated data
  rm(km2, km2.p)
  
  ### --------------------- ###
  # 3.2.3 Education
  ### --------------------- ###
  km3 <- survfit(SO~ESREAL, data=retire)
  km3.p <- tidy(km3) %>% mutate(strata = revalue(strata, c("ESREAL=Illiterate"="illiterate","ESREAL=Incomplete"="incomplete",
                                                           "ESREAL=Primary Educ."="Primary Educ.", "ESREAL=Secondary Educ."="Secondary Educ.",
                                                           "ESREAL=Tertiary Educ."="Tertiary Educ."))) %>% 
    ggplot() +
    geom_step(mapping = aes(x=time, y=estimate, color=strata)) +
    scale_y_continuous(name = "Survival Probability")          +
    scale_x_continuous(name = "Age")                           +
    scale_color_discrete(name="")                              +
    theme_minimal()
  
   rm(km3, km3.p)
  ## Differences between Tertiary education and illiterates visible, the others seem to overlap quite a lot 
  
  ### ------------------------------ ###
  # 3.2.4 Pension Size (categorical)
  ### ------------------------------ ###
   km4 <- survfit(SO~pensize, data=retire)
   km4.p <- tidy(km4) %>% mutate(strata = revalue(strata, c("pensize=less than 500 Euro"="less than 500 Euro",
                                                            "pensize=500-999 Euro"="500-999 Euro",
                                                            "pensize=1000-1999 Euro"="1000-1999 Euro", 
                                                            "pensize=more than 2000"="more than 2000"))) %>% 
     ggplot() +
     geom_step(mapping = aes(x=time, y=estimate, color=strata)) +
     scale_y_continuous(name = "Survival Probability")          +
     scale_x_continuous(name = "Age")                           +
     scale_color_discrete(name="")                              +
     theme_minimal()
   
   rm(km4, km4.p)
   ### that seems to be wrong at first sight, but the group of the 500 and less probably contains many women (assumed)

   ### --------------------------- ###
   # 3.2.5 sex/pensize - let´s see
   ### --------------------------- ###
   # less than 500 / male
   km4.a1 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="male" & pensize=="less than 500 Euro"), type="kaplan-meier", 
                     conf.type = "log-log")
   # less than 500 / male
   km4.b1 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="female" & pensize=="less than 500 Euro"), conf.type = "log-log")
   # 500-999 / male
   km4.a2 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="male" & pensize=="500-999 Euro"), conf.type = "log-log")
   # 500-999 / female
   km4.b2 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="female" & pensize=="500-999 Euro"), conf.type = "log-log")
   # 1000-1999 / male
   km4.a3 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="male" & pensize=="1000-1999 Euro"), conf.type = "log-log")
   # 1000-1999 / female
   km4.b3 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="female" & pensize=="1000-1999 Euro"), conf.type = "log-log")
   # more than 2000 / male
   km4.a4 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="male" & pensize=="more than 2000 Euro"), conf.type = "log-log")
   # more than 2000 / female
   km4.b4 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="female" & pensize=="more than 2000 Euro"), conf.type = "log-log")
   
  ## Help files for visual scan 
  KM_SEX.a1 <- broom::tidy(km4.a1) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize = "less than 500 Euro")
  KM_SEX.a2 <- broom::tidy(km4.a2) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize = "500-999 Euro")
  KM_SEX.a3 <- broom::tidy(km4.a3) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize = "1000-1999 Euro")
  KM_SEX.a4 <- broom::tidy(km4.a4) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize = "more than 2000 Euro")
  
  KM_SEX.b1 <- broom::tidy(km4.b1) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize = "less than 500 Euro")
  KM_SEX.b2 <- broom::tidy(km4.b2) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize = "500-999 Euro")
  KM_SEX.b3 <- broom::tidy(km4.b3) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize = "1000-1999 Euro")
  KM_SEX.b4 <- broom::tidy(km4.b4) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize = "more than 2000 Euro")
  
  ### Combine subset results to one data frame and plot
  
  KM_SEX <- dplyr::union(KM_SEX.a1,KM_SEX.a2) %>% dplyr::union(KM_SEX.a3)  %>% dplyr::union(KM_SEX.a4) %>% 
            dplyr::union(KM_SEX.b1) %>% dplyr::union(KM_SEX.b2) %>% dplyr::union(KM_SEX.b3) %>% dplyr::union(KM_SEX.b4)
  
  KM_SEX %>% ggplot() +
    geom_step(mapping = aes(x=time, y=estimate, linetype=sex, color=pensize)) +
    scale_y_continuous(name = "Survival Probability")          +
    scale_x_continuous(name = "Age")                           +
    scale_color_discrete(name="")                              +
    scale_linetype_discrete(name="")                           +
    theme_minimal()
  
  ### Very interesting survival curve differences between men and women - visually there is a survival advantage of
  ### better earning men while probably the low case number makes the curves indistiguishable for women
  
  # delete the help files
   rm(KM_SEX.a1,KM_SEX.a2,KM_SEX.a3,KM_SEX.a4, KM_SEX.b1, KM_SEX.b2, KM_SEX.b3, KM_SEX.b4, km4.a1,  km4.a2,
     km4.a3,  km4.a4, km4.b1,  km4.b2,  km4.b3,  km4.b4)
  
  
  ### -------------------------------------------------------- ###
  # 3.2.5 sex/education - see there non parametric differences
  ### -------------------------------------------------------- ###
     # Note - I don´t quite understand why the "positive" way to write the subset comand doesn´t work
   # high (secondary, tertiary) / male
   km5.a1 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="male" & ESREAL!="Primary Educ." & ESREAL!="Incomplete" & ESREAL!="Illiterate"),
                     type="kaplan-meier", conf.type = "log-log")
   # high (secondary, tertiary) / female
   km5.b1 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="female" & ESREAL!="Primary Educ." & ESREAL!="Incomplete" & ESREAL!="Illiterate"),
                    conf.type = "log-log")
   # low / male
   km5.a2 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="male" & ESREAL!="Secondary Educ." & ESREAL!="Tertiary Educ."), 
                     conf.type = "log-log")
   # low / female
   km5.b2 <- survfit(Surv(time=age,
                          time2=age.exit,
                          event = event) ~ 1, data=subset(retire,sex=="female" & ESREAL!="Secondary Educ." & ESREAL!="Tertiary Educ."), 
                     conf.type = "log-log")
   
  KM_ED.1 <- tidy(km5.a1) %>% dplyr::select(estimate,time) %>% mutate(edu = "high education") %>% mutate(sex="male")
  KM_ED.2 <- tidy(km5.b1) %>% dplyr::select(estimate,time) %>% mutate(edu = "high education") %>% mutate(sex="female")
  KM_ED.3 <- tidy(km5.a2) %>% dplyr::select(estimate,time) %>% mutate(edu = "low education") %>% mutate(sex="male")
  KM_ED.4 <- tidy(km5.b2) %>% dplyr::select(estimate,time) %>% mutate(edu = "low education") %>% mutate(sex="female")
  
  KM_EDU <- union(KM_ED.1,KM_ED.2) %>% union(KM_ED.3) %>% union(KM_ED.4) %>% ggplot() +
   geom_step(mapping = aes(x=time, y=estimate, color=sex, linetype=edu))  +
   scale_colour_manual(values = c("orange", "darkgrey"), name="")         +
   scale_y_continuous(name = "Survival Probability")          +
   scale_x_continuous(name = "Age")                           +
   scale_linetype_discrete(name="")                           +  
   theme_minimal()
  
  # delete the help files
  rm(KM_ED.1,KM_ED.2,KM_ED.3,KM_ED.4, km5.a1,  km5.a2, km5.b1,  km5.b2)
  

    
  ### ------------------------------------------------------------------------------------------------- ###   
  
  ### 3.2. Individual level survival regression analysis
  
  ## change the reference for some categorical variables
  retire <- within(retire, ECIVIL <- relevel(ECIVIL, ref = "married"))
  retire <- within(retire, HousReg <- relevel(HousReg, ref = "Own"))

  ## 3.2.1 Standard Cox Regression with only pension size and contribution and sex as main variables  
  cox.pen.1 <- coxph(SO ~ pensize, data = retire)
  summary(cox.pen.1)
  rm(cox.pen.1)
  
  ##                                coef exp(coef) se(coef)       z Pr(>|z|)    
  ## pensize1000-1999 Euro       0.05998   1.06182  0.00851   7.049  1.8e-12 ***
  ## pensizeless than 500 Euro  -0.30066   0.74033  0.01075 -27.978  < 2e-16 ***
  ## pensizemore than 2000 Euro -0.02901   0.97141  0.01736  -1.671   0.0947 . 
  ## ref: 500-999 Euro
  ## Likelihood ratio test= 1017  on 3 df,   p=0 - this indicates that pension size alone is not a strong predictor
  
  cox.pen.2 <- coxph(SO ~ pensize + sex + contrib.y.c, data = retire)
  summary(cox.pen.2)
  rm(cox.pen.2)
  
  ##                                    coef exp(coef)  se(coef)      z Pr(>|z|)    
  ## pensize1000-1999 Euro          0.006675  1.006697  0.008815  0.757   0.4489    
  ## pensizeless than 500 Euro      0.011254  1.011318  0.012413  0.907   0.3646    
  ## pensizemore than 2000 Euro    -0.108994  0.896736  0.017621 -6.185 6.19e-10 ***
  ## sexmale                        0.615451  1.850491  0.010852 56.713  < 2e-16 ***
  ## contrib.y.c26-40 years         0.056698  1.058336  0.011228  5.050 4.43e-07 ***
  ## contrib.y.cless than 15 years  0.097906  1.102859  0.016173  6.054 1.42e-09 ***
  ## contrib.y.cmore than 40 years -0.027897  0.972488  0.012623 -2.210   0.0271 *
  #### Likelihood ratio test= 5799  on 7 df,   p=0
  ##### !!!!!!!!!
  ##### This shows clearly why we need to stratify the model by sex (different life course trajectories)

  
  ### ------------------------------------------------------------------------------------------------- ###  
  
  ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
  ### some last changes
 
  ### And for the sake of a clean output I will collapse the education categories
  
  retire <- retire %>% mutate(EDU = factor(ifelse(ESREAL!="Secondary Educ." & ESREAL!="Tertiary Educ.",
                                                        "no or low education","high education")))
  
  retire <- within(retire, EDU <- relevel(EDU, ref = "no or low education"))
  
  ## collapse contrib.y
  retire <- retire %>% mutate(con.y = factor(ifelse(contrib.years<20, "less than 20 years",
                                                          ifelse(contrib.years<40, "20-40 years",
                                                                 "more than 40 years"))))
  ## pension size
  retire <- within(retire, pensize <- relevel(pensize, ref = "1000-1999 Euro"))
  
  ## And the household size variable
  retire <- retire %>% mutate(hh= factor(ifelse(NMIEM==2, "with partner only",ifelse(NMIEM<=7, "small household","large household"))))
  retire <- within(retire, hh <- relevel(hh, ref = "with partner only"))
  
  ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
  
  
  
  
  ## 3.2.2 Stratified models (= assumes different baselines)
  cox.all.1 <- coxph(SO ~ pensize + ESREAL + con.y + ret.age.c + FNAC + ECIVIL + HousReg + car + hh + strata(sex)
                     , data=retire)
  summary(cox.all.1)
  cox.zph(cox.all.1)
  
  # check the log-log-survival curves for sexes
  # km2 <- survfit(SO~sex, data = retire)
  # km2
  # km2.p <- tidy(km2) %>% mutate(strata = revalue(strata,c("sex=female"="female","sex=male"="male"))) %>%
  #   mutate(logkm = -ln(estimate)) %>% 
  #   ggplot() +
  #   geom_step(mapping = aes(x=time, y=loglogkm, color=strata))         +
  #   scale_y_continuous(name = "Survival Probability")                  +
  #   scale_x_continuous(name = "Age")                                   +
  #   scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
  #   theme_minimal()
  # 
  # km2.p          # note: logrank test not possible for left truncated data
  # rm(km2, km2.p)
  

  ## 3.2.3 Separate models for females and males 
  ## (Code based on Kleinbaum: http://rstudio-pubs-static.s3.amazonaws.com/5096_0880aaaf0df94f3b8533a1c024738246.html)
  
  ret.separate <- lapply(split(retire, retire$sex),
                         FUN = function(DF) {
                           
                           coxph(SO ~ pensize + ESREAL + con.y + ret.age.c + FNAC + ECIVIL + HousReg + car + hh, retire)
                         })
  ret.separate
  
  ## 3.2.4 Model including interaction effects
  ret.interaction.sex <- coxph(formula = SO ~ (pensize + ESREAL + contrib.y.c + ret.age.c + FNAC + ECIVIL + HousReg + car)*sex - sex + strata(sex),
                               data    = retire,
                               ties    = c("efron","breslow","exact")[1])
  ret.interaction.sex
  
  ### Compare the stratified model to the interaction model - (ANOvA)
  anova(ret.interaction.sex, cox.all.1)
  ###           loglik   Chisq Df P(>|Chi|)    
  ### Model1   -897934                        
  ### Model2   -897996  125.26 21 < 2.2e-16 ***
  ### This model is not statistically significantly different from the no interaction model at the 0.05 level, 
  ### thus, we conclude that the model without interaction is adequate.
  
  ### !!! The stratified model it is !!!
  
  ### ------------------------------------------------------------------------------------------------- ###
  
  ### Test the flexible parametric models ... is still in progress!!!! ... not finished yet!!!
  
  ## check distribution
  library(fitdistrplus)
  descdist(KM_SEX$estimate[KM_SEX$sex=="male"], discrete = FALSE)
  descdist(KM_SEX$estimate[KM_SEX$sex=="female"], discrete = FALSE)
  
  #### Flexible parametric models
  
  library(flexsurv)
  
    # ## ignores the strata function => separate analysis for men and women
    # flex.cox <- flexsurvreg(formula = SO ~ pensize + ESREAL + contrib.y.c + ret.age.c + 
    #                         FNAC + ECIVIL + HousReg + car + strata(sex), data = retire, dist="gompertz", cl = 0.95)
  
 ## male
  ret.m <- retire %>% filter(sex=="male")
  tbl_df(ret.m)
  str(ret.m)
  flex.cox.m <- flexsurvreg(formula = SO ~ pensize + ESREAL + contrib.y.c + ret.age.c + 
                            FNAC + ECIVIL + HousReg + car, data = ret.m, dist="gompertz", cl = 0.95)
 ## female
  ret.m <- retire %>% filter(sex=="female")
  flex.cox.f <- flexsurvreg(formula = SO ~ pensize + ESREAL + contrib.y.c + ret.age.c + 
                              FNAC + ECIVIL + HousReg + car, data = ret.f, dist="gompertz", cl = 0.95)
  
  ### ------------------------------------------------------------------------------------------------- ###   
  
  ### 4. Output Tables
  
  
  ## 4.1. Stratified Complete Model
  
  stargazer(cox.all.1, title="Full model",no.space=F, 
            ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
            covariate.labels=c("1000-1999  Eur/month","$<$ 500 Eur/month","$>$ 2000 Eur/month",
                               "no formal degree","Primary Ed.","Secondary Ed.",
                               "Tertiary Ed.","$<$ 20 y. contrib.", "$>$ 40 y. contrib.","in time ret.",
                               "late ret.", "birth year (cohort)","single",
                               "widowed", "divorced","other regime", "rent", "2 vehicles",
                               "$>$ 2 vehicles","no vehicles"), single.row=TRUE, apply.coef = exp)


  ## 4.1. Flexible Parametric Model 
  
  stargazer(flex.cox, title="Flexible paramtric model",no.space=F, 
            ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
            covariate.labels=c("500-999  Eur/month","$<$ 500 Eur/month","$>$ 2000 Eur/month",
                               "no formal degree","Primary Ed.","Secondary Ed.",
                               "Tertiary Ed.","26-40 y. contrib.","$<$ 15 y. contrib.",
                               "> 40 y. contrib.","in time ret.","late ret.", "birth year (cohort)","single",
                               "widowed", "divorced","rent","other regime", "2 vehicles",
                               "$>$ 2 vehicles","no vehicles"), single.row=TRUE)
 

 rm(cox.all.1)
  
  
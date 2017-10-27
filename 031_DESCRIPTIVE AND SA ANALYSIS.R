### ------------------------------------------------------------------------------------------------- ###
###                     Descriptive Statisitics and Survival analysis                       ###
### ------------------------------------------------------------------------------------------------- ###
#
 ##### 
### 1 Simple statistical tests for the event variable in combination with potential covariates

### 2 Descriptive tables for men and women
 
### 3 Survival analysis for individuals in the synthetic cohort and plots
 
### 4 Survival analysis for the married couples
 
### 5 Output-Tables
### ------------------------------------------------------------------------------------------------- ### 

### 0.1. Loading data set
  load("031_RETIND.RData")
  # ----------------------------- 
  # 637345 individuals
  # -----------------------------
 
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
 round(prop.table(table(retire$event,retire$SEXO),2), digits = 2)
 chisq.test(retire$event,retire$SEXO, simulate.p.value = FALSE, B = 20000)
 # X-squared = 3857.4, df = 1, p-value < 2.2e-16

 # -----------------------------
 #            female  male
 #   censored  0.87% 0.81%
 #   dead      0.13% 0.19%
 # -----------------------------
 
 summary(retire$exit.age[retire$event==1])
 summary(retire$exit.age[retire$event==0])
   
   # -----------------------------
   #### age at death
   #      Min. 1st Qu.  Median    Mean  3rd Qu.    Max.
   #     61.06   76.55   82.27   81.70    87.34   99.90       # new values for the "dead" population make more sense
                                                              # They are much higher on average.
   #### age at census
   #     61.01   69.56   74.21   75.32    80.66   99.99 
   # -----------------------------
 
 summary(aov(retire$exit.age ~ retire$event))
   # -----------------------------
   #                 Df   Sum Sq  Mean Sq  F value  Pr(>F)    
   # retire$exit       1  4798746 4798746   87426 <2e-16 ***
   # Residuals    831229 45625353      55                
   # -----------------------------

   # histogram of age at exit distribution
 retire %>% ggplot(aes(x=exit.age,fill=as.factor(event)))+
   geom_histogram(bins=40)+
   scale_fill_discrete(name = "")
   # -----------------------------
   #clear concentration of deaths at the older ages - bump around the age 83
 
 

### ------------------------------------------------------------------------------------------------- ###  
 
# 1.1.2 Event distribution by education, pension income and other variables of social position (statistical tests)
 
 ## Event by educational group
 
round(prop.table(table(retire$event, retire$ESREAL5),2),digits = 2)
chisq.test(table(retire$event, retire$ESREAL5))
  # ----------------------------- 
  #                  No or Incomplete Educ.  Tertiary Educ.  Secondary Educ.  Primary Educ.         # column percentage
  #   censored                        0.78%           0.89%            0.89%          0.84%
  #   dead                            0.22%           0.11%            0.11%          0.16%
  # 	Pearson's Chi-squared test
  #    data:  table(retire$exit, retire$ESREAL)
  #    X-squared = 9096.4, df = 3, p-value < 2.2e-16                                          ### has changed (less)
  
  ### Since we look at a synthetic cohort, these results indicate a social mortality gradient
  # ----------------------------- 
 
 
  # bar plot of event distribution by education
  retire %>% ggplot(aes(x=ESREAL5,fill=as.factor(event))) +
   geom_bar(stat = "count") +
   scale_fill_discrete(name = "")
  # graphic representation of the proportions of events and education groups
  
  ## Event by pension income
  
  round(prop.table(table(retire$event, retire$pensize),2),digits = 2)
  chisq.test(table(retire$exit, retire$pensize))
  
  # ----------------------------- 
  #             less than 650 Euro  650-999 Euro   1000-1999 Euro     more than 2000          # column percentage
  #   censored         0.80%            0.83%           0.85%               0.91%
  #   dead             0.20%            0.17%           0.15%               0.09%
  #   X-squared = 71789, df = 21501, p-value < 2.2e-16
  # ----------------------------- 
  
 

  ## Event by tenency status
  
  round(prop.table(table(retire$event, retire$HousReg),2),digits = 2)
  chisq.test(table(retire$event, retire$HousReg))
  # ----------------------------- 
  #        Other form    Own   Rent
  # Censor       0.82%  0.83%  0.80%
  # Dead         0.18%  0.17%  0.20%
  # -----------------------------   



### ------------------------------------------------------------------------------------------------- ###  
### ------------------------------------------------------------------------------------------------- ### 
  

##### 2.1 male-female tables
  
  # # For the continuous variables
  # vars1 <- retire[, c("ret.age", "ret.pen", "contrib.years")]
  # vars1 <- vars1 %>% mutate(contrib.years = as.numeric(contrib.years))
  # cap1 <- "Population characteristics by sex"
  # stats <- list("n", "min", "median","max","iqr")
  # tableContinuous(vars = vars1, stats = stats, print.pval = "anova", cap = cap1, longtable = FALSE)
  
  aggregate(retire$entry.age.r, by=list(retire$SEXO), FUN=mean)[2]
  
  ### Using data table
  
  dt <- data.table(retire)
  dt[,list(mean=mean(entry.age.r),median=median(entry.age.r),iqr=IQR(entry.age.r)),by=SEXO]
  # ----------------------------- 
  #       sex     mean median  iqr
  # 1:   male 72.11584  70.81 12.15
  # 2: female 72.79736  70.91 12.84
  # ----------------------------- 
  
  
  ## age at death
  ## -----------------------------  
  dt[,list(mean=mean(exit.age),median=median(exit.age),iqr=IQR(exit.age)),by=.(SEXO,event)]  
  # -----------------------------  
  #       sex     exit     mean median   iqr
  #    male       dead 81.17098  81.73 10.27
  #    male   censored 74.55478  73.30 11.55
  #    female     dead 84.62324  85.70 10.0
  #    female censored 74.75857  73.11 11.42
  # -----------------------------
  # The sex differences in the age at death in this population of 61+ year olds is interesting
  # -----------------------------
  
  ## male
  round(prop.table(table(retire$event[retire$SEXO=="male"], retire$pensize[retire$SEXO=="male"]),2),digits = 2)
  chisq.test(table(retire$event[retire$SEXO=="male"], retire$pensize[retire$SEXO=="male"]))

  
  ## female
  round(prop.table(table(retire$event[retire$SEXO=="female"], retire$pensize[retire$SEXO=="female"]),2),digits = 2)
  chisq.test(table(retire$event[retire$SEXO=="female"], retire$pensize[retire$SEXO=="female"]))
  # -----------------------------
  # Substantial differences in the response to the pension size
  # -----------------------------
  
  # Visualization who died by contribution years at what age
  retire %>% ggplot(aes(x=contrib.years_Retirement,y=exit.age))+
    geom_point(aes(color=as.factor(event)))+ facet_grid(.~ SEXO) +
    scale_color_discrete(name = "")
  # -----------------------------  
  ## For both sexes the deaths are concentrated in the higher ages and in the lower contribution years
  ## Graph is not as clear as before
  # -----------------------------  
  
  
### ------------------------------------------------------------------------------------------------- ###   
  

##### 3 Survival analysis for individuals in the synthetic cohort

  ## Entry time/age = entry.age.r
  
  ## Exit time/age = exit.age
  
  ## Event marker = event
  
### 3.1 Visual exploration through Kaplan Meier Estimators
  #     To account for left truncation, a cox ph approximation is used to estimate the KME
  
KM1 <- survfit(coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event)~1,data=retire),type="kaplan-meier")
  # -----------------------------    
  #  records    n.max  n.start   events   median  0.95LCL  0.95UCL 
  # 831231.0 192113.0   7903.0 142220.0     84.4     84.4     84.5
  # -----------------------------  
  
KM1
KM1b <- tidy(KM1) %>% 
    ggplot() +
    geom_step(mapping=aes(x=time, y=estimate)) +
    scale_y_continuous(name = "Survival Probability")                  +
    scale_x_continuous(name = "Age") +
    theme_bw()
# -----------------------------  
   # Looks about right for a population surival by age    
   rm(KM1,KM1b)
# -----------------------------


  
### --------------------- ###
### 3.1.2 sex differences ###
### --------------------- ###

  # male population
  km2.a1 <- survfit(coxph(Surv(time=entry.age.r,
                               time2=exit.age,
                               event=event)~1, data=subset(retire,SEXO=="male")), 
                    data=subset(retire,SEXO=="male"), type="kaplan-meier")
  
  # female population
  km2.b1 <- survfit(coxph(Surv(time=entry.age.r,
                               time2=exit.age,
                               event=event)~1, data=subset(retire,SEXO=="female")), 
                    data=subset(retire,SEXO=="female"), conf.type = "log-log")

  

  
  km2.p <- tidy(km2.a1) %>% dplyr::select(estimate,time) %>% mutate(sex="male") 
  km2.pb <- tidy(km2.b1)  %>% dplyr::select(estimate,time) %>% mutate(sex="female")
  
    km2.p <- dplyr::union(km2.p, km2.pb) %>% 
    ggplot() +
    geom_step(mapping = aes(x=time, y=estimate, color=sex))         +
    scale_y_continuous(name = "Survival Probability")                  +
    scale_x_continuous(name = "Age")                                   +
    scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
    theme_minimal()
    # -----------------------------  
    # Clear differences in the survival distribution between the sexes with advantages
    # for the female population (noise at the beginning does not appear)
    # -----------------------------      

rm(km2.p,km2.a1, km2.b1, km2.pb)
  
  
### --------------------- ###
### 3.2.3 Education       ###
### --------------------- ###
    # Illitates and incomplete
    km3.a1 <- survfit(coxph(Surv(time=entry.age.r,
                                 time2=exit.age,
                                 event=event)~1, data=subset(retire,ESREAL5=="No or Incomplete Educ.")), 
                      data=subset(retire,ESREAL5=="No or Incomplete Educ."), type="kaplan-meier")
    # Primary
    km3.a2 <- survfit(coxph(Surv(time=entry.age.r,
                                 time2=exit.age,
                                 event=event)~1, data=subset(retire,ESREAL5=="Primary Educ.")), 
                      data=subset(retire,ESREAL5=="Primary Educ."), type="kaplan-meier")
    # Secondary
    km3.a3 <- survfit(coxph(Surv(time=entry.age.r,
                                 time2=exit.age,
                                 event=event)~1, data=subset(retire,ESREAL5=="Secondary Educ.")), 
                      data=subset(retire,ESREAL5=="Secondary Educ."), type="kaplan-meier")
    # Tertiary
    km3.a4 <- survfit(coxph(Surv(time=entry.age.r,
                                 time2=exit.age,
                                 event=event)~1, data=subset(retire,ESREAL5=="Tertiary Educ.")), 
                      data=subset(retire,ESREAL5=="Tertiary Educ."), type="kaplan-meier")
    
    ## Help files for visual scan 
    KM3.1 <- broom::tidy(km3.a1) %>% dplyr::select(estimate,time) %>% mutate(education="No or incomplete")
    KM3.2 <- broom::tidy(km3.a2) %>% dplyr::select(estimate,time) %>% mutate(education="Primary")
    KM3.3 <- broom::tidy(km3.a3) %>% dplyr::select(estimate,time) %>% mutate(education="Secondary")
    KM3.4 <- broom::tidy(km3.a4) %>% dplyr::select(estimate,time) %>% mutate(education="Tertiary")
    
    KM3 <- dplyr::union(KM3.1,KM3.2) %>% dplyr::union(KM3.3) %>% dplyr::union(KM3.4)
    
    KM3 %>% ggplot() +
      geom_step(mapping = aes(x=time, y=estimate, color=education)) +
      scale_y_continuous(name = "Survival Probability")          +
      scale_x_continuous(name = "Age")                           +
      scale_color_discrete(name="")                              +
      scale_linetype_discrete(name="")                           +
      theme_minimal()

    # ----------------------------- 
    # Survival curves run almost perfectly parallel for most of the time + slight gradient (Tertiary on top)
    # ----------------------------- 
rm(KM3, KM3.1,KM3.2,KM3.3,KM3.4,km3.a1,km3.a2,km3.a3,km3.a4)
    
    
### ------------------------------------ ###
### 3.2.4 Sex/Pension Size (categorical) ###
### ------------------------------------ ###
   # less than 650 / male
   km4.a1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize=="less than 650 Euro")), 
                     data=subset(retire,SEXO=="male" & pensize=="less than 650 Euro"), type="kaplan-meier")
   # less than 650 / male
   km4.b1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize=="less than 650 Euro")), 
                     data=subset(retire,SEXO=="female" & pensize=="less than 650 Euro"), conf.type = "log-log")
   # 650-999 / male
   km4.a2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize=="650-999 Euro")), 
                     data=subset(retire,SEXO=="male" & pensize=="650-999 Euro"), conf.type = "log-log")
   # 650-999 / female
   km4.b2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize=="650-999 Euro")), 
                     data=subset(retire,SEXO=="female" & pensize=="650-999 Euro"), conf.type = "log-log")
   # 1000-1999 / male
   km4.a3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize=="1000-1999 Euro")), 
                     data=subset(retire,SEXO=="male" & pensize=="1000-1999 Euro"), conf.type = "log-log")
   # 1000-1999 / female
   km4.b3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize=="1000-1999 Euro")), 
                     data=subset(retire,SEXO=="female" & pensize=="1000-1999 Euro"), conf.type = "log-log")
   # more than 2000 / male
   km4.a4 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize=="more than 2000 Euro")),
                     data=subset(retire,SEXO=="male" & pensize=="more than 2000 Euro"), conf.type = "log-log")
   # more than 2000 / female
   km4.b4 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize=="more than 2000 Euro")),
                     data=subset(retire,SEXO=="female" & pensize=="more than 2000 Euro"), conf.type = "log-log")
   
  ## Help files for visual scan 
  KM_SEX.a1 <- broom::tidy(km4.a1) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize = "less than 650 Euro")
  KM_SEX.a2 <- broom::tidy(km4.a2) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize = "650-999 Euro")
  KM_SEX.a3 <- broom::tidy(km4.a3) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize = "1000-1999 Euro")
  KM_SEX.a4 <- broom::tidy(km4.a4) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize = "more than 2000 Euro")
  
  KM_SEX.b1 <- broom::tidy(km4.b1) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize = "less than 650 Euro")
  KM_SEX.b2 <- broom::tidy(km4.b2) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize = "650-999 Euro")
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
   
   ##--------------------------------------
   ## A few descriptive tests to see why the women with more than 2000 fluctutate so much
   
   table(retire$SEXO,retire$pensize)
  
   table(retire$event[retire$pensize=="more than 2000 Euro"],retire$SEXO[retire$pensize=="more than 2000 Euro"])
   # ----------------------------- 
   # almost as expected there are "only" 304 women in the group with more than 2000 Euro who experience the event
   # (compared to 3535 men)
   # ----------------------------- 


   
### ----------------------------------------------------------- ###
### 3.2.5 sex/education - see there non parametric differences  ###
### ----------------------------------------------------------- ###
   
### For later!
  

    
  ### ------------------------------------------------------------------------------------------------- ###  
  ### ------------------------------------------------------------------------------------------------- ###  
  ### ------------------------------------------------------------------------------------------------- ###  
  ### ------------------------------------------------------------------------------------------------- ###  
  
  
##### 3. Individual level survival regression analysis
  
  
     ## change the reference for some categorical variables
     retire <- within(retire, HousReg <- relevel(HousReg, ref = "Other form"))

   
## 3.3.1 Standard Cox Regression with only pension size and contribution and sex as main variables  
  cox.pen.1 <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event)~pensize, data = retire)
  

  summary(cox.pen.1)
  rm(cox.pen.1)
  
  ##                                coef  exp(coef)  se(coef)       z Pr(>|z|)    
  ##   pensize1000-1999 Euro     0.10035   1.10556  0.01758 5.709 1.13e-08 ***
  ##   pensize650-999 Euro       0.02850   1.02892  0.01732 1.646  0.09982 .  
  ##   pensizeless than 650 Euro 0.04563   1.04669  0.01703 2.679  0.00739 ** 
  ## ref: more than 2000 Euro
  ## Likelihood ratio test= 75.19  on 3 df,   p=3.331e-16
  
    #### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #####
    ### NEW RESULTS!!!
    #                               coef exp(coef) se(coef)     z Pr(>|z|)
    # pensize1000-1999 Euro     0.08400   1.08763  0.01491 5.635 1.75e-08 ***
    # pensize650-999 Euro       0.02886   1.02928  0.01457 1.981 0.047575 *
    # pensizeless than 650 Euro 0.05589   1.05748  0.01437 3.888 0.000101 ***

### ------------------------------------------------------------------------------------------------- ###  
### ------------------------------------------------------------------------------------------------- ###  
  
### 3.3.2 Two survival models for men and women
  # To account for the different age at death distribution as well as the different life course trajectories,
  # the hazards of dying for the two genders will estimated separately in two models
  
  ## male population
  cox.male.a <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize + ESREAL5 + FNAC + DIS + civil.status + mobil +
                           HousReg + hh,
                          data=subset(retire, SEXO=="male"))
  
  ## female population
  cox.female.b <- coxph(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ pensize + ESREAL5 + FNAC + DIS + civil.status + mobil +
                             HousReg + hh,
                          data=subset(retire, SEXO=="female"))
  
  summary(cox.male.a)
  cox.zph( cox.male.a)
  summary(cox.female.b)
  cox.zph( cox.female.b)
  
  # ----------------------------- 
  # The same pattern as in previous models show here
  # ----------------------------- 
  
  
  #### Testing if a stratified model or an interaction model do fit the data better #####
  
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## 3.3.4 Stratified Cox Model
  # As alternative to the separate models we apply a stratified model which allows to account for the different
  # baseline mortality of the two sexes - for the sake of visibility the other model is prefered
  
  cox.strat.1 <- coxph(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ pensize + ESREAL5 + FNAC + civil.status + mobil +
                            HousReg + hh + strata(SEXO)
                     , data=retire)
  summary(cox.strat.1)
  cox.zph(cox.strat.1)
  
  ## 3.2.3 Separate models for females and males 
  ## (Code based on Kleinbaum: http://rstudio-pubs-static.s3.amazonaws.com/5096_0880aaaf0df94f3b8533a1c024738246.html)
  
  ret.separate <- lapply(split(retire, retire$SEXO),
                         FUN = function(DF) {
                           coxph(Surv(time=entry.age.r,
                                           time2=exit.age,
                                           event=event) ~ pensize + ESREAL5 + FNAC + DIS + civil.status + mobil +
                                        HousReg + hh, retire)
                         })
  ret.separate
  
  ## 3.2.4 Model including interaction effects
  ret.interaction.sex <- coxph(formula = Surv(time=entry.age.r,
                                              time2=exit.age,
                                              event=event) ~ (pensize + ESREAL5 + FNAC + DIS + civil.status + mobil +
                                                                HousReg + hh)*SEXO - SEXO + strata(SEXO),
                               data    = retire,
                               ties    = c("efron","breslow","exact")[1])
  ret.interaction.sex
  
  ### Compare the stratified model to the interaction model - (ANOvA)
  anova(ret.interaction.sex, cox.strat.1)
  ###           loglik   Chisq Df P(>|Chi|)    
  ### Model1  -1509885                          
  ### Model2  -1511976 4183.1 15 < 2.2e-16 ***
  ### This model is not statistically significantly different from the no interaction model at the 0.05 level, 
  ### thus, we conclude that the model without interaction is adequate.
  
  ## !!! The stratified model it is !!! - to better show the differences two separate models were chosen
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ## 
  
  ### 3.4 Further model and assumption test
  
  library(survminer)
  
##--- male population 
  mm <- cox.zph(cox.male.a)
  ff <- cox.zph(cox.female.b)
  
  # Proportional Hazards Assumption - pensionsize variables
  ggcoxzph(mm,resid=T, se=T, var=c(1:3), caption = "Schoenfeld Residuals by time",
           ggtheme = theme_minimal(),font.main = 12)
   # assumption is only hardly met by the group who receives 1000-1999 Euro per month
  
  # Proportional Hazards Assumption - rest
  ggcoxzph(mm,resid=T, se=T, var=c(4:13), caption = "Schoenfeld Residuals by time",
           ggtheme = theme_minimal(),font.main = 12) 
   # assumption is not met for the secondary and primary education group and the partner variable
  
  # residual check
  ggcoxdiagnostics(cox.male.a,type = "schoenfeld")
  
##--- female population   
  # Proportional Hazards Assumption
  ggcoxzph(ff,resid=T, se=T, var = c(1:3), caption = "Schoenfeld Residuals by time",
           ggtheme = theme_minimal(),font.main = 12)
  
    # PHA is not met in the analysis of pension size for women
  
  ggcoxzph(ff,resid=T, se=T, var = c(4:13), caption = "Schoenfeld Residuals by time",
           ggtheme = theme_minimal(),font.main = 12)
    # PHA is met for secondary education, no car, and the civil statuses
  
  
  ### ------------------------------------------------------------------------------------------------- ###
  ### ------------------------------------------------------------------------------------------------- ###
  ### 4. Output Tables
  
  
  ## 4.1. Stratified Complete Model
  
  # stargazer(cox.all.1, title="Full model",no.space=F, 
  #           ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
  #           covariate.labels=c("1000-1999  Eur/month","$<$ 500 Eur/month","$>$ 2000 Eur/month",
  #                              "no formal degree","Primary Ed.","Secondary Ed.",
  #                              "Tertiary Ed.","$<$ 20 y. contrib.", "$>$ 40 y. contrib.","in time ret.",
  #                              "late ret.", "birth year (cohort)","single",
  #                              "widowed", "divorced","other regime", "rent", "2 vehicles",
  #                              "$>$ 2 vehicles","no vehicles"), single.row=TRUE, apply.coef = exp)


  
  ## 4.1. Separate Models
  
  stargazer(cox.male.a,cox.female.b, title="Cox PH Model",no.space=F, 
            ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
            covariate.labels=c("1000-1999  Eur/month","650-999 Eur/month","$<$ 650 Eur/month",
                               "Tertiary Ed.","Secondary Ed.","Primary Ed.","Birth year (cohort)", "Received Disability Pension",
                               "Not married","Widowed","No car avail." ,"Owns house/apt.",
                               "Rents house/apt.","Lives only with partner"),
            single.row=TRUE, apply.coef = exp)
  

  ## 4.2. Different form of display - Results in a Forest Plot - May be for the presentation
  
  # male population
  ggforest(cox.male.a, plot.title = "", ggtheme = theme_minimal(),xlab = "Hazard Ratio",refLabel = "reference")
  
  # female population
  ggforest(cox.female.b,plot.title = "",ggtheme = theme_minimal(), xlab = "Hazard Ratio")
  

 
 ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
 ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
 ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
  
  
 
 ### Test the flexible parametric models
 
 ## check distribution
 library(fitdistrplus)
 descdist(KM_SEX$estimate[KM_SEX$sex=="male"], discrete = FALSE)
 descdist(KM_SEX$estimate[KM_SEX$sex=="female"], discrete = FALSE)
 
 
 #### Flexible parametric models
 
 library(flexsurv)
 # library(grofit)
 
 ### The flexsurvreg function ignores the strata function => separate analysis for men and women
 
 ## f(t) - cumulative distribution of deaths over time
 
 # check distribution
 
 survfit.coxph(cox.male.a,newdata=)
 
 # x <- 1:30
 # y <- gompertz(x, 10, 2, 5)
 # plot(x,y)
 
 
 ## First model with only pension size
 flex.ph.1 <- flexsurvreg(formula =Surv(age, age.exit,event) ~ pensize, data = retire, dist="gompertz")
 
 
 ## separate model for the male population -  only pension size
 
 flex.m.pen <- flexsurvreg(formula = Surv(age, age.exit,event) ~ pensize, data = subset(retire, sex=="male"), 
                           dist="gompertz")
 
 
 ## full flexible parametric model for the male population
 
 flex.m <- flexsurvreg(formula = Surv(time=age,time2=age.exit, event=event) ~ pensize + EDU + con.y + ret.age.c + 
                         FNAC + ECIVIL + HousReg + car + hh, data = subset(retire, sex=="male"), 
                       dist="gompertz")
 
 ## Flexible parametric model for the female population
 flex.f.pen <- flexsurvreg(formula =Surv(age, age.exit,event) ~ pensize, data = subset(retire, sex=="female"), 
                           dist="gompertz")
 
 ### ------------------------------------------------------------------------------------------------- ###   
 
 ### clean up one more time
 rm(cox.all.a,cox.all.b,cox.male.a,flex.ph.1,km2.a1,km2.b1,km2.p,km2b, ret.interaction.sex, ret.separate,km2.pb,
    flex.m.pen)

 ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ## 
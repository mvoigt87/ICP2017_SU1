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
 # Use for a parametric model
 library(flexsurv)
 
 ### ------------------------------------------------------------------------------------------------- ### 
 
 ### 1.1. Combination of the event 
 
 # 1.1.1 Event distribution by age and sex (plus statistical tests)
 
 # event distribution by sex - table
 round(prop.table(table(retire$event,retire$SEXO),2), digits = 2)
 chisq.test(retire$event,retire$SEXO, simulate.p.value = FALSE, B = 20000)
 # X-squared = 3857.4, df = 1, p-value < 2.2e-16

 # -----------------------------
 #            female  male
 #   censored  0.86% 0.79%
 #   dead      0.14% 0.21%
 # -----------------------------
 
 summary(retire$exit.age[retire$event==1])
 summary(retire$exit.age[retire$event==0])
   
   # -----------------------------
   #### age at death
   #      Min. 1st Qu.  Median    Mean  3rd Qu.    Max.
   #     65.02   77.05   82.51   82.09   87.49    99.90       # new values for the "dead" population make more sense
                                                              # They are much higher on average.
   #### age at censorship
   #     65.01   71.25   75.77   76.73   81.68   99.99 
   # -----------------------------
 
 summary(aov(retire$exit.age ~ retire$event))
   # -----------------------------
   #                 Df   Sum Sq  Mean Sq  F value  Pr(>F)    
   # retire$exit       1  3229328 3229328   67016 <2e-16 ***
   # Residuals    734680 35402273      48                
   # -----------------------------

   # histogram of age at exit distribution
 retire %>% ggplot(aes(x=exit.age,fill=as.factor(event)))+
   geom_histogram(bins=40)+
   scale_fill_discrete(name = "")
   # -----------------------------
   #clear concentration of deaths at the older ages - bump around the age 83
 
 

### ------------------------------------------------------------------------------------------------------------ ###  
 
### 1.2 Event distribution by education, pension income and other variables of social position (statistical tests)
 
## 1.2.1 Event by educational group
 
round(prop.table(table(retire$event, retire$ESREAL5),2),digits = 2)
chisq.test(table(retire$event, retire$ESREAL5))
  # ----------------------------- 
  #                  No or Incomplete Educ.  Tertiary Educ.  Secondary Educ.  Primary Educ.         # column percentage
  #   censored                        0.77%           0.86%            0.87%          0.83%
  #   dead                            0.23%           0.14%            0.13%          0.17%
  # 	Pearson's Chi-squared test
  #    data:  table(retire$exit, retire$ESREAL)
  #    X-squared = 7945.5, df = 3, p-value < 2.2e-16                                          ### has changed (less)
  
  ### Since we look at a synthetic cohort, these results indicate a social mortality gradient
  ### Although the values between secondary and tertiary education are very similar

  # ----------------------------- 
 
 
  # bar plot of event distribution by education
  retire %>% ggplot(aes(x=ESREAL5,fill=as.factor(event))) +
   geom_bar(stat = "count") +
   scale_fill_discrete(name = "")
  # graphic representation of the proportions of events and education groups
  
## 1.2.2 Event by pension income

  # --------------------------------------- #
  # Visual test - Graph Income distribution

  DINTBL.sw <- aggregate(retire$INCOME,by=list(retire$SEXO),FUN=mean)

  # 1  female  651.8494 Euro
  # 2    male  974.1605 Euro

### IN GREY tones
  
 INC.DIS <- retire %>% dplyr::mutate(grp.mean = ifelse(SEXO=="female",651.85,974.16)) %>% 
    ggplot(aes(x=INCOME, color=SEXO)) +
    geom_histogram(aes(y=50*..density..),fill="white", alpha=0.5, position="dodge",binwidth = 50) +
    geom_vline(aes(xintercept=grp.mean, color=SEXO),
               linetype="dashed") +
    scale_color_manual(values=c("#000000", "#A9A9A9"), name=" ") +
    scale_x_continuous(name="Montly Public Pension Income (in €)", limits = c(0,3500)) +
    scale_y_continuous(name = "Relative Frequency") +
    theme_bw()
 
 INC.DIS <- INC.DIS + theme(legend.position = c(0.85, 0.85))
  
  # ---------------------------------------------------------- #
  # Visual test - Graph Income distribution without disability
  
  
  DINTBL.sd <- aggregate(retire$income_Retirement,by=list(retire$SEXO),FUN=mean)
  
  #   Group.1        x
  # 1  female 479.6380
  # 2    male 765.6855
  
  retire %>% dplyr::mutate(grp.mean = ifelse(SEXO=="female",479.64,765.69)) %>% 
    ggplot(aes(x=income_Retirement, color=SEXO)) +
    geom_histogram(fill="white", alpha=0.5, position="dodge",binwidth = 50) +
    geom_vline(aes(xintercept=grp.mean, color=SEXO),
               linetype="dashed") +
    scale_color_brewer(palette="Dark2", name=" ") +
    scale_x_continuous(name="Montly Public Pension Income (in €)", limits = c(1,3500)) +
    scale_y_continuous(name = " ") +
    theme_bw()
  
  
  # ---------------------------------------------------------- #
  # Visual test - Graph Income distribution death and alive
  
  # doesn´t mean much as there is no control for age
  
  DINTBL.ev <- aggregate(retire$INCOME,by=list(retire$event),FUN=mean)
  
  # alive 874.7381
  # dead 807.0715
  
  
  INC.E.DIS <- retire %>% dplyr::mutate(event = as.character(ifelse(event==0, "alive", "dead"))) %>% 
    dplyr::mutate(grp.mean = ifelse(event=="alive",874.73,807.07)) %>% 
    ggplot(aes(x=INCOME, color=event)) +
    geom_histogram(aes(y=50*..density..),fill="white", alpha=0.5, position="dodge",binwidth = 50) +
    geom_vline(aes(xintercept=grp.mean, color=event),
               linetype="dashed") +
    scale_color_manual(values=c("#000000", "#A9A9A9"), name=" ") +
    scale_x_continuous(name="Montly Public Pension Income (in €)", limits = c(0,3500)) +
    scale_y_continuous(name = "Relative Frequency") +
    theme_bw()
  
  INC.E.DIS <- INC.E.DIS + theme(legend.position = c(0.85, 0.85))
  
  # Well this is interesting as it almost completely overlaying
  
  # --------------------------------------- #
  ## Table event distribution by income with 4 categories
  
  round(prop.table(table(retire$event, retire$pensize),2),digits = 2)
  chisq.test(table(retire$exit, retire$pensize))
  
  # ----------------------------- 
  #             less than 650 Euro  650-999 Euro   1000-1999 Euro     more than 2000          # column percentage
  #   censored         0.80%            0.82%           0.82%               0.88%
  #   dead             0.20%            0.18%           0.18%               0.12%
  #   X-squared = 45996, df = 19677, p-value < 2.2e-16
  # ----------------------------- 

#### %%%%%%%%%%%%%%%%%%%%%% ####
## Event by pension income by 3 categories
  
  round(prop.table(table(retire$event, retire$pensize.3),2),digits = 2)
  chisq.test(table(retire$exit, retire$pensize.3))
  
  # ----------------------------- 
  #              more than 1500 Euro  1000-1499 Euro  less than 1000 Euro   # column percentage
  # censored                   0.86%           0.82%                0.80%
  # dead                       0.14%           0.18%                0.20%
  # X-squared = 26689, df = 13118, p-value < 2.2e-16
  # ----------------------------- 
  
  
#### %%%%%%%%%%%%%%%%%%%%%% #### 
  
   # average income in numbers for the descriptive tables
  
  DINTBL <- aggregate(retire$INCOME,by=list(retire$SEXO),FUN=mean)
  
  #            x
  # 1  female  651.8494  Euro                                        ## new values
  # 2    male  974.1605  Euro


  
  
## 1.2.4 Event by tenency status
  
  round(prop.table(table(retire$event, retire$HousReg),2),digits = 2)
  chisq.test(table(retire$event, retire$HousReg))
  # ----------------------------- 
  #            Not owned   owned
  # censored       0.79%   0.81%
  # dead           0.21%   0.19%
  # X-squared = 264.61, df = 1, p-value < 2.2e-16
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
  # 1:   male 73.59104  72.75 11.18
  # 2: female 73.56216  72.01 12.44
  # ----------------------------- 
  
  
  ## age at death
  ## -----------------------------  
  dt[,list(mean=mean(exit.age),median=median(exit.age),iqr=IQR(exit.age)),by=.(SEXO,event)]  
  # -----------------------------  
  #       sex     exit     mean median   iqr
  #    male       dead 81.22292  81.55 10.18
  #    male   censored 76.82105  75.89 10.06
  #    female     dead 84.47997  85.33  9.86
  #    female censored 76.57104  75.48 11.06
  # -----------------------------
  # The sex differences in the age at death in this population of 65+ year olds is interesting
  # -----------------------------
  
  ## male
  round(prop.table(table(retire$event[retire$SEXO=="male"], retire$pensize[retire$SEXO=="male"]),2),digits = 2)
  chisq.test(table(retire$event[retire$SEXO=="male"], retire$pensize[retire$SEXO=="male"]))

  
  ## female
  round(prop.table(table(retire$event[retire$SEXO=="female"], retire$pensize[retire$SEXO=="female"]),2),digits = 2)
  chisq.test(table(retire$event[retire$SEXO=="female"], retire$pensize[retire$SEXO=="female"]))
  
  ## 3 categories

  ## male
  round(prop.table(table(retire$event[retire$SEXO=="male"], retire$pensize.3[retire$SEXO=="male"]),2),digits = 2)
  chisq.test(table(retire$event[retire$SEXO=="male"], retire$pensize.3[retire$SEXO=="male"]))
  ## female
  round(prop.table(table(retire$event[retire$SEXO=="female"], retire$pensize.3[retire$SEXO=="female"]),2),digits = 2)
  chisq.test(table(retire$event[retire$SEXO=="female"], retire$pensize.3[retire$SEXO=="female"]))
  
  
  
  # -----------------------------
  # Substantial differences in the response to the pension size
  # -----------------------------
  
  # Visualization who died by contribution years at what age
    # retire %>% ggplot(aes(x=contrib.years_Retirement,y=exit.age))+
    #   geom_point(aes(color=as.factor(event)))+ facet_grid(.~ SEXO) +
    #   scale_color_discrete(name = "")
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
  # 734682.0 181518.0  32380.0 138780.0     84.8     84.7     84.8
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
  
    
    
### ------------------------------- ###
### 3.1.3 differences in disability ###
### ------------------------------- ###    
  
    # Without Disability
    km3.a <- survfit(coxph(Surv(time= entry.age.r,
                          time2 = exit.age,
                          event = event)~1, data = subset(retire, DIS==0)),
                     data=subset(retire,DIS==0), type="kaplan-meier")
    
    # With Disability
    km3.b <- survfit(coxph(Surv(time= entry.age.r,
                                time2 = exit.age,
                                event = event)~1, data = subset(retire, DIS==1)),
                     data=subset(retire,DIS==1), type="kaplan-meier")
    
    # Visualization
    km3.wd <- tidy(km3.a) %>% dplyr::select(estimate,time) %>% dplyr::mutate(Dis="no disability")
    km3.d <- tidy(km3.b) %>% dplyr::select(estimate,time) %>% dplyr::mutate(Dis="with disability")
    
    km3.pl <- dplyr::union(km3.wd,km3.d) %>% 
      ggplot() +
      geom_step(mapping=aes(x=time,y=estimate,color=Dis)) +
      scale_y_continuous(name = "Survival Probability")  +
      scale_x_continuous(name = "Age") +
      scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
      theme_minimal()
    
    ## as expected the disabled individuals die off faster - differences are not as pronounced as the sex differences
    
    ##     records    n.max  n.start   events   median  0.95LCL  0.95UCL
    ##  No dis -------------------------------------------------------
    ##    550581.0 132427.0  31993.0  89951.0     85.6     85.5     85.7
    ##  Dis -------------------------------------------------------    
    ##    184101.0  49223.0   1114.0  48829.0     82.2     82.1     82.3 
    
    rm(km3.a, km3.b, km3.wd, km3.d, km3.pl)
    
### --------------------- ###
### 3.2.4 Education       ###
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
### 3.2.5 Sex/Pension Size (categorical) ###
### ------------------------------------ ###
   # less than 650 / male
   km4.a1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize=="less than 650 Euro")), 
                     data=subset(retire,SEXO=="male" & pensize=="less than 650 Euro"), type="kaplan-meier")
   # less than 650 / female
   km4.b1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize=="less than 650 Euro")), 
                     data=subset(retire,SEXO=="female" & pensize=="less than 650 Euro"), type="kaplan-meier")
   # 650-999 / male
   km4.a2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize=="650-999 Euro")), 
                     data=subset(retire,SEXO=="male" & pensize=="650-999 Euro"), type="kaplan-meier")
   # 650-999 / female
   km4.b2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize=="650-999 Euro")), 
                     data=subset(retire,SEXO=="female" & pensize=="650-999 Euro"), type="kaplan-meier")
   # 1000-1999 / male
   km4.a3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize=="1000-1999 Euro")), 
                     data=subset(retire,SEXO=="male" & pensize=="1000-1999 Euro"), type="kaplan-meier")
   # 1000-1999 / female
   km4.b3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize=="1000-1999 Euro")), 
                     data=subset(retire,SEXO=="female" & pensize=="1000-1999 Euro"), type="kaplan-meier")
   # more than 2000 / male
   km4.a4 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize=="more than 2000 Euro")),
                     data=subset(retire,SEXO=="male" & pensize=="more than 2000 Euro"), type="kaplan-meier")
   # more than 2000 / female
   km4.b4 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize=="more than 2000 Euro")),
                     data=subset(retire,SEXO=="female" & pensize=="more than 2000 Euro"), type="kaplan-meier")
   
  ## Help files for visual scan 
  KM_SEX.a1 <- broom::tidy(km4.a1) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize.sw = "less than 650 Euro")
  KM_SEX.a2 <- broom::tidy(km4.a2) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize.sw = "650-999 Euro")
  KM_SEX.a3 <- broom::tidy(km4.a3) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize.sw = "1000-1999 Euro")
  KM_SEX.a4 <- broom::tidy(km4.a4) %>% dplyr::select(estimate,time) %>% mutate(sex="male") %>% mutate(pensize.sw = "more than 2000 Euro")
  
  KM_SEX.b1 <- broom::tidy(km4.b1) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize.sw = "less than 650 Euro")
  KM_SEX.b2 <- broom::tidy(km4.b2) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize.sw = "650-999 Euro")
  KM_SEX.b3 <- broom::tidy(km4.b3) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize.sw = "1000-1999 Euro")
  KM_SEX.b4 <- broom::tidy(km4.b4) %>% dplyr::select(estimate,time) %>% mutate(sex="female") %>% mutate(pensize.sw = "more than 2000 Euro")
  
  ### Combine subset results to one data frame and plot
  
  KM_SEX <- dplyr::union(KM_SEX.a1,KM_SEX.a2) %>% dplyr::union(KM_SEX.a3)  %>% dplyr::union(KM_SEX.a4) %>% 
            dplyr::union(KM_SEX.b1) %>% dplyr::union(KM_SEX.b2) %>% dplyr::union(KM_SEX.b3) %>% dplyr::union(KM_SEX.b4)
  
  KM_SEX %>% ggplot() +
    geom_step(mapping = aes(x=time, y=estimate, linetype=sex, color=pensize.sw)) +
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
   
   table(retire$SEXO,retire$pensize.sw)
  
   table(retire$event[retire$pensize.sw=="more than 2000 Euro"],retire$SEXO[retire$pensize.sw=="more than 2000 Euro"])
   # ----------------------------- 
   # almost as expected there are "only" 315 women in the group with more than 2000 Euro who experience the event
   # (compared to 4744 men)
   # ----------------------------- 

### --------------------------------------------- ###
### 3.2.4 Sex/Pension Size (categorical - 3 cats) ###
### --------------------------------------------- ###
   
   # less than 600 / male
   km5.a1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire, SEXO=="male" & pensize.3=="less than 600 Euro")), 
                     data=subset(retire,SEXO=="male" & pensize.3=="less than 600 Euro"), type="kaplan-meier")
   # less than 600 / female
   km5.b1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize.3=="less than 600 Euro")), 
                     data=subset(retire,SEXO=="female" & pensize.3=="less than 600 Euro"), type="kaplan-meier")
   
   
   
   # 600-1199 / male
   km5.a2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize.3=="600-1199 Euro")), 
                     data=subset(retire,SEXO=="male" & pensize.3=="600-1199 Euro"), type="kaplan-meier")
   # 1000-1199 / female
   km5.b2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize.3=="600-1199 Euro")), 
                     data=subset(retire,SEXO=="female" & pensize.3=="600-1199 Euro"), type="kaplan-meier")
   
   
   # 1200+ / male
   km5.a3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="male" & pensize.3=="more than 1200 Euro")), 
                     data=subset(retire,SEXO=="male" & pensize.3=="more than 1200 Euro"), type="kaplan-meier")
   # 1200+ / female
   km5.b3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,SEXO=="female" & pensize.3=="more than 1200 Euro")), 
                     data=subset(retire,SEXO=="female" & pensize.3=="more than 1200 Euro"), type="kaplan-meier")
   
   # extract the values for the visualization
   
   KM5A <- tidy(km5.a1) %>% dplyr::select(estimate, time) %>% dplyr::mutate(sex="male") %>% 
     dplyr::mutate(pensize="0 - 600 Euro")
   KM5B <- tidy(km5.b1) %>% dplyr::select(estimate, time) %>% dplyr::mutate(sex="female") %>% 
     dplyr::mutate(pensize="0 - 600 Euro")
   KM5C <- tidy(km5.a2) %>% dplyr::select(estimate, time) %>% dplyr::mutate(sex="male") %>% 
     dplyr::mutate(pensize="600-1199 Euro")
   KM5D <- tidy(km5.b2) %>% dplyr::select(estimate, time) %>% dplyr::mutate(sex="female") %>% 
     dplyr::mutate(pensize="600-1199 Euro")
   KM5E <- tidy(km5.a3) %>% dplyr::select(estimate, time) %>% dplyr::mutate(sex="male") %>% 
     dplyr::mutate(pensize="more than 1200 Euro")
   KM5F <- tidy(km5.b3) %>% dplyr::select(estimate, time) %>% dplyr::mutate(sex="female") %>% 
     dplyr::mutate(pensize="more than 1200 Euro")
   
   ## Combine the single KMEs to one graph
   
   KM_3CAT <- dplyr::union(KM5A,KM5B) %>% dplyr::union(KM5C) %>% dplyr::union(KM5D) %>% dplyr::union(KM5E) %>%
              dplyr::union(KM5F)
   # plot
   KM_3CAT %>% ggplot() +
     geom_step(mapping = aes(x=time, y=estimate, linetype=sex, color=pensize)) +
     scale_y_continuous(name = "Survival Probability")          +
     scale_x_continuous(name = "Age")                           +
     scale_color_brewer(palette="Dark2", name=" ")              +
     scale_linetype_discrete(name="")                           +
     theme_minimal()
    
   # plot 3 indiv. income categories (using facet grit instead of linetyp)
   
   KM_3CAT %>% ggplot() +
     geom_step(mapping = aes(x=time, y=estimate, color=pensize)) +
     facet_grid(.~sex)                                          +
     scale_y_continuous(name = "Survival Probability")          +
     scale_x_continuous(name = "Age")                           +
     scale_color_brewer(palette="Dark2", name=" ")              +
     theme_minimal()
   
    # Same visual result as for the 4 categories - relative proportional expected trajectory for men but not for women
   
   # clean
   rm(KM5A,KM5B,KM5C,KM5D,KM5E,KM5F,km5.a1,km5.a2,km5.a3,km5.b1,km5.b2,km5.b3)
   
### --------------------------------------------------------------- ###
### 3.2.6 Impact of disability - stratified KME (3 income groups)   ###
### --------------------------------------------------------------- ###

   # less than 600 / disabled / male
   km6.a1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire, DIS==1 & pensize.3=="less than 600 Euro" & SEXO=="male")), 
                     data=subset(retire,DIS==1 & pensize.3=="less than 600 Euro" & SEXO=="male"), type="kaplan-meier")
   
   # less than 600 / not disabled / male
   km6.b1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,DIS==0 & pensize.3=="less than 600 Euro"& SEXO=="male")), 
                     data=subset(retire,DIS==0 & pensize.3=="less than 600 Euro" & SEXO=="male"), type="kaplan-meier")
   
   # 600-1199 / disabled / male
   km6.a2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire, DIS==1 & pensize.3=="600-1199 Euro" & SEXO=="male")), 
                     data=subset(retire,DIS==1 & pensize.3=="600-1199 Euro" & SEXO=="male"), type="kaplan-meier")
   
   # 600-1199 / not disabled / male
   km6.b2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,DIS==0 & pensize.3=="600-1199 Euro" & SEXO=="male")), 
                     data=subset(retire,DIS==0 & pensize.3=="600-1199 Euro" & SEXO=="male"), type="kaplan-meier")
   
   # more than 1200 / disabled / male
   km6.a3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire, DIS==1 & pensize.3=="more than 1200 Euro" & SEXO=="male")), 
                     data=subset(retire,DIS==1 & pensize.3=="more than 1200 Euro" & SEXO=="male"), type="kaplan-meier")
   
   # more than 1200 / not disabled / male
   km6.b3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,DIS==0 & pensize.3=="more than 1200 Euro" & SEXO=="male")), 
                     data=subset(retire,DIS==0 & pensize.3=="more than 1200 Euro" & SEXO=="male"), type="kaplan-meier")
   
   
   ### Extract information for plotting
   
   KM6.A <- tidy(km6.a1) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
     dplyr::mutate(pensize="less than 600 Euro") %>% dplyr:: mutate (sex="male")
   KM6.B <- tidy(km6.b1) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
     dplyr::mutate(pensize="less than 600 Euro") %>% dplyr:: mutate (sex="male")
   KM6.C <- tidy(km6.a2) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
     dplyr::mutate(pensize="600-1199 Euro") %>% dplyr:: mutate (sex="male")
   KM6.D <- tidy(km6.b2) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
     dplyr::mutate(pensize="600-1199 Euro") %>% dplyr:: mutate (sex="male")
   KM6.E <- tidy(km6.a3) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
     dplyr::mutate(pensize="more than 1200 Euro") %>% dplyr:: mutate (sex="male")
   KM6.F <- tidy(km6.b3) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
     dplyr::mutate(pensize="more than 1200 Euro") %>% dplyr:: mutate (sex="male")
   
   KM_DIS.M <- dplyr::union(KM6.A,KM6.B) %>% dplyr::union(KM6.C) %>% dplyr::union(KM6.D) %>% dplyr::union(KM6.E) %>% 
              dplyr::union(KM6.F)
   
   # ------------------------------------------------------------------------------------------------- #
   ###### FEMALE
   # less than 1000 / disabled / male
   km6.c1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire, DIS==1 & pensize.3=="less than 600 Euro" & SEXO=="female")), 
                     data=subset(retire,DIS==1 & pensize.3=="less than 600 Euro" & SEXO=="female"), type="kaplan-meier")
   
   # less than 1000 / not disabled / male
   km6.d1 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,DIS==0 & pensize.3=="less than 600 Euro" & SEXO=="female")), 
                     data=subset(retire,DIS==0 & pensize.3=="less than 600 Euro" & SEXO=="female"), type="kaplan-meier")
   
   # 1000-1499 / disabled / male
   km6.c2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire, DIS==1 & pensize.3=="600-1199 Euro" & SEXO=="female")), 
                     data=subset(retire,DIS==1 & pensize.3=="600-1199 Euro" & SEXO=="female"), type="kaplan-meier")
   
   # 1000-1499 / not disabled / male
   km6.d2 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,DIS==0 & pensize.3=="600-1199 Euro" & SEXO=="female")), 
                     data=subset(retire,DIS==0 & pensize.3=="600-1199 Euro" & SEXO=="female"), type="kaplan-meier")
   
   # more than 1500 / disabled / male
   km6.c3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire, DIS==1 & pensize.3=="more than 1200 Euro" & SEXO=="female")), 
                     data=subset(retire,DIS==1 & pensize.3=="more than 1200 Euro" & SEXO=="female"), type="kaplan-meier")
   
   # more than 1500 / not disabled / male
   km6.d3 <- survfit(coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event)~1, data=subset(retire,DIS==0 & pensize.3=="more than 1200 Euro" & SEXO=="female")), 
                     data=subset(retire,DIS==0 & pensize.3=="more than 1200 Euro" & SEXO=="female"), type="kaplan-meier")
   
   
   ### Extract information for plotting
   
   KM7.A <- tidy(km6.c1) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
     dplyr::mutate(pensize="less than 600 Euro") %>% dplyr:: mutate (sex="female")
   KM7.B <- tidy(km6.d1) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
     dplyr::mutate(pensize="less than 600 Euro") %>% dplyr:: mutate (sex="female")
   KM7.C <- tidy(km6.c2) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
     dplyr::mutate(pensize="600-1199 Euro") %>% dplyr:: mutate (sex="female")
   KM7.D <- tidy(km6.d2) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
     dplyr::mutate(pensize="600-1199 Euro") %>% dplyr:: mutate (sex="female")
   KM7.E <- tidy(km6.c3) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
     dplyr::mutate(pensize="more than 1200 Euro") %>% dplyr:: mutate (sex="female")
   KM7.F <- tidy(km6.d3) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
     dplyr::mutate(pensize="more than 1200 Euro") %>% dplyr:: mutate (sex="female")
   
   KM_DIS.F <- dplyr::union(KM7.A,KM7.B) %>% dplyr::union(KM7.C) %>% dplyr::union(KM7.D) %>% dplyr::union(KM7.E) %>% 
     dplyr::union(KM7.F) 
   
   KMDIS <- dplyr::union(KM_DIS.M, KM_DIS.F)
   
   
   
   ############# 
   #### Plot ###
   #############
   
   KMDIS %>% 
     ggplot()  +
    geom_step(mapping = aes(x=time, y=estimate, linetype=DIS, color=pensize)) +
    facet_grid(.~ sex)                                           +
      scale_y_continuous(name = "Survival Probability")          +
      scale_x_continuous(name = "Age")                           +
      scale_color_brewer(palette="Dark2", name=" ")              +
      scale_linetype_discrete(name="")                           +
      theme_minimal()
     

  # clean up the mess
   
  rm(KM_DIS,KM6.A,KM6.B,KM6.C,KM6.D,KM6.E,KM6.F,km6.a1,km6.a2,km6.a3,km6.b1,km6.b2,km6.b3,
     KM7.A,KM7.B,KM7.C,KM7.D,KM7.E,KM7.F,km6.c1,km6.c2,km6.c3,km6.d1,km6.d2,km6.d3)
   
  
  ### ------------------------------------------------------------------------------------------------- ###  
  ### ------------------------------------------------------------------------------------------------- ###  
  ### ------------------------------------------------------------------------------------------------- ###  
  ### ------------------------------------------------------------------------------------------------- ###  
  
  
##### 3. Individual level survival regression analysis
  
  
     ## change the reference for some categorical variables
     retire$HousReg <- as.factor(as.character(retire$HousReg))
     retire <- within(retire, HousReg <- relevel(HousReg, ref = "owned"))
     retire <- within(retire, pensize <- relevel(pensize, ref = "more than 2000 Euro"))
     # retire <- within(retire, pensize <- relevel(pensize, ref = "650-999 Euro"))
     # retire <- within(retire, pensize.3 <- relevel(pensize.3, ref = "more than 1200 Euro"))
     retire <- within(retire, pensize.3 <- relevel(pensize.3, ref = "600-1199 Euro"))
     

  ### ------------------------------------------------------------------------------------------------- ### 
     
   
## 3.3.1 Standard Cox Regression with only pension size and contribution and sex as main variables  
     
  cox.pen.1 <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event)~pensize, data = retire)
  
  # 3 category variable
  cox.pen.2 <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event)~pensize.3, data = retire)
  
  
  summary(cox.pen.1)
  summary(cox.pen.2)
  
  # clean
  rm(cox.pen.1, cox.pen.2)
  
  ##                                coef  exp(coef)  se(coef)       z Pr(>|z|)    
  ## pensize.sw1000-1999 Euro      0.08926   1.09336  0.01542  5.790 7.03e-09 ***
  ## pensize.sw650-999 Euro        0.10916   1.11534  0.01519  7.185 6.70e-13 ***
  ## pensize.swless than 650 Euro -0.04509   0.95592  0.01473 -3.061  0.00221 ** 
  ## ref: more than 2000 Euro
  ## Likelihood ratio test=  691.6  on 3 df,   p=0
  
  
## 3.3.2 Standard Cox Regression with only pension size  - by sex  
  
  
  cox.pen.1.m <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event)~pensize, data = subset(retire,SEXO=="male"))
  
  # 3 category income
  cox.pen.2.m <- coxph(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event)~pensize.3, data = subset(retire,SEXO=="male"))
  
  
  summary(cox.pen.1.m)
  summary(cox.pen.2.m)

  cox.pen.1.f <- coxph(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event)~pensize, data = subset(retire,SEXO=="female"))
  
  # 3 category income
  cox.pen.2.f <- coxph(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event)~pensize.3, data = subset(retire,SEXO=="female"))
  
  summary(cox.pen.1.f)
  summary(cox.pen.2.f)  
  
  
  ### in Latex table
  
  # stargazer(cox.pen.2.m,cox.pen.2.f,
  #           title="Cox PH Model",no.space=F, 
  # ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Hazard Ratios"),
  # covariate.labels=c("1000-1499 Eur/month","$<$ 1000 Eur/month"), single.row=TRUE, apply.coef = exp)
  
  
  
  
### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
### ------------------------------------------------------------------------------------------------- ###  
### ------------------------------------------------------------------------------------------------- ###  
  
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## 3.3.4 Stratified Cox Model
  # As alternative to the separate models we apply a stratified model which allows to account for the different
  # baseline mortality of the two sexes - for the sake of visibility the other model is prefered
  
  cox.strat.1 <- coxph(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                         FNAC + DIS + civil.status + hh + strata(SEXO)
                       , data=retire)
  summary(cox.strat.1)
  # cox.zph(cox.strat.1)
  
  ## 3.2.4 Model including interaction effects
  ret.interaction.sex <- coxph(formula = Surv(time=entry.age.r,
                                              time2=exit.age,
                                              event=event) ~ (pensize.3 + ESREAL5 + mobil + HousReg + 
                                                                FNAC + DIS + civil.status + hh)*SEXO - SEXO + strata(SEXO),
                               data    = retire,
                               ties    = c("efron","breslow","exact")[1])
  ret.interaction.sex
  
  ### Compare the stratified model to the interaction model - (ANOvA)
  anova(ret.interaction.sex, cox.strat.1)
  ###           loglik   Chisq Df P(>|Chi|)    
  ### Model1  -1470072                           
  ### Model2  -1470159 172.96 12 < 2.2e-16 ***
  ### This model is not statistically significantly different from the no interaction model at the 0.05 level, 
  ### thus, we conclude that the model without interaction is adequate.
  
  ## !!! The stratified model it is !!! - to better show the differences two separate models were chosen
  
  
  ## 3.2.3 Separate models for females and males 
  ## (Code based on Kleinbaum: http://rstudio-pubs-static.s3.amazonaws.com/5096_0880aaaf0df94f3b8533a1c024738246.html)
  
  ret.separate <- lapply(split(retire, retire$SEXO),
                         FUN = function(DF) {
                           coxph(Surv(time=entry.age.r,
                                      time2=exit.age,
                                      event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                                   FNAC + DIS + civil.status + hh, retire)
                         })
  ret.separate
  
  
  
  
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  
  
  
### 3.3.2 Two survival models for men and women
  # To account for the different age at death distribution as well as the different life course trajectories,
  # the hazards of dying for the two genders will estimated separately in two models
  
  ## male population
  ## ---------------
  
  # Model with only income
  cox.male.a <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize,
                          data=subset(retire, SEXO=="male"))

  
  # Adding the other wealth variables
  cox.male.b <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize + ESREAL5 + mobil + HousReg,
                     data=subset(retire, SEXO=="male"))
  
  # Adding contextual variables
  cox.male.c <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                           FNAC + DIS + civil.status + hh,
                      data=subset(retire, SEXO=="male"))
  
  
  ## female population
  ## ---------------

  # Model with only income 
  cox.female.a <- coxph(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ pensize,
                          data=subset(retire, SEXO=="female"))
  
  # Adding the other wealth variables
  cox.female.b <- coxph(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ pensize + ESREAL5 + mobil + HousReg,
                        data=subset(retire, SEXO=="female"))
  
  # Adding contextual variables
  cox.female.c <- coxph(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                             FNAC + DIS + civil.status + hh,
                        data=subset(retire, SEXO=="female"))
  
  
  ### Model results
  summary(cox.male.a)
  summary(cox.male.b)
  summary(cox.male.c)
  cox.zph( cox.male.a)

  summary(cox.female.a)
  summary(cox.female.b)
  summary(cox.female.c)
  
  
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  
  
  ### 3.3.3 Two survival models for men and women (3 categories)
  #
  # To account for the different age at death distribution as well as the different life course trajectories,
  # the hazards of dying for the two genders will estimated separately in two models
  
  ## male population
  ## ---------------
  
  # Model with only income
  cox.male.3a <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize.3,
                      data=subset(retire, SEXO=="male"))
  
  
  # Adding the other wealth variables
  cox.male.3b <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg,
                      data=subset(retire, SEXO=="male"))
  
  # Adding contextual variables
  cox.male.3c <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                        FNAC + DIS + civil.status + hh,
                      data=subset(retire, SEXO=="male"))
  
        ### %%% Disability comparison
        cox.male.3c.dis <- coxph(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                            FNAC + civil.status + hh + strata(DIS),
                       data=subset(retire, SEXO=="male"))
        
        AIC(cox.male.3c.dis)-AIC(cox.male.3c) # quite a big difference
  
  ## female population
  ## ---------------
  
  # Model with only income 
  cox.female.3a <- coxph(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ pensize.3,
                        data=subset(retire, SEXO=="female"))
  
  # Adding the other wealth variables
  cox.female.3b <- coxph(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg,
                        data=subset(retire, SEXO=="female"))
  
  # Adding contextual variables
  cox.female.3c <- coxph(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                          FNAC + DIS + civil.status + hh,
                        data=subset(retire, SEXO=="female"))
  
  ### %%% Disability comparison
  cox.female.3c.dis <- coxph(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                             FNAC + civil.status + hh + strata(DIS),
                           data=subset(retire, SEXO=="female"))
  
  AIC(cox.female.3c.dis) - AIC(cox.female.3c)
  
  ### Model results
  summary(cox.male.3a)
  summary(cox.male.3b)
  summary(cox.male.3c)
  cox.zph( cox.male.3a)
  
  summary(cox.female.3a)
  summary(cox.female.3b)
  summary(cox.female.3c)
  summary(cox.female.3c.dis)
  
  
  
  
  ### ------------------------------------------------------------------------------------------------- ###
  ### ------------------------------------------------------------------------------------------------- ###
  ### 4. Output Tables
  
  
  ## 4.1. Separate Models - Males (3 Cat)
  
  
  stargazer(cox.male.3a,cox.male.3b,cox.male.3c, title="Cox PH Model",no.space=F,
            ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
            covariate.labels=c("$>$ 1200 Euro/month","$<$ 600 Euro/month",
                               "Tertiary Ed.","Secondary Ed.","Primary Ed.", "No car avail.",
                               "Does not own house/apt","Birth year (cohort)", "Received Disability Pension",
                               "Single","Widowed","2 Person Household"),
            single.row=FALSE, apply.coef = exp)
  # 
  # 
  # 
  # ## 4.1. Separate Models - Females (3 Cat)
  
  stargazer(cox.female.3a,cox.female.3b, cox.female.3c, title="Cox PH Model",no.space=F,
            ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
            covariate.labels=c("$>$ 1200 Euro/month","$<$ 600 Euro/month",
                               "Tertiary Ed.","Secondary Ed.","Primary Ed.", "No car avail.",
                               "Does not own house/apt","Birth year (cohort)", "Received Disability Pension",
                               "Single","Widowed","2 Person Household"),
            single.row=FALSE, apply.coef = exp)
  
### --------------------------------------------------------------------------------------------------------------- ###  
  
  ## 4.3. Separate Models - Males (4 Cat)
  
  
  stargazer(cox.male.a,cox.male.b,cox.male.c, title="Cox PH Model",no.space=F, 
            ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
            covariate.labels=c("1000-1999  Eur/month","650-999 Eur/month","$<$ 650 Eur/month",
                               "Tertiary Ed.","Secondary Ed.","Primary Ed.", "No car avail.",
                               "Does not own house/apt","Birth year (cohort)", "Received Disability Pension",
                               "Single","Widowed","2 Person Household"),
            single.row=TRUE, apply.coef = exp)
  
  
  
  ## 4.4 Separate Models - Females (4 Cat)
  
  stargazer(cox.female.a,cox.female.b, cox.female.c, title="Cox PH Model",no.space=F, 
            ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
            covariate.labels=c("1000-1499  Eur/month","650-999 Eur/month", "$<$ 650 Eur/month",
                               "Tertiary Ed.","Secondary Ed.","Primary Ed.", "No car avail.",
                               "Does not own house/apt","Birth year (cohort)", "Received Disability Pension",
                               "Single","Widowed","2 Person Household"),
            single.row=TRUE, apply.coef = exp)

  
  

  ## 4.5. Different form of display - Results in a Forest Plot - May be for the presentation
  
  # male population
  ggforest(cox.male.a, plot.title = "", ggtheme = theme_minimal(),xlab = "Hazard Ratio",refLabel = "reference")
  
  # female population
  ggforest(cox.female.b,plot.title = "",ggtheme = theme_minimal(), xlab = "Hazard Ratio")
  

  
  
  # ----------------------------- 
  # The same pattern as in previous models show here
  # ----------------------------- 
  
  
  #### Testing if a stratified model or an interaction model do fit the data better #####
  
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
  
  
 
 ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
 ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
 ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
  
  
 
 ### Test the flexible parametric models
 
 ## check distribution
 library(fitdistrplus)
 descdist(km2.a1$surv, discrete = T)
 descdist(km2.b1$cumhaz, discrete = T)
 
 
 #### Flexible parametric models
 
 library(flexsurv)
 # library(grofit)
 
     ### The flexsurvreg function ignores the strata function => separate analysis for men and women
 
### f(t) - cumulative distribution of deaths over time
 
## check distribution
 
# ggcoxadjustedcurves(cox.female.b)
 
    # males - cox model from earlier
 
    cox.m.1 <- survfit(cox.male.a) 
    # surv-curve
    plot(cox.m.1, xlab="Age", xlim=c(60,101),
      ylab="Proportion Survivors")
    # cumlative hazard
    plot(cox.m.1$time, cox.m.1$cumhaz, xlab="Age", xlim=c(60,101),
        ylab="Cumulative Hazard")
    descdist(cox.m.1$cumhaz, discrete = F)

 ## First model with only pension size 
 
 ## separate model for the male population -  only pension size
 
 flex.m.pen <- flexsurvreg(formula = Surv(time=entry.age.r,
                                          time2=exit.age,
                                          event=event) ~ pensize.sw, 
                           data = subset(retire, SEXO=="male"), 
                           dist="gompertz")
 
 ### Model Output
 flex.m.pen

 ## Easy Plots 
 plot(flex.m.pen,type="survival", xlim=c(60,100))
 plot(flex.m.pen,type="cumhaz")
 
 ## full flexible parametric model for the male population
 
 flex.m <- flexsurvreg(formula = Surv(time=entry.age.r,
                                      time2=exit.age,
                                      event=event) ~ pensize.sw + ESREAL5 + DIS + 
                         civil.status + mobil + HousReg + hh, 
                       data = subset(retire, SEXO=="male"), 
                       dist="gompertz")
 
 ### Model Output
 flex.m
 
 
 ## full flexible parametric model for the female population
 flex.f <- flexsurvreg(formula = Surv(time=entry.age.r,
                                      time2=exit.age,
                                      event=event) ~ pensize.sw + ESREAL5 + DIS + 
                         civil.status + mobil + HousReg + hh, 
                       data = subset(retire, SEXO=="female"), 
                       dist="gompertz")

 ### Model Output
 flex.f 
 
 ### ------------------------------------------------------------------------------------------------- ###   
 
 ### clean up one more time
 rm(cox.pen.1.m,cox.pen.2.m,cox.pen.1.f,cox.pen.2.f,cox.all.a,cox.all.b,cox.male.a,flex.ph.1,km2.a1,km2.b1,km2.p,km2b, ret.interaction.sex, ret.separate,km2.pb,
    flex.m.pen)
 
 ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##  
 ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##  
 ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##  
 ## Model using pension size as normalized continuous variable
 
 summary(retire$income_Retirement)
 summary(retire$INCOME)
 
 # Normalize the pension size
 
 # Standard Deviation & Mean
 SD <- sd(retire$INCOME)  # 503.1832
 M <- mean(retire$INCOME) # 861.9560
 
 retire <- retire %>% mutate(INCOME.N = (INCOME-M)/SD)
 
 hist(retire$INCOME.N)
 
 # Model with normalized income
 
 cox.male.N <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ INCOME.N,
                          data=subset(retire, SEXO=="male"))
 
 cox.female.N <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ INCOME.N,
                     data=subset(retire, SEXO=="female"))
 
 summary(cox.male.N)
 
 # Adding contextual variables
 cox.male.3c <- coxph(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ INCOME.N + ESREAL5 + mobil + HousReg + 
                          FNAC + DIS + civil.status + hh,
                        data=subset(retire, SEXO=="male"))
 
 summary(cox.male.3c)
 
 ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##  
 ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##  
 ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ## 
 ## For ALAP !!!
 
 retire <- within(retire, ESREAL3 <- relevel(ESREAL3, ref = "Secondary or higher Educ."))
 
 cox.strat.1 <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ ESREAL3 + mobil + HousReg + 
                        FNAC + DIS + civil.status + hh + strata(SEXO)
                      , data=retire)
 summary(cox.strat.1)
 
 
 stargazer(cox.strat.1, title="Stratified Cox PH Model",no.space=F, 
           ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
           covariate.labels=c("No or Incomplete Educ.","Primary Educ.", "No car avail.",
                              "Does not own house/apt","Birth year (cohort)", "Received Disability Pension",
                              "Single","Widowed","2 Person Household"),
           single.row=TRUE, apply.coef = exp)
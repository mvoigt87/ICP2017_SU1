### ------------------------------------------------------------------------------------------------- ###
###                     Descriptive Statisitics and Survival analysis                                 ###
### ------------------------------------------------------------------------------------------------- ###
### 1  Statistical tests - event variable in combination with potential covariates

### 2  Correlation test between wealth variables
 
### 3  Test for homogeneous populations (by sex, disability)
 
### 4  Kaplan Meier Estimation for different sub-groups

### ------------------------------------------------------------------------------------------------- ### 
rm(list=ls())
### 0.1. Loading data set
  #load("031_RETIND.RData")
   load("031b_RETIND.RData")
  # ----------------------------- 
  # 547813 individuals
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
 
 ### 1. Combination of the event 
 
 # 1.1 Event distribution by age and sex (plus chi square tests)
 
 # event distribution by sex - table
 round(prop.table(table(retire.A$event,retire.A$SEXO),2), digits = 2)
 chisq.test(retire.A$event,retire.A$SEXO, simulate.p.value = FALSE, B = 20000)
 # X-squared = 3857.4, df = 1, p-value < 2.2e-16

 # -----------------------------
 #            female  male
 #   censored  0.91% 0.83%
 #   dead      0.09% 0.17%
 # -----------------------------
 
 summary(retire.A$exit.age[retire.A$event==1])
 summary(retire.A$exit.age[retire.A$event==0])
   
   # -----------------------------
   #### age at death
   #      Min. 1st Qu.  Median    Mean  3rd Qu.    Max.
   #     65.01   76.78   82.14   81.67   87.01   99.77      # new values for the "dead" population make more sense
                                                              # They are much higher on average.
   #### age at censorship
   #     65.01   68.98   73.57   74.93   79.94   99.99
   # -----------------------------
 
 summary(aov(retire.A$exit.age ~ retire.A$event))
   # -----------------------------

   # histogram of age at exit distribution
retire.A %>% ggplot(aes(x=exit.age,fill=as.factor(event)))+
   geom_histogram(bins=40)+
   scale_fill_discrete(name = "")
 # birth cohort
retire.A %>% ggplot(aes(x=FNAC,fill=as.factor(event)))+
   geom_histogram(bins=40)+
   scale_fill_discrete(name = "")
   # -----------------------------
   #clear concentration of deaths at the older ages - bump around the age 83
 
  # Sex differences in the age at death distribution
 
retire.A %>% dplyr::filter(event==1) %>% ggplot(aes(x=exit.age))+
    geom_histogram(aes(y=50*..density..),bins=40) +
    facet_grid(.~ SEXO) +
    theme_bw() 
    # relative distribution of ages at death show similar patterns but more noise (less cases, less deaths) for women

### ------------------------------------------------------------------------------------------------------------ ###  
###### Event distribution by education, pension income and other variables of social position (statistical tests)
### ------------------------------------------------------------------------------------------------------------ ###  
 
 
## 1.2 Event by educational group
 
round(prop.table(table(retire.A$event, retire.A$ESREAL5),2),digits = 2)
chisq.test(table(retire.A$event, retire.A$ESREAL5))
  # ----------------------------- 
  #                  No or Incomplete Educ.  Tertiary Educ.  Secondary Educ.  Primary Educ.         # column percentage
  #   censored                        0.81%           0.89%            0.90%          0.86%
  #   dead                            0.19%           0.11%            0.10%          0.14%
  
  ### These results indicate a mortality gradient but need to be interpreted carefully as these percentages are
  ### not age or time-specific. Oldest individuals have lower share of higher educated individuals

  # bar plot of event distribution by education
retire.A %>% ggplot(aes(x=ESREAL5,fill=as.factor(event))) +
   geom_bar(stat = "count") +
   scale_fill_discrete(name = "") + theme_bw()

## b) Education - income relationship
round(prop.table(table(retire.A$pensize, retire.A$ESREAL5),2),digits = 2)  ### strongly related and possibly interacting


## 1.3 Event by pension income
# --------------------------- #

# Visual test - Graph Income distribution   - !RELATIVE FREQUENCY!

### IN GREY tones - !!! ONLY RETIREMENT

# aggregate(retire.A$income_Retirement,by=list(retire.A$SEXO),FUN=mean) -----  #### if disability is included

aggregate(retire.A$INCOME,by=list(retire.A$SEXO),FUN=mean)

# 1  female  743.4845 Euro
# 2    male  1052.9420 Euro

INC.DIS <- retire.A %>% dplyr::mutate(grp.mean = ifelse(SEXO=="female",743.4845,1052.9420)) %>% 
  ggplot(aes(x=INCOME, color=SEXO)) +
  geom_histogram(aes(y=50*..density..),fill="white", alpha=0.5, position="dodge",binwidth = 50) +
  geom_vline(aes(xintercept=grp.mean, color=SEXO),
             linetype="dashed") +
  scale_color_manual(values=c("#000000", "#A9A9A9"), name=" ") +
  scale_x_continuous(name="Montly Public Pension Income (in €)", limits = c(300,3500)) +
  scale_y_continuous(name = "Relative Frequency") +
  theme_bw()

INC.DIS <- INC.DIS + theme(legend.position = c(0.85, 0.85))

  # 1  female  743.4845 Euro
  # 2    male  1052.9420 Euro

    ### IN GREY tones
     #  
     # INC.DIS <- retire.A %>% dplyr::mutate(grp.mean = ifelse(SEXO=="female",743.4845,1052.9420)) %>% 
     #    ggplot(aes(x=INCOME, color=SEXO)) +
     #    geom_histogram(aes(y=50*..density..),fill="white", alpha=0.5, position="dodge",binwidth = 50) +
     #    geom_vline(aes(xintercept=grp.mean, color=SEXO),
     #               linetype="dashed") +
     #    scale_color_manual(values=c("#000000", "#A9A9A9"), name=" ") +
     #    scale_x_continuous(name="Montly Public Pension Income (in €)", limits = c(300,3500)) +
     #    scale_y_continuous(name = "Relative Frequency") +
     #    theme_bw()
     # 
     # INC.DIS <- INC.DIS + theme(legend.position = c(0.85, 0.85))

  
  
# ---------------------------------------------------------- #
# Visual test - Graph Income distribution death and alive
  
aggregate(retire.A$INCOME,by=list(retire.A$event),FUN=mean)
  
  # alive 981.6124
  # dead  890.1289
  
  
INC.E.DIS <- retire.A %>% dplyr::mutate(event = as.character(ifelse(event==0, "alive", "dead"))) %>% 
    dplyr::mutate(grp.mean = ifelse(event=="alive",981.6124,890.1289)) %>% 
    ggplot(aes(x=INCOME, color=event)) +
    geom_histogram(aes(y=50*..density..),fill="white", alpha=0.5, position="dodge",binwidth = 50) +
    geom_vline(aes(xintercept=grp.mean, color=event),
               linetype="dashed") +
    scale_color_manual(values=c("#000000", "#A9A9A9"), name=" ") +
    scale_x_continuous(name="Montly Public Pension Income (in €)", limits = c(0,3500)) +
    scale_y_continuous(name = "Relative Frequency") +
    theme_bw()
  
INC.E.DIS <- INC.E.DIS + theme(legend.position = c(0.85, 0.85))
  

# ----------------------------------------------------- #
## Table event distribution by income with 4 categories
# ----------------------------------------------------- #

round(prop.table(table(retire.A$event, retire.A$pensize),2),digits = 2)
  # ----------------------------- 
  #             less than 650 Euro  650-999 Euro   1000-1999 Euro     more than 2000          # column percentage
  #   censored         0.84%            0.85%           0.86%               0.92%
  #   dead             0.16%            0.15%           0.14%               0.08%
  # ----------------------------- 

#### %%%%%%%%%%%%%%%%%%%%%% ####

## Event by pension income by 3 categories
  
round(prop.table(table(retire.A$event, retire.A$pensize.3),2),digits = 2)
  # ----------------------------- 
  #              more than 1500 Euro  1000-1499 Euro   less than 600 Euro   # column percentage
  # censored                   0.88%           0.84%                0.84%
  # dead                       0.12%           0.16%                0.16%
  # X-squared = 26689, df = 13118, p-value < 2.2e-16
  # ----------------------------- 
  
  
#### %%%%%%%%%%%%%%%%%%%%%% #### 
  
## 1.4 Event by housing ownership status
  
round(prop.table(table(retire.A$event, retire.A$HousReg),2),digits = 2)
chisq.test(table(retire.A$event, retire.A$HousReg))
  # ----------------------------- 
  #            Not owned   owned
  # censored       0.83%   0.86%
  # dead           0.17%   0.14%
  # X-squared = 187.71, df = 1, p-value < 2.2e-16
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
  
aggregate(retire.A$entry.age, by=list(retire.A$SEXO), FUN=mean)[2]
  
##### Using data table
  
dt <- data.table(retire.A)
dt[,list(mean=mean(entry.age),median=median(entry.age),iqr=IQR(entry.age)),by=SEXO]
  # ----------------------------- 
  #      SEXO     mean median   iqr
  # 1:   male 72.39648  70.81 12.26
  # 2: female 71.15017  68.56 10.16
  # ----------------------------- 
  
  
  ## age at death
  ## -----------------------------  
dt[,list(mean=mean(exit.age),median=median(exit.age),iqr=IQR(exit.age)),by=.(SEXO,event)]  
  # -----------------------------  
  #      SEXO event     mean median   iqr
  # 1:   male     1 81.34898  81.82 10.02
  # 2:   male     0 75.28503  74.12 11.19  
  # 3: female     1 83.33520  84.16 11.05
  # 4: female     0 74.07427  72.56 10.16
  # -----------------------------
  
  ## male
round(prop.table(table(retire.A$event[retire.A$SEXO=="male"], retire.A$pensize[retire.A$SEXO=="male"]),2),digits = 2)
chisq.test(table(retire.A$event[retire.A$SEXO=="male"], retire.A$pensize[retire.A$SEXO=="male"]))

  ## female
round(prop.table(table(retire.A$event[retire.A$SEXO=="female"], retire.A$pensize[retire.A$SEXO=="female"]),2),digits = 2)
chisq.test(table(retire.A$event[retire.A$SEXO=="female"], retire.A$pensize[retire.A$SEXO=="female"]))

### Interesting - middle categories don´t seem to be very different
  
##### 3 categories

  ## male
round(prop.table(table(retire.A$event[retire.A$SEXO=="male"], retire.A$pensize.3[retire.A$SEXO=="male"]),2),digits = 2)
chisq.test(table(retire.A$event[retire.A$SEXO=="male"], retire.A$pensize.3[retire.A$SEXO=="male"]))

  ## female
round(prop.table(table(retire.A$event[retire.A$SEXO=="female"], retire.A$pensize.3[retire.A$SEXO=="female"]),2),digits = 2)
chisq.test(table(retire.A$event[retire.A$SEXO=="female"], retire.A$pensize.3[retire.A$SEXO=="female"]))

### For men the 3 categories seem to work better - for women not

##### ------------------------------------------------------------------------------------------------- ###   
##### 3  Test for other homogeneous populations (by sex, disability)  
##### ------------------------------------------------------------------------------------------------- ###   

# education
 # men
round(prop.table(table(retire.A$event[retire.A$ESREAL3=="No or Incomplete Educ." & retire.A$SEXO=="male"], 
                       retire.A$pensize.3[retire.A$ESREAL3=="No or Incomplete Educ." & retire.A$SEXO=="male"]),2),digits = 2)

round(prop.table(table(retire.A$event[retire.A$ESREAL3=="Primary Educ." & retire.A$SEXO=="male"], 
                       retire.A$pensize.3[retire.A$ESREAL3=="Primary Educ." & retire.A$SEXO=="male"]),2),digits = 2)

round(prop.table(table(retire.A$event[retire.A$ESREAL3=="Secondary or higher Educ." & retire.A$SEXO=="male"], 
                       retire.A$pensize.3[retire.A$ESREAL3=="Secondary or higher Educ." & retire.A$SEXO=="male"]),2),digits = 2)

 # women
round(prop.table(table(retire.A$event[retire.A$ESREAL3=="No or Incomplete Educ." & retire.A$SEXO=="female"], 
                       retire.A$pensize.3[retire.A$ESREAL3=="No or Incomplete Educ." & retire.A$SEXO=="female"]),2),digits = 2)

round(prop.table(table(retire.A$event[retire.A$ESREAL3=="Primary Educ." & retire.A$SEXO=="female"], 
                       retire.A$pensize.3[retire.A$ESREAL3=="Primary Educ." & retire.A$SEXO=="female"]),2),digits = 2)

round(prop.table(table(retire.A$event[retire.A$ESREAL3=="Secondary or higher Educ." & retire.A$SEXO=="female"], 
                       retire.A$pensize.3[retire.A$ESREAL3=="Secondary or higher Educ." & retire.A$SEXO=="female"]),2),digits = 2)


# housing/car
round(prop.table(table(retire.A$event[retire.A$HousReg=="owned"], retire.A$pensize.3[retire.A$HousReg=="owned"]),2),digits = 2)

round(prop.table(table(retire.A$event[retire.A$HousReg!="owned"], retire.A$pensize.3[retire.A$HousReg!="owned"]),2),digits = 2)


round(prop.table(table(retire.A$event[retire.A$mobil=="car(s) available"], retire.A$pensize.3[retire.A$mobil=="car(s) available"]),2),digits = 2)

round(prop.table(table(retire.A$event[retire.A$mobil=="no car available"], retire.A$pensize.3[retire.A$mobil=="no car available"]),2),digits = 2)
## short summary: Big between group (own/ not own) differences but no indication for an interaction with pension


##### ------------------------------------------------------------------------------------------------- ###   
##### 4 Kaplan Meier Estimates
##### ------------------------------------------------------------------------------------------------- ###   

  ## Entry time/age = entry.age (revise for <65 year old who enter after 2011)
  
  ## Exit time/age = exit.age
  
  ## Event marker = event
  
### 4.1 Visual exploration through Kaplan Meier Estimators
  #     To account for left truncation, a cox ph approximation is used to estimate the KME
  
KM1 <- survfit(coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event)~1,data=retire.A),type="kaplan-meier")
  # -----------------------------    
  # records    n.max  n.start   events   median  0.95LCL  0.95UCL 
  # 547813.0 142208.0  84385.0  81176.0     85.1     85.1     85.2
  # -----------------------------  
  
KM1
KM1b <- tidy(KM1) %>% 
    ggplot() +
    geom_step(mapping=aes(x=time, y=estimate)) +
    scale_y_continuous(name = "Survival Probability")                  +
    scale_x_continuous(name = "Age") +
    theme_bw()
  
    rm(KM1,KM1b)
# -----------------------------


### --------------------- ###
### 3.2 sex differences ###
### --------------------- ###

  # male population
km2.a1 <- survfit(coxph(Surv(time=entry.age,
                               time2=exit.age,
                               event=event)~1, data=subset(retire.A,SEXO=="male")), 
                    data=subset(retire,SEXO=="male"), type="kaplan-meier")
  
  # female population
km2.b1 <- survfit(coxph(Surv(time=entry.age,
                               time2=exit.age,
                               event=event)~1, data=subset(retire.A,SEXO=="female")), 
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
    ## check distribution
    # descdist(km2.a1$surv, discrete = T)
    # descdist(km2.b1$cumhaz, discrete = T)
    
    rm(km2.p,km2.a1, km2.b1, km2.pb)
    
  
### ------------------------------- ###
### 3.3 differences in disability ###
### ------------------------------- ###    
  
    # # Without Disability
    # km3.a <- survfit(coxph(Surv(time= entry.age.r,
    #                       time2 = exit.age,
    #                       event = event)~1, data = subset(retire, DIS==0)),
    #                  data=subset(retire,DIS==0), type="kaplan-meier")
    # 
    # # With Disability
    # km3.b <- survfit(coxph(Surv(time= entry.age.r,
    #                             time2 = exit.age,
    #                             event = event)~1, data = subset(retire, DIS==1)),
    #                  data=subset(retire,DIS==1), type="kaplan-meier")
    # 
    # # Visualization
    # km3.wd <- tidy(km3.a) %>% dplyr::select(estimate,time) %>% dplyr::mutate(Dis="no disability")
    # km3.d <- tidy(km3.b) %>% dplyr::select(estimate,time) %>% dplyr::mutate(Dis="with disability")
    # 
    # km3.pl <- dplyr::union(km3.wd,km3.d) %>% 
    #   ggplot() +
    #   geom_step(mapping=aes(x=time,y=estimate,color=Dis)) +
    #   scale_y_continuous(name = "Survival Probability")  +
    #   scale_x_continuous(name = "Age") +
    #   scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
    #   theme_minimal()
    # 
    # ## as expected the disabled individuals die off faster - differences are not as pronounced as the sex differences
    # 
    # ##     records    n.max  n.start   events   median  0.95LCL  0.95UCL
    # ##  No dis -------------------------------------------------------
    # ##    550581.0 132427.0  31993.0  89951.0     85.6     85.5     85.7
    # ##  Dis -------------------------------------------------------    
    # ##    184101.0  49223.0   1114.0  48829.0     82.2     82.1     82.3 
    # 
    # rm(km3.a, km3.b, km3.wd, km3.d, km3.pl)
    
### --------------------- ###
### 3.4 Education       ###
### --------------------- ###
    # Illitates and incomplete
    km3.a1 <- survfit(coxph(Surv(time=entry.age,
                                 time2=exit.age,
                                 event=event)~1, data=subset(retire.A,ESREAL5=="No or Incomplete Educ.")), 
                      data=subset(retire.A,ESREAL5=="No or Incomplete Educ."), type="kaplan-meier")
    # Primary
    km3.a2 <- survfit(coxph(Surv(time=entry.age,
                                 time2=exit.age,
                                 event=event)~1, data=subset(retire.A,ESREAL5=="Primary Educ.")), 
                      data=subset(retire.A,ESREAL5=="Primary Educ."), type="kaplan-meier")
    # Secondary
    km3.a3 <- survfit(coxph(Surv(time=entry.age,
                                 time2=exit.age,
                                 event=event)~1, data=subset(retire.A,ESREAL5=="Secondary Educ.")), 
                      data=subset(retire.A,ESREAL5=="Secondary Educ."), type="kaplan-meier")
    # Tertiary
    km3.a4 <- survfit(coxph(Surv(time=entry.age,
                                 time2=exit.age,
                                 event=event)~1, data=subset(retire.A,ESREAL5=="Tertiary Educ.")), 
                      data=subset(retire.A,ESREAL5=="Tertiary Educ."), type="kaplan-meier")
    
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
    # Survival curves run almost perfectly parallel for most of the time + gradient visible
    # ----------------------------- 
rm(KM3, KM3.1,KM3.2,KM3.3,KM3.4,km3.a1,km3.a2,km3.a3,km3.a4)
    
    
### ------------------------------------ ###
### 3.5 Sex/Pension Size (categorical)   ###
### ------------------------------------ ###
   # less than 650 / male
km4.a1 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="male" & pensize=="less than 650 Euro")), 
                     data=subset(retire.A,SEXO=="male" & pensize=="less than 650 Euro"), type="kaplan-meier")
   # less than 650 / female
km4.b1 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="female" & pensize=="less than 650 Euro")), 
                     data=subset(retire.A,SEXO=="female" & pensize=="less than 650 Euro"), type="kaplan-meier")
   # 650-999 / male
km4.a2 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="male" & pensize=="650-999 Euro")), 
                     data=subset(retire.A,SEXO=="male" & pensize=="650-999 Euro"), type="kaplan-meier")
   # 650-999 / female
km4.b2 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="female" & pensize=="650-999 Euro")), 
                     data=subset(retire.A,SEXO=="female" & pensize=="650-999 Euro"), type="kaplan-meier")
   # 1000-1999 / male
km4.a3 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="male" & pensize=="1000-1999 Euro")), 
                     data=subset(retire.A,SEXO=="male" & pensize=="1000-1999 Euro"), type="kaplan-meier")
   # 1000-1999 / female
km4.b3 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="female" & pensize=="1000-1999 Euro")), 
                     data=subset(retire.A,SEXO=="female" & pensize=="1000-1999 Euro"), type="kaplan-meier")
   # more than 2000 / male
km4.a4 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="male" & pensize=="more than 2000 Euro")),
                     data=subset(retire.A,SEXO=="male" & pensize=="more than 2000 Euro"), type="kaplan-meier")
   # more than 2000 / female
km4.b4 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="female" & pensize=="more than 2000 Euro")),
                     data=subset(retire.A,SEXO=="female" & pensize=="more than 2000 Euro"), type="kaplan-meier")
   
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
  
KM_SEX <- KM_SEX %>% ggplot() +
    geom_step(mapping = aes(x=time, y=estimate, linetype=sex, color=pensize.sw)) +
    scale_y_continuous(name = "Survival Probability")          +
    scale_x_continuous(name = "Age")                           +
    scale_color_discrete(name="")                              +
    scale_linetype_discrete(name="")                           +
    theme_minimal()
KM_SEX  
  ### Very interesting survival curve differences between men and women - visually there is a survival advantage of
  ### better earning men while probably the low case number makes the curves indistiguishable for women
  
  # delete the help files
 rm(KM_SEX.a1,KM_SEX.a2,KM_SEX.a3,KM_SEX.a4, KM_SEX.b1, KM_SEX.b2, KM_SEX.b3, KM_SEX.b4, km4.a1,  km4.a2,
     km4.a3,  km4.a4, km4.b1,  km4.b2,  km4.b3,  km4.b4)
   
   
##--------------------------------------
## A few descriptive tests to see why the women with more than 2000 fluctutate so much
   
#table(retire.A$SEXO,retire.A$pensize.sw)
  
#table(retire$event[retire$pensize.sw=="more than 2000 Euro"],retire$SEXO[retire$pensize.sw=="more than 2000 Euro"])
 
### --------------------------------------------- ###
### 3.6 Sex/Pension Size (categorical - 3 cats) ###
### --------------------------------------------- ###
   
   # less than 600 / male
km5.a1 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A, SEXO=="male" & pensize.3=="less than 600 Euro")), 
                     data=subset(retire.A,SEXO=="male" & pensize.3=="less than 600 Euro"), type="kaplan-meier")
   # less than 600 / female
km5.b1 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="female" & pensize.3=="less than 600 Euro")), 
                     data=subset(retire.A,SEXO=="female" & pensize.3=="less than 600 Euro"), type="kaplan-meier")
   
   
   
   # 600-1199 / male
km5.a2 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="male" & pensize.3=="600-1199 Euro")), 
                     data=subset(retire.A,SEXO=="male" & pensize.3=="600-1199 Euro"), type="kaplan-meier")
   # 1000-1199 / female
km5.b2 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="female" & pensize.3=="600-1199 Euro")), 
                     data=subset(retire.A,SEXO=="female" & pensize.3=="600-1199 Euro"), type="kaplan-meier")
   
   
   # 1200+ / male
km5.a3 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="male" & pensize.3=="more than 1200 Euro")), 
                     data=subset(retire.A,SEXO=="male" & pensize.3=="more than 1200 Euro"), type="kaplan-meier")
   # 1200+ / female
km5.b3 <- survfit(coxph(Surv(time=entry.age,
                                time2=exit.age,
                                event=event)~1, data=subset(retire.A,SEXO=="female" & pensize.3=="more than 1200 Euro")), 
                     data=subset(retire.A,SEXO=="female" & pensize.3=="more than 1200 Euro"), type="kaplan-meier")
   
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
   
# Relative clear gradient of men but crossing survival curves for women (with least wealthy group with lowest SP at the end)
   
rm(KM5A,KM5B,KM5C,KM5D,KM5E,KM5F,km5.a1,km5.a2,km5.a3,km5.b1,km5.b2,km5.b3)
   
### --------------------------------------------------------------- ###
### 3.7 Impact of disability - stratified KME (3 income groups)   ###
### --------------------------------------------------------------- ###

#    # less than 600 / disabled / male
# km6.a1 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire, DIS==1 & pensize.3=="less than 600 Euro" & SEXO=="male")), 
#                      data=subset(retire,DIS==1 & pensize.3=="less than 600 Euro" & SEXO=="male"), type="kaplan-meier")
#    
#    # less than 600 / not disabled / male
# km6.b1 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire,DIS==0 & pensize.3=="less than 600 Euro"& SEXO=="male")), 
#                      data=subset(retire,DIS==0 & pensize.3=="less than 600 Euro" & SEXO=="male"), type="kaplan-meier")
#    
#    # 600-1199 / disabled / male
# km6.a2 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire, DIS==1 & pensize.3=="600-1199 Euro" & SEXO=="male")), 
#                      data=subset(retire,DIS==1 & pensize.3=="600-1199 Euro" & SEXO=="male"), type="kaplan-meier")
#    
#    # 600-1199 / not disabled / male
# km6.b2 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire,DIS==0 & pensize.3=="600-1199 Euro" & SEXO=="male")), 
#                      data=subset(retire,DIS==0 & pensize.3=="600-1199 Euro" & SEXO=="male"), type="kaplan-meier")
#    
#    # more than 1200 / disabled / male
# km6.a3 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire, DIS==1 & pensize.3=="more than 1200 Euro" & SEXO=="male")), 
#                      data=subset(retire,DIS==1 & pensize.3=="more than 1200 Euro" & SEXO=="male"), type="kaplan-meier")
#    
#    # more than 1200 / not disabled / male
# km6.b3 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire,DIS==0 & pensize.3=="more than 1200 Euro" & SEXO=="male")), 
#                      data=subset(retire,DIS==0 & pensize.3=="more than 1200 Euro" & SEXO=="male"), type="kaplan-meier")
#    
#    
#    ### Extract information for plotting
# KM6.A <- tidy(km6.a1) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
#      dplyr::mutate(pensize="less than 600 Euro") %>% dplyr:: mutate (sex="male")
# KM6.B <- tidy(km6.b1) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
#      dplyr::mutate(pensize="less than 600 Euro") %>% dplyr:: mutate (sex="male")
# KM6.C <- tidy(km6.a2) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
#      dplyr::mutate(pensize="600-1199 Euro") %>% dplyr:: mutate (sex="male")
# KM6.D <- tidy(km6.b2) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
#      dplyr::mutate(pensize="600-1199 Euro") %>% dplyr:: mutate (sex="male")
# KM6.E <- tidy(km6.a3) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
#      dplyr::mutate(pensize="more than 1200 Euro") %>% dplyr:: mutate (sex="male")
# KM6.F <- tidy(km6.b3) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
#      dplyr::mutate(pensize="more than 1200 Euro") %>% dplyr:: mutate (sex="male")
#    
# KM_DIS.M <- dplyr::union(KM6.A,KM6.B) %>% dplyr::union(KM6.C) %>% dplyr::union(KM6.D) %>% dplyr::union(KM6.E) %>% 
#               dplyr::union(KM6.F)
#    
# # ------------------------------------------------------------------------------------------------- #
# ###### FEMALE
# # less than 1000 / disabled / male
# km6.c1 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire, DIS==1 & pensize.3=="less than 600 Euro" & SEXO=="female")), 
#                      data=subset(retire,DIS==1 & pensize.3=="less than 600 Euro" & SEXO=="female"), type="kaplan-meier")
#    
# # less than 1000 / not disabled / male
# km6.d1 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire,DIS==0 & pensize.3=="less than 600 Euro" & SEXO=="female")), 
#                      data=subset(retire,DIS==0 & pensize.3=="less than 600 Euro" & SEXO=="female"), type="kaplan-meier")
#    
# # 1000-1499 / disabled / male
# km6.c2 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire, DIS==1 & pensize.3=="600-1199 Euro" & SEXO=="female")), 
#                      data=subset(retire,DIS==1 & pensize.3=="600-1199 Euro" & SEXO=="female"), type="kaplan-meier")
#    
# # 1000-1499 / not disabled / male
# km6.d2 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire,DIS==0 & pensize.3=="600-1199 Euro" & SEXO=="female")), 
#                      data=subset(retire,DIS==0 & pensize.3=="600-1199 Euro" & SEXO=="female"), type="kaplan-meier")
#    
# # more than 1500 / disabled / male
# km6.c3 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire, DIS==1 & pensize.3=="more than 1200 Euro" & SEXO=="female")), 
#                      data=subset(retire,DIS==1 & pensize.3=="more than 1200 Euro" & SEXO=="female"), type="kaplan-meier")
#    
# # more than 1500 / not disabled / male
# km6.d3 <- survfit(coxph(Surv(time=entry.age.r,
#                                 time2=exit.age,
#                                 event=event)~1, data=subset(retire,DIS==0 & pensize.3=="more than 1200 Euro" & SEXO=="female")), 
#                      data=subset(retire,DIS==0 & pensize.3=="more than 1200 Euro" & SEXO=="female"), type="kaplan-meier")
#    
#    
# ### Extract information for plotting
# KM7.A <- tidy(km6.c1) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
#      dplyr::mutate(pensize="less than 600 Euro") %>% dplyr:: mutate (sex="female")
# KM7.B <- tidy(km6.d1) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
#      dplyr::mutate(pensize="less than 600 Euro") %>% dplyr:: mutate (sex="female")
# KM7.C <- tidy(km6.c2) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
#      dplyr::mutate(pensize="600-1199 Euro") %>% dplyr:: mutate (sex="female")
# KM7.D <- tidy(km6.d2) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
#      dplyr::mutate(pensize="600-1199 Euro") %>% dplyr:: mutate (sex="female")
# KM7.E <- tidy(km6.c3) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="Disability") %>% 
#      dplyr::mutate(pensize="more than 1200 Euro") %>% dplyr:: mutate (sex="female")
# KM7.F <- tidy(km6.d3) %>% dplyr::select(estimate,time) %>% dplyr::mutate(DIS="No Disability") %>% 
#      dplyr::mutate(pensize="more than 1200 Euro") %>% dplyr:: mutate (sex="female")
#    
# KM_DIS.F <- dplyr::union(KM7.A,KM7.B) %>% dplyr::union(KM7.C) %>% dplyr::union(KM7.D) %>% dplyr::union(KM7.E) %>% 
#      dplyr::union(KM7.F) 
#    
# KMDIS <- dplyr::union(KM_DIS.M, KM_DIS.F)
#    
#    
#    
# ############# 
# #### Plot ###
# #############
# KMDIS %>% 
#      ggplot()  +
#     geom_step(mapping = aes(x=time, y=estimate, linetype=DIS, color=pensize)) +
#     facet_grid(.~ sex)                                           +
#       scale_y_continuous(name = "Survival Probability")          +
#       scale_x_continuous(name = "Age")                           +
#       scale_color_brewer(palette="Dark2", name=" ")              +
#       scale_linetype_discrete(name="")                           +
#       theme_minimal()
#      
#   rm(KM_DIS,KM6.A,KM6.B,KM6.C,KM6.D,KM6.E,KM6.F,km6.a1,km6.a2,km6.a3,km6.b1,km6.b2,km6.b3,
#      KM7.A,KM7.B,KM7.C,KM7.D,KM7.E,KM7.F,km6.c1,km6.c2,km6.c3,km6.d1,km6.d2,km6.d3)
#    
### ------------------------------------------------------------------------------------------------- ###  
### ------------------------------------------------------------------------------------------------- ###  
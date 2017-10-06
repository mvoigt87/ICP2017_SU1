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
  load("030_RetirementIND.RData")   # _oct
  load("060_ss_cc2002.RData")
 
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

 #            female  male           ## new values
 #   censored  0.91% 0.85%
 #   dead      0.09% 0.15%
 
 # bar plot events by sex
 retire %>% ggplot(aes(x=sex,fill=exit))+
   geom_bar(stat = "count")+
   scale_fill_discrete(name = "")
 
 summary(retire$age.exit[retire$exit=="dead"])
 summary(retire$age.exit[retire$exit=="censored"])
   #### age at death
   #      Min. 1st Qu.  Median    Mean  3rd Qu.    Max.
   #    57.26    76.53   81.92   81.08    86.52   94.90       # new values for the "dead" population make more sense
                                                              # They are much higher on average.
   #### age at census
   #   56.58    68.42    73.28   74.57    80.02   94.99 
 
 summary(aov(retire$age.exit ~ retire$exit))
   #                 Df   Sum Sq Mean Sq F value Pr(>F)    
   # retire$exit      1  3331876 3331876   61388 <2e-16 ***
   # Residuals   641973 34843448      54                 
 
 # histogram of age at exit distribution
 retire %>% ggplot(aes(x=age.exit,fill=exit))+
   geom_histogram(bins=40)+
   scale_fill_discrete(name = "")
 ## clear concentration at the older ages
 
 
 ################# 
 #################
 
 ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
 ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
 
 ## For a partner variable which starts at the time of entry to the risk set 2011
 
  cbind(colnames(retire))
  cbind(colnames(parned.c2002))
 
  ### Join all individuals who have a married partner in 2011
  
  part.R <- parned.c2002 %>% select(kID,FNAC) %>% mutate(GebJahr = FNAC) %>% select(-FNAC) %>% 
  #  create a partner marker for the ones in the parned data
  mutate(marstat = 1)
 
  ### Bring them back together and create a partnership variable (married in 2011/not married in 2011)
  
  # 1. Full Join command keeps all rows and all columns
  retire <- retire %>% full_join(part.R, by="kID")
  
  # create the marital status variable for the full individual dataset and make a factor variable out of it
  retire <- retire %>% mutate(civilst = factor(ifelse(is.na(marstat),"not married","married"))) %>% 
    ### drop the 2 unused variables
    select(-GebJahr, -marstat) %>% 
    ### and the cases we have no further information about (not originally in the "retire" data set)
    filter(!is.na(end.date))
  
  round(prop.table(table(retire$civilst)),digits = 2)
  
  #     married     not married 
  #       0.89%           0.11%        ### looks ok!
  
  round(prop.table(table(retire$civilst,retire$ECIVIL),2),digits = 2)
  
  #               single married widowed divorced/sep
  #   married       0.00    0.98    0.00         0.00
  #   not married   1.00    0.02    1.00         1.00  ### Compared to 2002 only 2% of the sample seem
                                                       ### to have lost their partner (divorce or widowed)
  #table(retire$ECIVIL[!is.na(retire$marstat)])
  
  mean(retire$age[retire$civilst=="married"])       # 70.71 years
  mean(retire$age[retire$civilst=="not married"])   # 73.12 years
  

  
  ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
  ##### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
  
 ### ------------------------------------------------------------------------------------------------- ###  
 
# 1.1.2 Event distribution by education, pension income and other variables of social position (statistical tests)
 
 ## Event by educational group
 
 ed.e.tbl <- table(retire$exit, retire$ESREAL)
 round(prop.table(ed.e.tbl,2),digits = 2)
 chisq.test(ed.e.tbl)
 rm(ed.e.tbl)
 
  #            Illiterate  Incomplete  Primary Educ.  Secondary Educ.  Tertiary Educ.         # column percentage
  #   censored       0.79       0.83          0.88            0.91           0.91
  #   dead           0.21       0.17          0.12            0.09           0.09
  # 	Pearson's Chi-squared test
  #    data:  table(retire$exit, retire$ESREAL)
  #    X-squared = 8821.3, df = 4, p-value < 2.2e-16                                          ### has changed (less)
  
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
  
 
  ## Event by Car ownership - everybody in the data set seems to have a car
  
  # car.e.tbl <- table(retire$exit, retire$car)
  # round(prop.table(car.e.tbl,2),digits = 2)
  # chisq.test(car.e.tbl)
  # rm(car.e.tbl)
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
  
  
  #### Changing the partnership variable
  
  ## extract the list of married
  
  
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

  # 3.1. Create the survival object (new = survival objects incoporated in functions)
  
  # time=age at death

  # SO <- Surv(time=retire$age,
  #            time2=retire$age.exit,
  #            event = retire$event)
  
  

  # 3.2. Kaplan Meier and Log Ranks for the main covariates
    # ### --------------------- ###
    # km1 <- survfit(SO~1)
    # km1
    # km1.b <- tidy(km1)
    # km1.b %>% ggplot() +
    #   geom_step(mapping=aes(x=time, y=estimate)) +
    #   scale_y_continuous(name = "Survival Probability")                  +
    #   scale_x_continuous(name = "Age")
    # 
    # # general non parametric survival curve
    # 
    # ##   records     n.max  n.start   events   median  0.95LCL  0.95UCL 
    # ##  641223.0  160208.0      0.0  85797.0     85.6     85.6     85.7 
    # rm(km1,km1.b)
  
  
  
  ####### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
  
  ### accounting for left truncation
  
  #       survfit(coxph(Surv(start,stop,event)~1,data=test2),type="kaplan-meier")
  
  # Using type="kaplan-meier" gives a product-type estimator that in
  # untruncated data would be the Kaplan-Meier. Without this you get exp(-H)
  # where H is the estimated cumulative hazard (an estimator variously
  #                                             attributed to Aalen, Breslow, Peto, and probably others).
  
  
  
  ### !!! Kaplan Meier Plots do not display the left truncated survival curve !!!
  
  
  
  ####### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
  
  #### Left truncated KM - through a cox ph estimation
  
  km1a <-     survfit(coxph(Surv(time=age, 
                                 time2 =age.exit,
                                 event = event)~1,data=retire),type="kaplan-meier")
  km1a
  km1a.b <- tidy(km1a)
  km1a.b %>% ggplot() +
    geom_step(mapping=aes(x=time, y=estimate)) +
    scale_y_continuous(name = "Survival Probability")                  +
    scale_x_continuous(name = "Age")
  
  rm(km1a,km1a.b)
     
     ### !!! start time is basically the only thing that changes = accounting for left truncation
  
  
  
  ### --------------------- ###
  # 3.2.2 sex differences
  ### --------------------- ###
  # km2 <- survfit(SO~sex, data = retire)
  # km2b <- survfit(coxph(Surv(time=age, 
  #             time2 =age.exit,
  #             event = event)~ sex ,data=retire),type="kaplan-meier")
  # km2
 
  
  #### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ####
  #### Changes for left-truncation
  
  ### Left truncated data
  km2b
  
  # male
  km2.a1 <- survfit(coxph(Surv(time=age, 
                               time2 =age.exit,
                               event = event)~1, data=subset(retire,sex=="male")), 
                    data=subset(retire,sex=="male"), type="kaplan-meier")
  
  # female
  km2.b1 <- survfit(coxph(Surv(time=age, 
                               time2 =age.exit,
                               event = event)~1, data=subset(retire,sex=="female")), 
                    data=subset(retire,sex=="female"), conf.type = "log-log")

  

  
  km2.p <- tidy(km2.a1) %>% dplyr::select(estimate,time) %>% mutate(sex="male") 
  km2.pb <- tidy(km2.b1)  %>% dplyr::select(estimate,time) %>% mutate(sex="female")
  
    km2.p <- dplyr::union(km2.p, km2.pb) %>% 
    ggplot() +
    geom_step(mapping = aes(x=time, y=estimate, color=sex))         +
    scale_y_continuous(name = "Survival Probability")                  +
    scale_x_continuous(name = "Age")                                   +
    scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
    theme_minimal()
    
  km2.p          # note: logrank test not possible for left truncated data
  
  
  ### cumulative distribution function
  
    km2.p <- dplyr::union(km2.p, km2.pb) %>% 
    ggplot() +
    geom_step(mapping = aes(x=time, y=1-estimate, color=sex))  
  

  
  rm(km2, km2.p, km2b, km2.a1, km2.b1,  km2.pb)
  
  
  ### --------------------- ###
  # 3.2.3 Education
  ### --------------------- ###
  # km3 <- survfit(SO~ESREAL, data=retire)
  # km3.p <- tidy(km3) %>% mutate(strata = revalue(strata, c("ESREAL=Illiterate"="illiterate","ESREAL=Incomplete"="incomplete",
  #                                                          "ESREAL=Primary Educ."="Primary Educ.", "ESREAL=Secondary Educ."="Secondary Educ.",
  #                                                          "ESREAL=Tertiary Educ."="Tertiary Educ."))) %>% 
  #   ggplot() +
  #   geom_step(mapping = aes(x=time, y=estimate, color=strata)) +
  #   scale_y_continuous(name = "Survival Probability")          +
  #   scale_x_continuous(name = "Age")                           +
  #   scale_color_discrete(name="")                              +
  #   theme_minimal()
  # 
  #  rm(km3, km3.p)
  ## Differences between Tertiary education and illiterates visible, the others seem to overlap quite a lot 
  
  ### ------------------------------ ###
  # 3.2.4 Pension Size (categorical)
  ### ------------------------------ ###
   # km4 <- survfit(SO~pensize, data=retire)
   # km4.p <- tidy(km4) %>% mutate(strata = revalue(strata, c("pensize=less than 500 Euro"="less than 500 Euro",
   #                                                          "pensize=500-999 Euro"="500-999 Euro",
   #                                                          "pensize=1000-1999 Euro"="1000-1999 Euro", 
   #                                                          "pensize=more than 2000"="more than 2000"))) %>% 
   #   ggplot() +
   #   geom_step(mapping = aes(x=time, y=estimate, color=strata)) +
   #   scale_y_continuous(name = "Survival Probability")          +
   #   scale_x_continuous(name = "Age")                           +
   #   scale_color_discrete(name="")                              +
   #   theme_minimal()
   # 
   # rm(km4, km4.p)
   ### that seems to be wrong at first sight, but the group of the 500 and less probably contains many women (assumed)

   
   ####### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
   
   ### accounting for left truncation
   ####### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ######
   
   ### --------------------------- ###
   # 3.2.5 sex/pensize - let´s see
   ### --------------------------- ###
   # less than 500 / male
   km4.a1 <- survfit(coxph(Surv(time=age, 
                                time2 =age.exit,
                                event = event)~1, data=subset(retire,sex=="male" & pensize=="less than 500 Euro")), 
                     data=subset(retire,sex=="male" & pensize=="less than 500 Euro"), type="kaplan-meier")
   # less than 500 / male
   km4.b1 <- survfit(coxph(Surv(time=age, 
                                time2 =age.exit,
                                event = event)~1, data=subset(retire,sex=="female" & pensize=="less than 500 Euro")), 
                     data=subset(retire,sex=="female" & pensize=="less than 500 Euro"), conf.type = "log-log")
   # 500-999 / male
   km4.a2 <- survfit(coxph(Surv(time=age, 
                                time2 =age.exit,
                                event = event)~1, data=subset(retire,sex=="male" & pensize=="500-999 Euro")), 
                     data=subset(retire,sex=="male" & pensize=="500-999 Euro"), conf.type = "log-log")
   # 500-999 / female
   km4.b2 <- survfit(coxph(Surv(time=age, 
                                time2 =age.exit,
                                event = event)~1, data=subset(retire,sex=="female" & pensize=="500-999 Euro")), 
                     data=subset(retire,sex=="female" & pensize=="500-999 Euro"), conf.type = "log-log")
   # 1000-1999 / male
   km4.a3 <- survfit(coxph(Surv(time=age, 
                                time2 =age.exit,
                                event = event)~1, data=subset(retire,sex=="male" & pensize=="1000-1999 Euro")), 
                     data=subset(retire,sex=="male" & pensize=="1000-1999 Euro"), conf.type = "log-log")
   # 1000-1999 / female
   km4.b3 <- survfit(coxph(Surv(time=age, 
                                time2 =age.exit,
                                event = event)~1, data=subset(retire,sex=="female" & pensize=="1000-1999 Euro")), 
                     data=subset(retire,sex=="female" & pensize=="1000-1999 Euro"), conf.type = "log-log")
   # more than 2000 / male
   km4.a4 <- survfit(coxph(Surv(time=age, 
                                time2 =age.exit,
                                event = event)~1, data=subset(retire,sex=="male" & pensize=="more than 2000 Euro")),
                     data=subset(retire,sex=="male" & pensize=="more than 2000 Euro"), conf.type = "log-log")
   # more than 2000 / female
   km4.b4 <- survfit(coxph(Surv(time=age, 
                                time2 =age.exit,
                                event = event)~1, data=subset(retire,sex=="female" & pensize=="more than 2000 Euro")),
                     data=subset(retire,sex=="female" & pensize=="more than 2000 Euro"), conf.type = "log-log")
   
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
   
   ##--------------------------------------
   ## A few descriptive tests to see why the women with less than 500 fluctutate so much
   
   table(retire$sex,retire$pensize)
  
   table(retire$event[retire$pensize=="less than 500 Euro"],retire$sex[retire$pensize=="less than 500 Euro"])
   

   # compare entry ... 
    retire %>% filter(pensize=="less than 500 Euro") %>% 
       ggplot(aes(x=ret.age))  +
      geom_histogram(bins=50) +
      facet_grid(. ~ sex)     +
      theme_minimal()
   
   # with actual and ... 
    retire %>% filter(pensize=="less than 500 Euro") %>% 
       ggplot(aes(x=age)) +
       geom_histogram(bins=50) +
       facet_grid(. ~ sex)
   # with exit age
    retire %>% filter(pensize=="less than 500 Euro") %>% 
       ggplot(aes(x=age.exit, fill=exit)) +
       geom_histogram(bins=50) +
       labs(fill="")+
       facet_grid(. ~ sex)
   
   # Period effects
    retire %>% filter(pensize=="less than 500 Euro") %>% 
       ggplot(aes(x=start.date)) +
       geom_histogram(bins=50) +
       facet_grid(. ~ sex)
   
   # how big is the share of early retirement entries
   round(prop.table(table(retire$ret.age.c[retire$pensize=="less than 500 Euro"])), digits = 3)
   
   ##--------------------------------------
   
   
  ### -------------------------------------------------------- ###
  # 3.2.5 sex/education - see there non parametric differences
  ### -------------------------------------------------------- ###
     # Note - I don´t quite understand why the "positive" way to write the subset comand doesn´t work
   # high (secondary, tertiary) / male
  #  km5.a1 <- survfit(Surv(time=age,
  #                         time2=age.exit,
  #                         event = event) ~ 1, data=subset(retire,sex=="male" & ESREAL!="Primary Educ." & ESREAL!="Incomplete" & ESREAL!="Illiterate"),
  #                    type="kaplan-meier", conf.type = "log-log")
  #  # high (secondary, tertiary) / female
  #  km5.b1 <- survfit(Surv(time=age,
  #                         time2=age.exit,
  #                         event = event) ~ 1, data=subset(retire,sex=="female" & ESREAL!="Primary Educ." & ESREAL!="Incomplete" & ESREAL!="Illiterate"),
  #                   conf.type = "log-log")
  #  # low / male
  #  km5.a2 <- survfit(Surv(time=age,
  #                         time2=age.exit,
  #                         event = event) ~ 1, data=subset(retire,sex=="male" & ESREAL!="Secondary Educ." & ESREAL!="Tertiary Educ."), 
  #                    conf.type = "log-log")
  #  # low / female
  #  km5.b2 <- survfit(Surv(time=age,
  #                         time2=age.exit,
  #                         event = event) ~ 1, data=subset(retire,sex=="female" & ESREAL!="Secondary Educ." & ESREAL!="Tertiary Educ."), 
  #                    conf.type = "log-log")
  #  
  # KM_ED.1 <- tidy(km5.a1) %>% dplyr::select(estimate,time) %>% mutate(edu = "high education") %>% mutate(sex="male")
  # KM_ED.2 <- tidy(km5.b1) %>% dplyr::select(estimate,time) %>% mutate(edu = "high education") %>% mutate(sex="female")
  # KM_ED.3 <- tidy(km5.a2) %>% dplyr::select(estimate,time) %>% mutate(edu = "low education") %>% mutate(sex="male")
  # KM_ED.4 <- tidy(km5.b2) %>% dplyr::select(estimate,time) %>% mutate(edu = "low education") %>% mutate(sex="female")
  # 
  # KM_EDU <- union(KM_ED.1,KM_ED.2) %>% union(KM_ED.3) %>% union(KM_ED.4) %>% ggplot() +
  #  geom_step(mapping = aes(x=time, y=estimate, color=sex, linetype=edu))  +
  #  scale_colour_manual(values = c("orange", "darkgrey"), name="")         +
  #  scale_y_continuous(name = "Survival Probability")          +
  #  scale_x_continuous(name = "Age")                           +
  #  scale_linetype_discrete(name="")                           +  
  #  theme_minimal()
  # 
  # # delete the help files
  # rm(KM_ED.1,KM_ED.2,KM_ED.3,KM_ED.4, km5.a1,  km5.a2, km5.b1,  km5.b2)
  

    
  ### ------------------------------------------------------------------------------------------------- ###  
  ### ------------------------------------------------------------------------------------------------- ###  
  ### ------------------------------------------------------------------------------------------------- ###  
  ### ------------------------------------------------------------------------------------------------- ###  
  
  
  ### 3.2. Individual level survival regression analysis
  
  
  
  ## change the reference for some categorical variables
  # retire <- within(retire, ECIVIL <- relevel(ECIVIL, ref = "married"))
  retire <- within(retire, HousReg <- relevel(HousReg, ref = "Other Form"))

  ## 3.2.1 Standard Cox Regression with only pension size and contribution and sex as main variables  
  cox.pen.1 <- coxph(Surv(time=age, 
                          time2 =age.exit,
                          event = event)~pensize, data = retire)
  

  summary(cox.pen.1)
  rm(cox.pen.1)
  
  ##                                coef  exp(coef)  se(coef)       z Pr(>|z|)    
  ##  pensize1000-1999 Euro      0.09987    1.10503   0.01828   5.464 4.66e-08 ***
  ##  pensize500-999 Euro        0.05882    1.06058   0.01739   3.382 0.000719 ***
  ##  pensizeless than 500 Euro -0.22591    0.79779   0.01963 -11.507  < 2e-16 *** 
  ## ref: 500-999 Euro
  ## Likelihood ratio test= 866.7  on 3 df,   p=0
  

  
  
  
  
  
  
  ### ------------------------------------------------------------------------------------------------- ###  

  
  ## 3.2.2 Two survival models for men and women
  # To account for the different age at death distribution as well as the different life course trajectories,
  # the hazards of dying for the two genders will estimated separately in two models
  
  ## male population
  cox.male.a <- coxph(Surv(time=age, 
                          time2 =age.exit,
                          event = event, type = "counting") ~ pensize + EDU + con.y + ret.age.c + FNAC + civilst +
                          HousReg + hh,
                          data=subset(retire, sex=="male"))
  
  ## female population
  cox.female.b <- coxph(Surv(time=age, 
                          time2 =age.exit,
                          event = event) ~ pensize + EDU + con.y + ret.age.c + FNAC + civilst +
                          HousReg + hh,
                          data=subset(retire, sex=="female"))
  
  summary(cox.male.a)
  cox.zph( cox.male.a)
  summary(cox.female.b)
  cox.zph( cox.female.b)
  
  
  
  
  
  
  ## 3.2.3 Stratified Cox Model
  # As alternative to the separate models we apply a stratified model which allows to account for the different
  # baseline mortality of the two sexes - for the sake of visibility the other model is prefered
  
  cox.strat.1 <- coxph(Surv(time=age, 
                            time2 =age.exit,
                            event = event) ~ pensize + EDU + con.y + ret.age.c + FNAC + civilst +
                            HousReg + hh + strata(sex)
                     , data=retire)
  summary(cox.strat.1)
  cox.zph(cox.strat.1)
  
  #### NEW!!! Strong income effects in both models!!! In the right direction
  
  
  
  
  
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
                           coxph(Surv(time=age, 
                                      time2 =age.exit,
                                      event = event) ~ pensize + EDU + con.y + ret.age.c + FNAC + civilst +
                                      HousReg + hh, retire)
                         })
  ret.separate
  
  ## 3.2.4 Model including interaction effects
  ret.interaction.sex <- coxph(formula = Surv(time=age, 
                                              time2 =age.exit,
                                              event = event) ~ (pensize + EDU + con.y + ret.age.c + FNAC + civilst +
                                                                  HousReg + hh)*sex - sex + strata(sex),
                               data    = retire,
                               ties    = c("efron","breslow","exact")[1])
  ret.interaction.sex
  
  ### Compare the stratified model to the interaction model - (ANOvA)
  anova(ret.interaction.sex, cox.strat.1)
  ###           loglik   Chisq Df P(>|Chi|)    
  ### Model1 -896571                        
  ### Model2 -896619 96.213 12 3.109e-15 ***
  ### This model is not statistically significantly different from the no interaction model at the 0.05 level, 
  ### thus, we conclude that the model without interaction is adequate.
  
  ### !!! The stratified model it is !!!
  
  ### ------------------------------------------------------------------------------------------------- ###
  
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
            covariate.labels=c("1000-1999  Eur/month","500-999 Eur/month","$<$ 500 Eur/month",
                               "Secondary/Tertiary Ed.","$<$ 20 y. contrib.","$>$ 40 y. contrib.",
                               "in time ret.","late ret.", "birth year (cohort)","not married",
                               "own house/apt.","couple hh"), single.row=TRUE, apply.coef = exp)
  
  # stargazer(cox.all.b, title="Cox PH Model - male population",no.space=F, 
  #           ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
  #           covariate.labels=c("1000-1999  Eur/month","500-999 Eur/month","$<$ 500 Eur/month",
  #                              "Secondary/Tertiary Ed.","$<$ 20 y. contrib.","$>$ 40 y. contrib.",
  #                              "in time ret.","late ret.", "birth year (cohort)","married",
  #                              "widowed", "divorced","own house/apt.","rent","no vehicles",
  #                              "large household","small household"), single.row=TRUE)
  
  
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
  
  
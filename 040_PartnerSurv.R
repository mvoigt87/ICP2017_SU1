### ------------------------------------------------------------------------------------------------- ###
###                     Partner Data - Descriptives, Analysis, Output                                 ###
### ------------------------------------------------------------------------------------------------- ###
#
##### 
### 1 Merging the partner data + Datamanagement + Simple statistical tests and graphical checks

### 2 Survival analysis for individuals in partnerships - (household income and widowhood effect)

### 3 Output-Tables

### ------------------------------------------------------------------------------------------------- ### 

### 0.1. Loading data set
load("030_RetirementIND.RData")
load("060_ss_cc2002.RData")

### 0.2 load necessary package
library(reshape)
library(plyr)
library(tidyverse)
library(survival)
library(forcats)
library(data.table)
library(broom)
library(stargazer)

### 1. Exploring and merging partner data

str(parned.c2002)

  # kID is the ID of an individual, who was married to an individual (their ID is kIDcon) 
  # in the ss.benefits data at 2002 and 2011. Therefore, the cleaned data of the retire data set could be
  # connected to the parned.c2002 data - keeping in mind that many variables have the same name

## 1.1. Select reasonable variables from retire and rename if necessary
cbind(colnames(retire))
retire <- subset(retire, select = c(1:7,10,11,17,25,28,30:49))

tbl_df(retire)

## 1.2. Prepare parned data 
cbind(colnames(parned.c2002))


part2 <- parned.c2002
 # add p to colnames to identify the partner variable
library(stringr)
colnames(part2)[3:15] <- str_c( colnames(part2)[3:15],"_p" )
tbl_df(part2)
  

## 1.3. Merge the two data sets - coupl
coupl <- inner_join(retire,part2, by="kID")
         ## 510653 cases

## quick check
 # civil status
 table(coupl$ECIVIL,coupl$ECIVIL_p)
 # some partner civil status do not coincide with individual - theoretically everybody should be married

 # sex
 table(coupl$sex,coupl$SEXO_p)
 ## 8 homosexual relationships
 
 # combinations with other pensioners
  id.a <- as.vector(coupl$kID)
  id.b <- as.vector(coupl$kIDcon)

  r.test <- as.data.frame(intersect(id.a,id.b))
 # 140762 individuals and their partners receive pensions at some point between 2002-2011
  
## 1.4. Create subcohort of people who both received a social security pension
  
  ## extract couples with social security pension
  colnames(r.test)[1] <- "kID"
  
  pen.coupl <- left_join(r.test,coupl, by="kID") %>% select(-CSE)
  
  ## and to also obtain knowledge about the household income, I will join the pension data from the partner
  r.small <- retire %>% select(kID,age,contrib.years, end.date, ret.pen, ret.age, ret.age.c,retire.y, event, 
                               exit, contrib.y.c, pensize, widow.y, wid.pen) %>% 
    dplyr::rename(kIDcon=kID)
  colnames(r.small)[2:14] <- str_c( colnames(r.small)[2:14],"_p" )
  tbl_df(r.small)
  
  ### combine with pencoupl
  pen.coupl <- pen.coupl %>% left_join(r.small,by="kIDcon")

    # tidy up a little :
    rm(part2,parned.c2002,ss.benefits,ss.benefits.c2002,id.a,id.b, r.test, r.small)
  
### 1.6 Datamanagement   
    
    # 1.6.1 Partner death = widowhood between 2011 and 2015 
    # - simply when the partner dies before the individual under observation (time varying for later)
    pen.coupl <- pen.coupl %>% mutate(partner.death = ifelse(event_p==1 & end.date > end.date_p,1,0)) %>% 
    ## Make a factor out of it
    mutate(p.surv = factor(ifelse(partner.death==1,"lost partner","partner alive"))) %>% 
    # 1.6.2 Calculate Household income
    # theoretically a time varying variable depending on the widowhood status
      ## for now a simplified version3
    # ! widowhood pension is still in cents (/100)
        mutate(wid.pen=wid.pen/100) %>% 
    mutate(hhincome=ifelse(partner.death == 0,ret.pen+ret.pen_p,ret.pen+wid.pen)) %>% 
    
    # 1.6.3 Edit variable number of household members
    mutate(hh= factor(ifelse(NMIEM==2, "with partner only",ifelse(NMIEM<=7, "small household","large household"))))
   
    table(pen.coupl$hh)
    pen.coupl <- within(pen.coupl, hh <- relevel(hh, ref = "small household"))
    
    hist(pen.coupl$hhincome)
    
    # Edit partnerdeath variable
    pen.coupl <- within(pen.coupl, p.surv <- relevel(p.surv, ref = "partner alive"))
    
    ## and the partner variables sex and education
    
    pen.coupl$sex_p <- as.factor(ifelse(pen.coupl$SEXO_p==1,"male","female"))
    
    pen.coupl$ESREAL_p <- as.factor(pen.coupl$ESREAL_p)
    pen.coupl$ESREAL_p <- revalue(pen.coupl$ESREAL_p, c("1"="Illiterate", "2"= "Incomplete", "3"="Primary Educ.",
                                              "4"="Secondary Educ.", "5"="Secondary Educ.", "6"="Secondary Educ.",
                                              "7"="Secondary Educ.", "8"="Tertiary Educ.", "9"="Tertiary Educ.",
                                              "10"="Tertiary Educ."))
    
    
    ## Change references for better depiction to the largest and most central group
    pen.coupl <- within(pen.coupl, ESREAL <- relevel(ESREAL, ref = "Primary Educ."))
    pen.coupl <- within(pen.coupl, ESREAL_p <- relevel(ESREAL_p, ref = "Primary Educ."))
    
    # interesting table
     # Assortative mating is important to consider when making assumptions about the household composition and income
    mix.ed <- table(pen.coupl$ESREAL,pen.coupl$ESREAL_p)
    round(prop.table(mix.ed,1), digits = 2) # row percentage
    
    ## Age difference of the partners
    
    pen.coupl <- pen.coupl %>% mutate(age.diff=age-age_p) %>% 
      ## and as categorical variable (checked for minimum value)
      mutate(age.diff.c = as.factor(ifelse(age.diff < -10, ">10 y younger",
                                           ifelse(age.diff <= - 1,"1-10 y younger",
                                           ifelse(age.diff <= 1, "same age",
                                           ifelse(age.diff <= 10,"1-10 y older",
                                           ">10 y older"))))))
                                         
    pen.coupl <- within(pen.coupl, age.diff.c <- relevel(age.diff.c, ref = "same age"))                                    
     
    summary(pen.coupl$age.diff)     # minimum value -26.20
    pen.coupl %>% ggplot(aes(x=age.diff, fill=sex)) +
      geom_histogram(bins=50)+
      scale_fill_discrete(name = "")
    
    # almost a perfect normal distribution - with men on average older than women
    
    age.d.tbl <- table(pen.coupl$age.diff.c,pen.coupl$sex)
    round(prop.table(age.d.tbl,2),digits=3)
    
    ## as expected women are much more likely to be younger than their partner
    
### ------------------------------------------------------------------------------------------------- ### 

    
    
### 2 Descriptive and Survival analysis for individuals in partnerships - (household income and widowhood effect)
    
    
## 2.1.
    
    ## ------ ##
    ##  event ##
    ## ------ ##
    
    # How do the partners deaths influence each other (of course dependent by age - just a check)
    ev.tbl <- table(pen.coupl$exit,pen.coupl$partner.death)
    round(prop.table(ev.tbl,2),2)
    chisq.test(ev.tbl)
    rm(ev.tbl)
    
    #             p.alive   p.death
    #   censored  0.91%     0.92%
    #   dead      0.09%     0.08%
    
    ## visual exploring exit by age and partner age
    pen.coupl %>% ggplot(aes(x=age,y=age_p)) +
      geom_point(aes(color=exit)) +
      scale_color_discrete(name = "") +
      facet_grid(. ~ sex)     
    
    ## and the death of the partner
    pen.coupl %>% ggplot(aes(x=age,y=age_p)) +
      geom_point(aes(color=exit_p)) +
      scale_color_discrete(name = "") +
      facet_grid(. ~ sex) 
    
    ### Visual 
    
    ## ----- ##
    ##  sex  ##
    ## ----- ##
    
    s.e.tbl <- table(pen.coupl$exit,pen.coupl$sex)
    round(prop.table(s.e.tbl,2), digits = 2)
    rm(s.e.tbl)
    
    #             female   male
    #   censored   0.96%  0.87%
    #   dead       0.04%  0.13%
    
    pen.coupl %>% ggplot(aes(x=sex,fill=exit))+
      geom_bar(stat = "count")+
      scale_fill_discrete(name = "")              
    
    # histogram of age at exit distribution
    pen.coupl %>% ggplot(aes(x=age.exit,fill=exit))+
      geom_histogram(bins=40)+
      scale_fill_discrete(name = "") +
      facet_grid(. ~ sex)                               ## better to see differences by male and female
    
    
    ## ----------- ##
    ##  education  ##
    ## ----------- ##
    
    ed.tbl <- table(pen.coupl$exit,pen.coupl$ESREAL)
    ed.tbl2 <- table(pen.coupl$exit,pen.coupl$ESREAL_p)
    round(prop.table(ed.tbl,2),2)
    round(prop.table(ed.tbl2,2),2)
    rm(ed.tbl, ed.tbl2)
    ### distributions by own education and partners education are almost the same
    
    ## ------------------ ##
    ##  pension variables ##
    ## ------------------ ##
    
    p.tbl <- table(pen.coupl$exit, pen.coupl$pensize)
    round(prop.table(p.tbl,2),2)
    rm(p.tbl)
    ## not really strong differences
    
    p.tbl2 <- table(pen.coupl$pensize,pen.coupl$pensize_p)
    round(prop.table(p.tbl2,2),2)
    rm(p.tbl2)
    
    # household pension income                                      !!! not beautiful but interesting
    pen.coupl %>% ggplot(aes(x=hhincome, fill=exit)) +
                   geom_histogram(bins = 50) +
                   scale_fill_discrete(name = "")  +
                   facet_grid(.~ sex)
    
    ## 2.2. Analysing survival in dependency of the household income and the partners/household variables
    
    ## --------------------- ##
    ## 2.2.1 Survival object ##
    ## --------------------- ##
    
    SO <- Surv(time=pen.coupl$age, 
               time2 =pen.coupl$age.exit,
               event = pen.coupl$event)
    
    ## ------------------------------------------ ##    
    # 2.2.2 Kaplan Meier with the main covariates
    ## ------------------------------------------ ##
    
    km1 <- survfit(SO~1)
    km1
    km1.b <- tidy(km1)
    km1.b %>% ggplot() +
      geom_step(mapping=aes(x=time, y=estimate)) +
      scale_y_continuous(name = "Survival Probability")                  +
      scale_x_continuous(name = "Age")
    
    rm(km1,km1.b)
    
    ## -------------------- ##    
    # 2.2.3 sex differences
    ## -------------------- ##
    
    km2 <- survfit(SO~sex, data = pen.coupl)
    km2
    km2.p <- tidy(km2) %>% mutate(strata = revalue(strata,c("sex=female"="female","sex=male"="male"))) %>%  ggplot() +
      geom_step(mapping = aes(x=time, y=estimate, color=strata))         +
      scale_y_continuous(name = "Survival Probability")                  +
      scale_x_continuous(name = "Age")                                   +
      scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
      theme_minimal()
    
    km2.p          # note: logrank test not possible for left truncated data
    rm(km2, km2.p)
    
    
    ### ------------------------------ ###
    # 2.2.4 HH income
    ### ------------------------------ ###
    summary(pen.coupl$hhincome)
    ## quickly make a categorical variable
    pen.coupl <- pen.coupl %>% mutate(hh.in.c = factor(ifelse(pen.coupl$hhincome<1000,"less than 1000 Euro",
                                                              ifelse(pen.coupl$hhincome<1500, "1000-1499 Euro",
                                                                     ifelse(pen.coupl$hhincome<2000, "1500-1999 Euro", 
                                                                            "more than 2000 Euro")))))
    ## check by event
    hh.tbl <- table(pen.coupl$exit,pen.coupl$hh.in.c)
    round(prop.table(hh.tbl,2),digits = 2)
    rm(hh.tbl)
    # good: a higher mortality in lower income groups visible
    
    ## quick visual check for the sex differences
    
    
    ## and the kme for hh income differences
    
    km4 <- survfit(SO~hh.in.c, data=pen.coupl)
    km4.p <- tidy(km4) %>% mutate(strata = revalue(strata, c("hh.in.c=less than 1000 Euro"="less than 1000 Euro",
                                                             "hh.in.c=1000-1499 Euro"="1000-1499 Euro",
                                                             "hh.in.c=1500-1999 Euro"="1500-1999 Euro", 
                                                             "hh.in.c=more than 2000 Euro"="more than 2000 Euro"))) %>% 
      ggplot() +
      geom_step(mapping = aes(x=time, y=estimate, color=strata)) +
      scale_y_continuous(name = "Survival Probability")          +
      scale_x_continuous(name = "Age")                           +
      scale_color_discrete(name="")                              +
      theme_minimal()
    
    rm(km4, km4.p)
    

    ### ------------------------------ ###
    #   2.3 Cox model
    ### ------------------------------ ###
    
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
    ### some last cleanings
    pen.coupl <- within(pen.coupl, HousReg <- relevel(HousReg, ref = "Own"))
    
    ### And for the sake of a clean output I will collapse the education categories
    
    pen.coupl <- pen.coupl %>% mutate(EDU = factor(ifelse(ESREAL!="Secondary Educ." & ESREAL!="Tertiary Educ.",
                                                          "no or low education","high education")))
    
    pen.coupl <- within(pen.coupl, EDU <- relevel(EDU, ref = "no or low education"))
    
    ## and for the partner
    pen.coupl <- pen.coupl %>% mutate(EDU_p = factor(ifelse(ESREAL_p!="Secondary Educ." & ESREAL_p!="Tertiary Educ.",
                                                          "no or low education","high education")))
    
    pen.coupl <- within(pen.coupl, EDU_p <- relevel(EDU_p, ref = "no or low education"))    
    
    round(prop.table(table(pen.coupl$ESREAL,pen.coupl$ESREAL_p),2),digits = 2)
    round(prop.table(table(pen.coupl$EDU,pen.coupl$EDU_p)),digits = 2)
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
    
    
    ## 2.3.1 HH income only
    
    cox.p1 <- coxph(Surv(time = age, time2 = age.exit, event=event) ~ hh.in.c, data = pen.coupl)
    summary(cox.p1)
     
    #                                 coef exp(coef) se(coef)       z  Pr(>|z|)    
    # hh.in.c1500-1999 Euro       -0.03175   0.96875  0.03171  -1.001     0.317    
    # hh.in.cless than 1000 Euro   0.34888   1.41749  0.02394  14.576    <2e-16 ***
    # hh.in.cmore than 2000 Euro   0.01158   1.01165  0.03147   0.368     0.713 
    
    # compared to the group with 1000-1500 Euro per month the poorer individuals have a sign. higher risk of dying
    
    
    ## 2.3.2 as full as possible
    
    ## constantly changed through testing
    cox.p2 <- coxph(Surv(time = age, 
                         time2 = age.exit, 
                         event=event) ~ hh.in.c + sex + ESREAL + ESREAL_p + contrib.y.c +
                         contrib.y.c_p + ret.age.c + FNAC + age.diff.c + p.surv +
                         HousReg + car + hh, data = pen.coupl)
    summary(cox.p2)
    
    
    #### ---------------------------------------------------------------------------------------------------- ####
    
    ## 2.3.3 stratified model (2 baselines for the two sexes)
    
    cox.p3 <- coxph(Surv(time = age, time2 = age.exit, event=event) ~ hh.in.c + EDU + EDU_p + contrib.y.c +
                      contrib.y.c_p + ret.age.c + FNAC + age.diff.c + p.surv +
                      HousReg + car + strata(sex), data = pen.coupl)
    summary(cox.p3)
    

    
    
    #### ---------------------------------------------------------------------------------------------------- ####
    
    ### Model with interaction effects
    
    ret.interaction.sex <- coxph(formula = Surv(time = age, 
                                                time2 = age.exit, 
                                                event=event) ~ (hh.in.c + ESREAL + ESREAL_p + contrib.y.c +
                                                                   contrib.y.c_p + ret.age.c + FNAC + 
                                                                   age.diff.c + p.surv +
                                                                   HousReg + car)*sex - sex + strata(sex),
                                 data    = pen.coupl,
                                 ties    = c("efron","breslow","exact")[1])
    ret.interaction.sex
    
    ### Compare the stratified model to the interaction model - (ANOvA)
    anova(ret.interaction.sex, cox.p3)
    
    #          loglik   Chisq  Df  P(>|Chi|)    
    # Model1  -108263                        
    # Model2  -108403  280.22  30  < 2.2e-16 ***
    ### Model2 is not statistically significantly different from the no interaction model at the 0.05 level
    
    
### ------------------------------------------------------------------------------------------------- ### 

### 3 Output-Tables
    
    ### Stratified Model
    
    stargazer(cox.p3, title="Stratified Cox PH model",no.space=F, 
              ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
              covariate.labels=c("1500-1999  Eur/month","$<$ 1000 Eur/month","$>$ 2000 Eur/month",
                                 "high education.","high education (partner)","26-40 y. contrib.",
                                 "$<$ 15 y. contrib.","$>$ 40 y. contrib.","26-40 y. contrib.(partner)",
                                 "$<$ 15 y. contrib.(partner)","$>$ 40 y. contrib.(partner)","in time ret.",
                                 "late ret.","in time ret.(partner)","late ret.(partner)", "birth year (cohort)",
                                 "$>$ 10 y. younger","1-10 y. younger", "1-10 y. older","$>$10 y. older" ,
                                 "other regime", "rent", "2 vehicles","$>$ 2 vehicles","no vehicles"), 
                                  single.row=TRUE,apply.coef = exp)
    
    
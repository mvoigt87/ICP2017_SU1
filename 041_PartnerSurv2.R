
##### 
### 1 Data generation and creation of potential covariates (household income etc)

### 2 Descriptive tables

### 3 Survival analysis for the married couples

### 4 Output-Tables
### ------------------------------------------------------------------------------------------------- ### 



### 0.1. Loading data set
load("031_RETIND.RData")
# ----------------------------- 
# 831231 individuals
# -----------------------------

### 0.2 load necessary package
library(reshape)
library(tidyverse)
library(survival)
library(survminer)
library(forcats)
library(data.table)
library(broom)
library(stargazer)
library(stringr)

  
  # This analysis is centered around the married couples in the retirement data set. Through access to the census 
  # information from 2011 it was possible to link the couples within Andalusia who have been married between 2001 and 2011,
  # have been cohabitating for the same time period, and where both partners received a social security active retirement
  # between 2001 and 2011. The data set will be reduced extensively but it will be possible to see the effects of household
  # income on the survival of men and women.


## For a partner variable which starts at the time of entry to the risk set 2011

cbind(colnames(retire))


### 1. Create a data set with all individuals who live in marriage with a partner who also has payed into social security

## part.R extracts individuals with a married partner in 2011 (cohabitating because of the household variables)
part.R <- retire %>% filter(!is.na(kIDcon)) %>% 
  # Extract the few who are not living together in 2011 because of different reasons
  filter(!is.na(cohab2011))
  
## 497799 individuals

## 1.1.1 Check for individuals who are also receiving a pension

id.a <- as.vector(part.R$kID)
id.b <- as.vector(part.R$kIDcon)

r.test <- as.data.frame(intersect(id.a,id.b))
 # ----------------------------- 
 # about 210.000 individuals are residing togehter and receive a social security or disability pension
 # ----------------------------- 

## 1.1.2 extract couples with social security pension
colnames(r.test)[1] <- "kID"

pen.coupl <- left_join(r.test,part.R, by="kID")

 # -- quick check
 table(pen.coupl$ECIVIL)
 table(pen.coupl$ECIVIL,pen.coupl$cohab2011)

pen.coupl <- pen.coupl %>% filter(cohab2011==T) 

 # ----------------------------- 

rm(r.test,part.R, id.a, id.b)

### 1.2. Partner variables

 # 1.2.1 Generate a copy of the data set

cbind(colnames(pen.coupl))

r.test.a <- pen.coupl %>% dplyr::select(c(1:3,11:29,33:48)) %>% 
  # delete/rename the kIDcon variable
  mutate(PID = kIDcon) %>%
  # rename the kID variable
  mutate(kIDcon=kID) %>% dplyr::select(-kID)

 # rename the variables of the partner
cbind(colnames(r.test.a))
  # add p to colnames to identify the partner variable
colnames(r.test.a)[1:20] <- str_c( colnames(r.test.a)[1:20],"_p" )
colnames(r.test.a)[22:38] <- str_c( colnames(r.test.a)[22:38],"_p" )
tbl_df(r.test.a)


### 1.3. Add the partner variables to the partner data set pen.couple by a joining/matching with kIDcon

pen.coupl <- pen.coupl %>% inner_join(r.test.a, by="kIDcon")

 # quick check
 # ---
 # PID coincides with kID (gut!)
 table(pen.coupl$SEXO,pen.coupl$SEXO_p)
 # --- a few homosexual couples (less than 1%)
 
 
### 1.4 Partner death = widowhood between 2011 and 2015 
 
 # - simply when the partner dies before the individual under observation (time varying for later)
 pen.coupl <- pen.coupl %>% mutate(partner.death = ifelse(event_p==1 & data.out > data.out_p,1,0)) %>% 
   ## Make a factor out of it
   mutate(p.surv = factor(ifelse(partner.death==1,"widowed","partner alive"))) %>% 
   # 1.4.2 Calculate Household income
   # theoretically a time varying variable depending on the widowhood status
   ## for now a simplified version
   mutate(hhincome=ifelse(partner.death == 0,INCOME+INCOME_p,INC.CW)) %>% 
   
   # 1.4.3 Edit variable number of household members
   mutate(hh= factor(ifelse(NMIEM==2, "2 person household","larger household")))
 
  # -----------------------------  
   table(pen.coupl$hh)
    #  larger household with partner only 
    #           126100              76829
    #               62%               38%

  
  # -----------------------------
  # income variable 
   hist(pen.coupl$hhincome, breaks = 30)
   summary(pen.coupl$hhincome) 
  # mean = 1395; median = 1193  => ! difference between first and third quarter = 280 Euro only
  # from the histogram it probably makes sense to look at the ones with less than 1000, 1000-1500, and more than that
  
 

## 1.4.4 Distribution invites to look at 3 groups (less than 1000, 1000-1500, and more than that)
 
 
pen.coupl <- pen.coupl %>% mutate(HHINC.3 = factor(ifelse(hhincome<1000,"less than 1000 Euro",
                                                         ifelse(hhincome<=1500,"1000-1500 Euro","more than 1500 Euro"))))
 


 
## 1.4.5 Distribution invites to look at 4 groups (less than 1000 Euro, 1000-1499 Euro, 1500-1999 Euro, more then 2000)


pen.coupl <- pen.coupl %>% mutate(HHINC.4 = factor(ifelse(hhincome<1000,"less than 1000 Euro",
                                                        ifelse(hhincome<=1500,"1000-1500 Euro",
                                                               ifelse(hhincome<=2000,"1500-2000 Euro","more than 2000 Euro")))))


# -----------------------------
# average income in numbers for the descriptive tables

DINTBL <- aggregate(pen.coupl$INC.CW,by=list(pen.coupl$SEXO),FUN=mean)

#            
#            x
#  1  female 703.72 Euro
#  2    male 817.96 Euro

# -----------------------------


## 1.4.5 Breadwinner variable - who earns at least Euro more than the partner
# 
#   pen.coupl %>% mutate(main.earner = factor(ifelse(INCOME>50+INCOME_p,"breadwinner",
#                                                  ifelse(INCOME<INCOME_p-50,"lower income","equal"))))
# 
# round(prop.table(table(pen.coupl$main.earner)),digits = 2)
# round(prop.table(table(pen.coupl$main.earner,pen.coupl$SEXO),2),digits = 2)

## For the model -  breadwinner variable
 
pen.coupl <- pen.coupl %>% mutate(bw = factor(ifelse(INCOME>200+INCOME_p,"breadwinner","less or equal income")))

                                                              
round(prop.table(table(pen.coupl$bw, pen.coupl$SEXO),2), digits=2)

### For men this effect could be interesting
                                                              
                                                              
## 1.5 Age difference of the partners
 
pen.coupl <- pen.coupl %>% mutate(age.diff=age2011-age2011_p) %>% 
   ## and as categorical variable (checked for minimum value)
   mutate(age.diff.c = as.factor(ifelse(age.diff < -10, ">10 y younger",
                                        ifelse(age.diff <= - 1,"1-10 y younger",
                                               ifelse(age.diff <= 1, "same age",
                                                      ifelse(age.diff <= 10,"1-10 y older",
                                                             ">10 y older"))))))
 
pen.coupl <- within(pen.coupl, age.diff.c <- relevel(age.diff.c, ref = "same age")) 

# -----------
 summary(pen.coupl$age.diff)     # minimum value -30.78
 
 pen.coupl %>% ggplot(aes(x=age.diff, fill=SEXO)) +
   geom_histogram(bins=50)+
   scale_fill_discrete(name = "")+
   theme_minimal()
 
##### 2. Descriptive Statistics - Overview
 
# 2.0 A factor variable for the event for easier descriptive analysis
pen.coupl <- pen.coupl %>% mutate(exit = factor(ifelse(event==0,"censored","dead"))) %>% 
  # and for the partner
  mutate(exit_p = factor(ifelse(event_p==0,"censored","death")))



 ## 2.1 Event distribution
 ## Could be different for this selective and smaller dataset
 
 round(prop.table(table(pen.coupl$event,pen.coupl$event_p),2),digits = 3) # Column Percentage
 # -----------------------------  
 #         censor_p   dead_p
 #  censor  0.880      0.796
 #  dead    0.120      0.204
 # ----------------------------- 
 chisq.test(pen.coupl$event,pen.coupl$event_p)
 
 
 ## visual exploring exit by age and partner age in 2011
 pen.coupl %>% ggplot(aes(x=age2011,y=age2011_p)) +
   geom_point(aes(color=exit)) +
   scale_color_discrete(name = "") +
   facet_grid(. ~ SEXO)     
 
 ## and the death of the partner
 pen.coupl %>% ggplot(aes(x=age2011,y=age2011_p)) +
   geom_point(aes(color=exit_p)) +
   scale_color_discrete(name = "") +
   facet_grid(. ~ SEXO) 
 
 
 ## 2.2 Sex Differences in Mortality
 round(prop.table(table(pen.coupl$exit,pen.coupl$SEXO),2),digits = 2)    ## column percentage
 # -----------------------------  
 #           female   male
 # censored   0.92%  0.81%
 # dead       0.08%  0.19%
 # -----------------------------
 
 
 ## histogram of age at exit distribution
 pen.coupl %>% ggplot(aes(x=exit.age,fill=exit))+
   geom_histogram(bins=40)+
   scale_fill_discrete(name = "") +
   facet_grid(. ~ SEXO)   
 
 
 ## 2.3. Differences in Mortality by education
 round(prop.table(table(pen.coupl$exit,pen.coupl$ESREAL5),2),2)
 round(prop.table(table(pen.coupl$exit,pen.coupl$ESREAL5_p),2),2) 
 
 
 ##### ----------------------------------------------------------------------------------------------------- #####
 
 ##### 4. Survival Analysis
 
 ### 4.1. Kaplan Meier Survival Curves as graphic tests
 
 ### 4.1.1 Global survival of married couples in Andalusia
 
 KM1 <- survfit(coxph(Surv(time=entry.age.r,
                           time2 = exit.age,
                           event=event)~1, data = pen.coupl),type="kaplan-meier")
 
 # -----------------------------  
 #   records    n.max  n.start   events   median  0.95LCL  0.95UCL 
 #  202929.0  59849.0   8423.0  26663.0     86.0     85.9     86.1
 #  median and confidence interval values are higher than for the total population
 # ----------------------------- 
 
 ## graphical display
 
 tidy(KM1) %>% ggplot(aes(x=time,y=estimate)) +
   geom_step() +
   theme_bw()
 # -----------------------------  
 # There is a leveling off at the highest ages - probably due to small case numbers 
 # -----------------------------   

 ### 4.1.2 KME by stratified by sex
 
 # males
 KM.S1 <- survfit(coxph(Surv(time = entry.age.r,
                             time2 = exit.age,
                             event = event)~1, data = subset(pen.coupl,SEXO=="male")), type = "kaplan-meier")
 # females
 KM.S2 <- survfit(coxph(Surv(time = entry.age.r,
                             time2 = exit.age,
                             event = event)~1, data = subset(pen.coupl,SEXO=="female")), type = "kaplan-meier")
 
 km.male <- tidy(KM.S1) %>% dplyr::select(time,estimate) %>% mutate(sex="male")
 km.female <- tidy(KM.S2) %>% dplyr::select(time,estimate) %>% mutate(sex="female")
 
 km.sex <- union(km.male,km.female)
 km.sex %>% ggplot(aes(x=time, y=estimate, color=sex)) +
   geom_step() +
   scale_y_continuous(name = "Survival Probability")                  +
   scale_x_continuous(name = "Age")                                   +
   xlim(65, 99)                                                       +
   scale_color_manual(values = c("orange", "darkgrey"), name="")      +
   theme_minimal()
 
 # ----------------------------- 
 # There are only a few cases at the highest ages (assumption: people who entered in their late 90s 
 # (age at 2011) were mostly living without partner at the time of their entry - so we do not follow up so many of them)
 # ----------------------------- 
 
 
 ###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ######
 ###### !!! New! Contribution years are excluded
 ###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###### 

 # --------------------  
 # Reference categories
 # --------------------
 
 # Household Income Variable
 
 # 3 income groups
 
 # pen.coupl <- within(pen.coupl, HHINC.3 <- relevel(HHINC.3, ref = "more than 1500 Euro"))
 
 pen.coupl <- within(pen.coupl, HHINC.3 <- relevel(HHINC.3, ref = "less than 1000 Euro"))
 

 
 
 # 4 income groups
 pen.coupl <- within(pen.coupl, HHINC.4 <- relevel(HHINC.4, ref = "more than 2000 Euro"))
 
 # pen.coupl <- within(pen.coupl, HHINC.4 <- relevel(HHINC.4, ref = "less than 1000 Euro"))
 
 pen.coupl <- within(pen.coupl, HHINC.4 <- relevel(HHINC.4, ref = "1000-1500 Euro"))
 
 # breadwinner variable
 pen.coupl <- within(pen.coupl, bw <- relevel(bw, ref = "less or equal income")) 
 
 # Edit partnerdeath variable
 table(pen.coupl$partner.death)
 pen.coupl <- within(pen.coupl, p.surv <- relevel(p.surv, ref = "partner alive")) 
 
 # Partner education
 pen.coupl <- within(pen.coupl, ESREAL5_p <- relevel(ESREAL5_p, ref = "No or Incomplete Educ."))
 
 #### 4.2 Cox PH Regression Models
 
 # !! Shortened code - Trial code for covariate analysis in older file
 # It can be assumed that the used covariates are tested and passed the test - others (like the room number were removed)
 
 
 ## 4.2.0.5 Models with only household income - three categories (!)
 
 cox.inc.m.3 <- coxph(Surv(time = entry.age.r,
                         time2 = exit.age,
                         event = event)~ HHINC.3, data = subset(pen.coupl,SEXO=="male"))
 
 cox.inc.f.3 <- coxph(Surv(time = entry.age.r,
                         time2 = exit.age,
                         event = event)~ HHINC.3, data = subset(pen.coupl,SEXO=="female"))
 
 summary(cox.inc.m.3)
 summary(cox.inc.f.3)
 
 ## 4.2.0.99 Models with only household income - four categories (!)
 
 cox.inc.m.4 <- coxph(Surv(time = entry.age.r,
                         time2 = exit.age,
                         event = event)~ HHINC.4, data = subset(pen.coupl,SEXO=="male"))
 
 cox.inc.f.4 <- coxph(Surv(time = entry.age.r,
                         time2 = exit.age,
                         event = event)~ HHINC.4, data = subset(pen.coupl,SEXO=="female"))
 
 summary(cox.inc.m.4)
 summary(cox.inc.f.4)
 
 
 ## 4.2.1 Stratified Model
 
 # COX.STR <- coxph(Surv(time = entry.age.r,
 #                       time2 = exit.age,
 #                       event = event)~ HHINC + DIS + ESREAL5 + FNAC +  p.surv + age.diff.c +
 #                       DIS_p + ESREAL5_p +  mobil + HousReg + hh +
 #                       strata(SEXO)
 #                       ,data = pen.coupl)
 # summary(COX.STR)
 # ggforest(COX.STR)
 
 # ----------------------------- 
 # Similar to what have been observed before, the households with the minimum income have the strongest mortality
 # disadvantage (an almost 4 times increased risk compared to the wealthiest). The effect of the loss of a partner
 # and the disappearing education effect are especially worth to highlight
 # ----------------------------- 
 
 ## 4.2.2 Comparison with and interaction model
 
 
 # part.separate <- lapply(split(retire, retire$SEXO),
 #                        FUN = function(DF) {
 #                          coxph(Surv(time=entry.age.r,
 #                                     time2=exit.age,
 #                                     event=event) ~ HHINC + DIS + ESREAL5 + FNAC +  p.surv + age.diff.c +
 #                                     DIS_p + ESREAL5_p +  mobil + HousReg + hh, pen.coupl)
 #                        })
 # part.separate
 # 
 # ## 3.2.4 Model including interaction effects
 # part.interaction.sex <- coxph(formula = Surv(time=entry.age.r,
 #                                             time2=exit.age,
 #                                             event=event) ~ (HHINC + DIS + ESREAL5 + FNAC +  p.surv + 
 #                                                               age.diff.c + DIS_p + ESREAL5_p +  
 #                                                               mobil + HousReg + hh)*SEXO - SEXO + strata(SEXO),
 #                              data    = pen.coupl,
 #                              ties    = c("efron","breslow","exact")[1])
 # part.interaction.sex
 
 # ----------------------------- 
 ### Compare the stratified model to the interaction model - (ANOvA)
 # anova(part.interaction.sex,  COX.STR)
 
 #    loglik   Chisq  Df  P(>|Chi|)    
 # 1 -258193                        
 # 2 -258385 385.28   20  < 2.2e-16 ***
 ### The stratified model is not statistically significantly different from the no interaction model at the 0.05 level, 
 ### thus, we conclude that the model without interaction is adequate.
 # -----------------------------
 
 ##### 4.3 MAIN MODELS - SEPARATED COX PH MODELS
 
 # Male population
 COX.MALE <- coxph(Surv(time = entry.age.r,
                        time2 = exit.age,
                        event = event)~ HHINC.4 + DIS + ESREAL5 + FNAC +  p.surv + age.diff.c +
                                        DIS_p + ESREAL5_p + car + HousReg + hh + bw
                        ,data = subset(pen.coupl,SEXO=="male"))
 
 # Female population
 COX.FEMALE <- coxph(Surv(time = entry.age.r,
                        time2 = exit.age,
                        event = event)~ HHINC.4 + DIS + ESREAL5 + FNAC +  p.surv + age.diff.c +
                        DIS_p + ESREAL5_p + car + HousReg + hh + bw
                   ,data = subset(pen.coupl,SEXO=="female"))
 
 
 ##
 summary(COX.MALE)
 summary(COX.FEMALE)
 
 #### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ####
            #### Test CODE ####
 
 # COX.STR.X <- coxph(Surv(time = entry.age.r,
 #                       time2 = exit.age,
 #                       event = event)~ HHINC + contrib.years + DIS + ESREAL5 + FNAC +  p.surv + age.diff.c +
 #                    contrib.years_p + DIS_p + ESREAL5_p +  mobil + HousReg + hh + pensize + pensize_p +
 #                    strata(SEXO),
 #                    data = pen.coupl)
 
 
 # COX.MALE.X  <- coxph(Surv(time = entry.age.r,
 #                                    time2 = exit.age,
 #                                    event = event)~ HHINC + contrib.years + DIS + ESREAL5 + FNAC +  p.surv + age.diff.c +
 #                                 contrib.years_p + DIS_p + ESREAL5_p +  mobil + HousReg + hh + pensize + pensize_p, 
 #                                 data = subset(pen.coupl,SEXO=="male"))
 # 
 # COX.FEMALE.X <- coxph(Surv(time = entry.age.r,
 #                          time2 = exit.age,
 #                          event = event)~ HHINC + contrib.years + DIS + ESREAL5 + FNAC +  p.surv + age.diff.c +
 #                       contrib.years_p + DIS_p + ESREAL5_p +  mobil + HousReg + hh + pensize + pensize_p, 
 #                       data = subset(pen.coupl,SEXO=="female"))
 # 
 # summary(COX.MALE.X)
 # summary(COX.FEMALE.X)
 
 # -----------------------------
 # Drastic Risk reduction dependent on the partners pension income for males and females, while the household income
 # has a highly significant and expected (gradient)
 # -----------------------------
 
 

 #### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ####
 
 
 
 ##### 5. Model Output
 
 ## 5.1. just hh income
 stargazer(cox.inc.m,cox.inc.f, title="Cox PH Model -  Household Analysis",no.space=F, 
           ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
           covariate.labels=c("HH inc. 1000-2000 Euro/month","HH inc. $<$ 1000 Euro/month"),
           single.row=TRUE, apply.coef = exp)
 
 
 ## 5.2. Full Model
 stargazer(COX.MALE,COX.FEMALE, title="Cox PH Model -  Household Analysis",no.space=F, 
           ci=TRUE, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Relative mortality risk"),
           covariate.labels=c("HH inc. 1000-2000 Euro/month","HH inc. $<$ 1000 Euro/month", "Received Disability Pension", 
                              "Tertiary Educ.","Secondary Educ.", "Primary Educ.", "Birth Cohort", "Lost Partner", "$>$10 years older",
                              "$>$ 10 years younger", "1-10 years older", "1-10 years younger", "Disability partner", 
                              "Tertiary Educ. Partner","Secondary Educ. Partner", "Primary Educ. Partner", "No Cars Available", "Own House/Aptm.",
                              "Rent House/Aptm.", "Lives only with Partner", "Breadwinner"),
           single.row=TRUE, apply.coef = exp)
 
 
 #### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ####
 

 
 
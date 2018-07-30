### Only the primary survival models ###
### -------------------------------- ###


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

##### 1. Individual level survival regression analysis


## change the reference for some categorical variables
retire$HousReg <- as.factor(as.character(retire$HousReg))
retire <- within(retire, HousReg <- relevel(HousReg, ref = "owned"))
retire <- within(retire, pensize <- relevel(pensize, ref = "more than 2000 Euro"))
# retire <- within(retire, pensize <- relevel(pensize, ref = "650-999 Euro"))
# retire <- within(retire, pensize.3 <- relevel(pensize.3, ref = "more than 1200 Euro"))
retire <- within(retire, pensize.3 <- relevel(pensize.3, ref = "600-1199 Euro"))


## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##


### 2. Two survival models for men and women
# To account for the different age at death distribution as well as the different life course trajectories,
# the hazards of dying for the two genders will estimated separately in two models

## male population
## ---------------

# Model with only income
cox.male.a <- coxph(Surv(time=entry.age.r,
                         time2=exit.age,
                         event=event) ~ pensize,
                    data=subset(retire, SEXO=="male"))

# Income as continuous measure
cox.male.a.c <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ INCOME,
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


### 3. Two survival models for men and women (3 categories)
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

## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##

### 4. Models with Income as continuous variable

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
  
  


## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  
  
### 5. Parametric survival model
  
  # Adult mortality follows a Gompertz distribution
  
  pm1 <- flexsurvreg(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize, data = subset(retire, SEXO="male"),
                          dist = "gompertz")
  
  pm2 <- flexsurvreg(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ INCOME, data = subset(retire, SEXO="male"),
                     dist = "gompertz")
  
  
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  ## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  
  
  #### PARTNER DATA 
  
  ## Run the 041 code first!!!
  
  load("041_RETPART.RData")
  
  
  # compare income with log(income)
  # https://onlinelibrary.wiley.com/doi/full/10.1111/j.1468-0084.2008.00531.x
  
  par(mfrow=c(1,2))  
  hist(pen.coupl$hhincome)
  hist(log(pen.coupl$hhincome))
  par(mfrow=c(1,1)) 
  
  
  ### ------------------
  ### Male population
  ### ------------------
  
  ## 1) Solo income
  COX.MALE.A <- coxph(Surv(time = entry.age.r,
                           time2 = exit.age,
                           event = event)~ HHINC.3, data = subset(pen.coupl, SEXO=="male"))
  
  ## 1b) Log income
  COX.MALE.A.C <- coxph(Surv(time = entry.age.r,
                           time2 = exit.age,
                           event = event)~ log(hhincome), data = subset(pen.coupl, SEXO=="male"))
  
  ## 2) Parametric survival model 
  GOMP.MALE.A <- flexsurvreg(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ HHINC.3, data = subset(pen.coupl, SEXO="male"),
                      dist = "gompertz")
  
  GOMP.MALE.A
  
  ## 2) Parametric survival model -  log income
  GOMP.MALE.A.C <- flexsurvreg(Surv(time=entry.age.r,
                                  time2=exit.age,
                                  event=event) ~ log(hhincome), data = subset(pen.coupl, SEXO="male"),
                             dist = "gompertz")
  
  GOMP.MALE.A.C
  
  # survival curve
  plot(GOMP.MALE.A.C, xlim=c(65,100))

  # Compare models
  AIC(COX.MALE.A)-AIC(COX.MALE.A.C)
    
  anova(COX.MALE.A, COX.MALE.A.C)
  
  AIC(COX.MALE.A) - AIC(GOMP.MALE.A)
  
  AIC(COX.MALE.A.C) - AIC(GOMP.MALE.A.C)
  
  
  ### Looks like Gompertz model with log income provides the best fit to the data (or in AIC language lowest information loss)
  
  #####
  #####
  #####

    
  ## Wealth Variables + Direct Variables (Disability or death of the partner not included)
  COX.MALE.B <- coxph(Surv(time = entry.age.r,
                           time2 = exit.age,
                           event = event)~ HHINC.3 + ESREAL5 + mobil +  HousReg, 
                      data = subset(pen.coupl,SEXO=="male"))
  
  
  
  ## 3) Full Model
  ##    ----------
  COX.MALE.C <- coxph(Surv(time = entry.age.r,
                           time2 = exit.age,
                           event = event)~ HHINC.3 + ESREAL5 + mobil  +  HousReg +  p.surv + DIS + FNAC +
                        DIS_p + ESREAL5_p + hijo + bw, 
                      data = subset(pen.coupl,SEXO=="male"))

  
  ## 3b) Full model with log income
  COX.MALE.C.C <- coxph(Surv(time = entry.age.r,
                           time2 = exit.age,
                           event = event)~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  p.surv + DIS + FNAC +
                        DIS_p + ESREAL5_p + hijo + bw, 
                      data = subset(pen.coupl,SEXO=="male"))
  
  ### Model comparison
  
  AIC(COX.MALE.C)    # 352484.6
  AIC(COX.MALE.C.C)  # 352170.9
  
  anova(COX.MALE.C,COX.MALE.C.C) # Model with log income is preferable
  
  ## -------------------------------------------------------------------- ##
  # Now get baseline curve
  baseline <- basehaz(COX.MALE.C.C)
  # Draw baseline hazard
  plot(baseline$time, baseline$hazard, type='l',main="Hazard rates")
  # Draw disabled peoples hazard (i.e.) -  multiplicative to the baseline (coefficients)
  lines(baseline$time, exp(0.408486)*baseline$hazard, col="blue")
  ## -------------------------------------------------------------------- ## 
  
  ### 3c) Parametric model with categories
  
  GOMP.MALE.C <- flexsurvreg(Surv(time=entry.age.r,
                                  time2=exit.age,
                                  event=event) ~ HHINC.3 + ESREAL5 + mobil  +  HousReg +  p.surv + DIS + FNAC +
                               DIS_p + ESREAL5_p + hijo + bw, data = subset(pen.coupl, SEXO="male"),
                             dist = "gompertz")
  
  GOMP.MALE.C
  
  
  ## 3d) Parametric survival model with log(income)  - there seem to be some kind of problem with the log income
  GOMP.MALE.C.C <- flexsurvreg(Surv(time=entry.age.r,
                                  time2=exit.age,
                                  event=event) ~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  p.surv + DIS + FNAC +
                               DIS_p + ESREAL5_p + hijo + bw, data = subset(pen.coupl, SEXO="male"),
                             dist = "gompertz")
  
  GOMP.MALE.C.C
 
  
  #####
  #####
  #####
  
  
  ### ------------------
  ### Female population
  ### ------------------
  
  ## Solo income
  COX.FEMALE.A <- coxph(Surv(time = entry.age.r,
                             time2 = exit.age,
                             event = event)~ HHINC.3, data = subset(pen.coupl,SEXO=="female"))
  ## 1b) Log income
  COX.FEMALE.A.C <- coxph(Surv(time = entry.age.r,
                             time2 = exit.age,
                             event = event)~ log(hhincome), data = subset(pen.coupl, SEXO=="female"))
  
  ## 2) Parametric survival model 
  GOMP.FEMALE.A <- flexsurvreg(Surv(time=entry.age.r,
                                  time2=exit.age,
                                  event=event) ~ HHINC.3, data = subset(pen.coupl, SEXO="female"),
                             dist = "gompertz")
  
  GOMP.FEMALE.A
  
  ## 2) Parametric survival model -  log income
  GOMP.FEMALE.A.C <- flexsurvreg(Surv(time=entry.age.r,
                                    time2=exit.age,
                                    event=event) ~ log(hhincome), data = subset(pen.coupl, SEXO="female"),
                               dist = "gompertz")
  
  GOMP.FEMALE.A.C
  
  
  
  # Compare models
  AIC(COX.FEMALE.A)-AIC(COX.FEMALE.A.C)
  
  anova(COX.FEMALE.A, COX.FEMALE.A.C)
  
  # log income preferable
  
  AIC(COX.FEMALE.A) - AIC(GOMP.FEMALE.A)
  
  AIC(COX.FEMALE.A.C) - AIC(GOMP.FEMALE.A.C)
  
  # Gompertz Model preferable
  
  
  ## Wealth variables
  COX.FEMALE.B <- coxph(Surv(time = entry.age.r,
                             time2 = exit.age,
                             event = event)~ HHINC.3 + ESREAL5 + mobil +  HousReg,
                        data = subset(pen.coupl,SEXO=="female"))
  
  ## 3) Full Model
  
  ## 3a) Full Model with categories
  
  COX.FEMALE.C <- coxph(Surv(time = entry.age.r,
                             time2 = exit.age,
                             event = event)~ HHINC.3 + ESREAL5 + mobil  +  HousReg +  p.surv + DIS + FNAC+
                          DIS_p + ESREAL5_p + hijo + bw, 
                        data = subset(pen.coupl,SEXO=="female"))
  
  
  COX.FEMALE.C.C <- coxph(Surv(time = entry.age.r,
                             time2 = exit.age,
                             event = event)~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  p.surv + DIS + FNAC+
                          DIS_p + ESREAL5_p + hijo + bw, 
                        data = subset(pen.coupl,SEXO=="female"))
  
  anova(COX.FEMALE.C,COX.FEMALE.C.C)
  AIC(COX.FEMALE.C)    # 139364.3
  AIC(COX.FEMALE.C.C)  # 139174.8
  
  ## Model with log income seems to be  preferable
  
  
  ### 3c) Parametric model with categories
  
  GOMP.FEMALE.C <- flexsurvreg(Surv(time=entry.age.r,
                                  time2=exit.age,
                                  event=event) ~ HHINC.3 + ESREAL5 + mobil  +  HousReg +  p.surv + DIS + FNAC +
                               DIS_p + ESREAL5_p + hijo + bw, data = subset(pen.coupl, SEXO="female"),
                             dist = "gompertz")
  
  GOMP.FEMALE.C
  
  
  ## 3d) Parametric survival model with log(income)  - there seem to be some kind of problem with the log income
  GOMP.FEMALE.C.C <- flexsurvreg(Surv(time=entry.age.r,
                                    time2=exit.age,
                                    event=event) ~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  p.surv + DIS + FNAC +
                                 DIS_p + ESREAL5_p + hijo + bw, data = subset(pen.coupl, SEXO="female"),
                               dist = "gompertz")
  
  GOMP.FEMALE.C.C
  
  
  ### OUTPUT
   
  ## -----------------
  summary(COX.MALE.A)
  summary(COX.MALE.B)
  summary(COX.MALE.C)
  
  summary(COX.FEMALE.A)
  summary(COX.FEMALE.B)
  summary(COX.FEMALE.C)

  
  
  
  
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


  # Income distribution
  retire %>% dplyr::mutate(grp.mean = ifelse(SEXO=="female",479.64,765.69)) %>% 
    ggplot(aes(x=income_Retirement, color=SEXO)) +
    geom_histogram(fill="white", alpha=0.5, position="dodge",binwidth = 50) +
    geom_vline(aes(xintercept=grp.mean, color=SEXO),
               linetype="dashed") +
    scale_color_brewer(palette="Dark2", name=" ") +
    scale_x_continuous(name="Montly Public Pension Income (in €)", limits = c(1,3500)) +
    scale_y_continuous(name = " ") +
    theme_bw()
  
  


## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
  
  
### 5. Parametric survival model
  
  # Adult mortality follows a Gompertz distribution
  
  pm1 <- flexsurvreg(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize, data = subset(retire, SEXO="male"),
                          dist = "gompertz")

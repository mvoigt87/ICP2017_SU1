### Models we decided to use in the submission ###
### ------------------------------------------ ###


### 0.1. Loading data sets
load("031_RETIND.RData")
load("041_RETPART.RData")

# ----------------------------- 
# 637345 individuals
# -----------------------------

### 0.2 load necessary package
library(reshape)
library(tidyverse)
library(survival)
## to explore the survival data
library("survminer")
library(forcats)
library(data.table)
library(broom)
library(stargazer)
# Use for a parametric model
library(flexsurv)


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


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################


ret.men <- subset(retire, SEXO="male")
ret.women <- subset(retire, SEXO="female")


GOMP.MALE.R <- flexsurvreg(Surv(time=entry.age.r,
                              time2=exit.age,
                              event=event) ~ log(INCOME) + ESREAL5 + mobil  +  HousReg + DIS,
                              data = ret.men,
                         dist = "gompertz")

coef(GOMP.MALE.R)


GOMP.FEMALE.R <- flexsurvreg(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event) ~ log(INCOME) + ESREAL5 + mobil  +  HousReg +  DIS,
                                data = ret.women,
                           dist = "gompertz")

coef(GOMP.FEMALE.R)





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

pen.men <- subset(pen.coupl, SEXO="male")
pen.women <- subset(pen.coupl, SEXO="female")


GOMP.MALE <- flexsurvreg(Surv(time=entry.age.r,
                              time2=exit.age,
                              event=event) ~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  p.surv + DIS +
                           DIS_p + ESREAL5_p + hijo + bw, data = pen.men,
                         dist = "gompertz")

coef(GOMP.MALE)


GOMP.FEMALE <- flexsurvreg(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event) ~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  p.surv + DIS +
                                DIS_p + ESREAL5_p + hijo + bw, data = pen.women,
                           dist = "gompertz")

coef(GOMP.FEMALE)


### Model X

GOMP <- flexsurvreg(Surv(time=entry.age.r,
                              time2=exit.age,
                              event=event) ~ log(hhincome) + SEXO + ESREAL5 + mobil + HousReg +  p.surv + DIS +
                              log(FNAC) + DIS_p + ESREAL5_p + hijo + bw, data = pen.coupl,
                         dist = "gompertz")

coef(GOMP)



###--------------------------------------------------------------------------------------------------------###
# different baselines
###--------------------------------------------------------------------------------------------------------###

# function for the hazard rate
haz.Gompertz <- function(x, shape, rate) {
  hgompertz(x, shape = shape, rate = rate)
}


x <- Cox.SURVFIT.TIME                ## to make it the same time scale

# males
shape.m <- GOMP.MALE$coefficients[1]
rate.m <- 0.000000191                   ## something went wrong with the rate (always negative)
# females
shape.m <- GOMP.FEMALE$coefficients[1]
rate.m <- 0.000000191                   ## something went wrong with the rate (always negative)


haz.Gomp <- as.vector(haz.Gompertz(x=x, shape = shape, rate = rate))
  
  
  
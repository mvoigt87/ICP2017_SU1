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
library(lmtest)

### Reference Changes for the final models

### ---------------------------------------------------------------------------------------------------------- ###
  ## change the reference for some categorical variables
  retire$HousReg <- as.factor(as.character(retire$HousReg))
  retire <- within(retire, hijo <- relevel(as.factor(hijo), ref = "Single (with children)"))
  retire <- within(retire, HousReg <- relevel(HousReg, ref = "owned"))
  retire <- within(retire, pensize <- relevel(pensize, ref = "more than 2000 Euro"))
  # retire <- within(retire, pensize <- relevel(pensize, ref = "650-999 Euro"))
  # retire <- within(retire, pensize.3 <- relevel(pensize.3, ref = "more than 1200 Euro"))
  retire <- within(retire, pensize.3 <- relevel(pensize.3, ref = "600-1199 Euro"))

  # --------------------  
  # Reference categories
  # --------------------
  
  # Household Income Variable
  
  # 3 income groups
  
  pen.coupl <- within(pen.coupl, HHINC.3 <- relevel(HHINC.3, ref = "more than 1500 Euro"))
  
  # pen.coupl <- within(pen.coupl, HHINC.3 <- relevel(HHINC.3, ref = "less than 1000 Euro"))
  
  
  # 4 income groups
  pen.coupl <- within(pen.coupl, HHINC.4 <- relevel(HHINC.4, ref = "more than 2000 Euro"))
  
  # pen.coupl <- within(pen.coupl, HHINC.4 <- relevel(HHINC.4, ref = "less than 1000 Euro"))
  
  # pen.coupl <- within(pen.coupl, HHINC.4 <- relevel(HHINC.4, ref = "1000-1500 Euro"))
  
  # education variable
  pen.coupl <- within(pen.coupl, ESREAL5 <- relevel(ESREAL5, ref = "Tertiary Educ.")) 
  
  # Partner education
  pen.coupl <- within(pen.coupl, ESREAL5_p <- relevel(ESREAL5_p, ref = "No or Incomplete Educ."))
  
  # education collapsed variable - 3 categories
  pen.coupl <- within(pen.coupl, ESREAL3 <- relevel(ESREAL3, ref = "Secondary or higher Educ."))
  
  # Same for the partner variable
  pen.coupl <- within(pen.coupl, ESREAL3_p <- relevel(ESREAL3_p, ref = "Secondary or higher Educ."))  
  
  # breadwinner variable
  pen.coupl <- within(pen.coupl, bw <- relevel(bw, ref = "less or equal income")) 
  
  # Edit partnerdeath variable
  pen.coupl <- within(pen.coupl, p.surv <- relevel(p.surv, ref = "partner alive")) 
  
  #  variable hijo (only partner)
  pen.coupl <- within(pen.coupl, hijo <- relevel(as.factor(hijo), ref = "Only Partner")) 
  
  # Household ownership variable
  pen.coupl$HousReg <- as.factor(pen.coupl$HousReg)
  pen.coupl <- within(pen.coupl, HousReg <- relevel(HousReg, ref = "owned")) 

### ---------------------------------------------------------------------------------------------------------- ###


### ---------
### Cox Final
### ---------


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

pen.men <- subset(pen.coupl, SEXO="male")
pen.women <- subset(pen.coupl, SEXO="female")


GOMP.MALE <- flexsurvreg(Surv(time=entry.age.r,
                              time2=exit.age,
                              event=event) ~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  p.surv + DIS +
                           DIS_p + ESREAL5_p + hijo + bw, data = pen.men,
                         dist = "gompertz")

GOMP.MALE


GOMP.FEMALE <- flexsurvreg(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event) ~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  p.surv + DIS +
                                DIS_p + ESREAL5_p + hijo + bw, data = pen.women,
                           dist = "gompertz")

GOMP.FEMALE

#####################################################################################################################
#####################################################################################################################

### Model X - decision between the two models

GOMP.log <- flexsurvreg(Surv(time=entry.age.r,
                              time2=exit.age,
                              event=event) ~ log(hhincome) + SEXO + ESREAL5 + mobil + HousReg +  event_p + DIS +
                              log(FNAC) + DIS_p + ESREAL5_p + hijo + bw, data = pen.coupl,
                         dist = "gompertz")

GOMP.log ## Other than in the model test code, this model seems to be superior (although they look the same)


GOMP.3cat <- flexsurvreg(Surv(time=entry.age.r,
                              time2=exit.age,
                              event=event) ~ pensize.3 + SEXO + ESREAL5 + mobil + HousReg +  event_p + DIS +
                           log(FNAC) + DIS_p + ESREAL5_p + hijo + bw,
                         data = pen.coupl,
                         dist = "gompertz")

GOMP.3cat


AIC(GOMP.log)
AIC(GOMP.3cat)

2*(GOMP.3cat$loglik - GOMP.log$loglik)
qchisq(.95, df=1)




###--------------------------------------------------------------------------------------------------------###
# different baselines
###--------------------------------------------------------------------------------------------------------###

# function for the hazard rate
haz.Gompertz <- function(x, shape, rate) {
  hgompertz(x, shape = shape, rate = rate)
}


x <- seq(65,100,0.01)             ## to make it the same time scale
# parameters
shape.m <- GOMP$coefficients[1]
rate.m <- 0.193                   ## something went wrong with the rate (always negative)


haz.Gomp <- as.vector(haz.Gompertz(x=x, shape = shape.m, rate = rate.m))
  
  
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
## --------------------------
## Model X - with retire data
## --------------------------

GOMP.log <- flexsurvreg(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ log(INCOME) + SEXO + ESREAL5 + mobil + HousReg + DIS + hijo,
                      data = retire,
                      dist = "gompertz")



GOMP.3cat <- flexsurvreg(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize.3 + SEXO + ESREAL5 + mobil + HousReg + DIS + hijo,
                      data = retire,
                      dist = "gompertz")


GOMP.4cat <- flexsurvreg(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize + SEXO + ESREAL5 + mobil + HousReg + DIS + hijo,
                      data = retire,
                      dist = "gompertz")


# survival curve - 3 cats
par(mfrow=c(1,2))
plot(GOMP.3cat, xlim=c(65,100))
legend("topright",legend=c("KME","Gompertz Curve"), 
       lty=c(1,1),col=c("black","red"), cex=0.75)
plot(GOMP.log, xlim=c(65,100))
legend("topright",legend=c("KME","Gompertz Curve"), 
       lty=c(1,1),col=c("black","red"), cex=0.75)
par(mfrow=c(1,1))

#######################################################################################################################  
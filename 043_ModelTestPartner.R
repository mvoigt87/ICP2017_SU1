#### PARTNER DATA 


load("041_RETPART.RData")


# pen.coupl <- within(pen.coupl, HHINC.3 <- relevel(HHINC.3, ref = "more than 1500 Euro"))
# # pen.coupl <- within(pen.coupl, HHINC.3 <- relevel(HHINC.3, ref = "less than 1000 Euro"))
# 
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
pen.coupl <- within(pen.coupl, p.surv <- relevel(p.surv, ref = "widowed"))

#  variable hijo (only partner)
pen.coupl <- within(pen.coupl, hijo <- relevel(as.factor(hijo), ref = "Only Partner"))

# Household ownership variable
pen.coupl$HousReg <- as.factor(pen.coupl$HousReg)
pen.coupl <- within(pen.coupl, HousReg <- relevel(HousReg, ref = "owned"))
# compare income with log(income)
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.1468-0084.2008.00531.x

par(mfrow=c(1,2))  
hist(pen.coupl$hhincome)
hist(log(pen.coupl$hhincome))
par(mfrow=c(1,1)) 




## Wealth Variables + Direct Variables (Disability or death of the partner not included)

## A) 3 categories
COX.MALE.A <- coxph(Surv(time = entry.age.r,
                         time2 = exit.age,
                         event = event)~ HHINC.3, 
                    data = subset(pen.coupl,SEXO=="male"))


## B) 4 categories
COX.MALE.B <- coxph(Surv(time = entry.age.r,
                         time2 = exit.age,
                         event = event)~ HHINC.4, 
                    data = subset(pen.coupl,SEXO=="male"))


## C) Full model with log income
COX.MALE.C <- coxph(Surv(time = entry.age.r,
                         time2 = exit.age,
                         event = event)~ log(hhincome), 
                    data = subset(pen.coupl,SEXO=="male"))

### Model comparison
AIC(COX.MALE.A)    # 358492.1
AIC(COX.MALE.B)    # 358493
AIC(COX.MALE.C)    # 358469.2

anova(COX.MALE.A,COX.MALE.B) # NO significant differences
anova(COX.MALE.A,COX.MALE.C) # Model with log income is preferable

## -------------------------------------------------------------------- ##
# Now get baseline curve
baseline <- basehaz(COX.MALE.C)
# Draw baseline hazard
plot(baseline$time, baseline$hazard, type='l',main="Hazard rates")
# Draw disabled peoples hazard (i.e.) -  multiplicative to the baseline (coefficients)
lines(baseline$time, exp(-0.6238)*baseline$hazard, col="blue")
legend("topleft",legend=c("baseline","income effect"), 
       lty=c(1,1),col=c("black","blue"), cex=0.75)
## -------------------------------------------------------------------- ## 




### ----------------------
### Just income - Gompertz
### ----------------------

## 1) 3 categories
GOMP.INC.A <- flexsurvreg(Surv(time=entry.age.r,
                               time2=exit.age,
                               event=event) ~ HHINC.3, data = pen.coupl,
                          dist = "gompertz")

## 1b) 4 categories
GOMP.INC.B <-flexsurvreg(Surv(time=entry.age.r,
                              time2=exit.age,
                              event=event) ~ HHINC.4, data = pen.coupl,
                         dist = "gompertz")

## 2) Log Income 
GOMP.INC.C <- flexsurvreg(Surv(time=entry.age.r,
                               time2=exit.age,
                               event=event) ~ log(hhincome), data = pen.coupl,
                          dist = "gompertz")


# AIC model test

AIC <- matrix(ncol = 2, nrow = 3)

AIC[1,2] <- AIC(GOMP.INC.A)
AIC[2,2] <- AIC(GOMP.INC.B)
AIC[3,2] <- AIC(GOMP.INC.C)
Income <- c("3 categories", "4 categories","log income")
colnames(AIC) <- c("Distribution", "AIC")
AIC <- transform(AIC, Distribution = as.character(Income), 
                 AIC = as.factor(AIC))
#### clean up AIC
AIC$AIC <- as.numeric(levels(AIC$AIC)[AIC$AIC])
AIC[order(AIC$AIC),]                                     ### Log income as preferable


# survival curve
plot(GOMP.INC.C, xlim=c(65,100))
legend("topright",legend=c("KME","Gompertz Curve"), 
       lty=c(1,1),col=c("black","red"), cex=0.75)


##### ------------------------------------------------------------------------------------------------ #####
##### ------------------------------------------------------------------------------------------------ #####
##### ------------------------------------------------------------------------------------------------ #####


#### 1) Parametric survival model with log(income)  - there seem to be some kind of problem with the log income

### ----------------
### Male population
### ----------------
GOMP.MALE.A <- flexsurvreg(Surv(time=entry.age.r,
                                time2=exit.age,
                                event=event) ~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  event_p + DIS + log(FNAC) +
                             DIS_p + ESREAL5_p + hijo + bw, data = subset(pen.coupl, SEXO="male"),
                           dist = "gompertz")

GOMP.MALE.A

### ------------------
### Female population
### ------------------

GOMP.FEMALE.A <- flexsurvreg(Surv(time=entry.age.r,
                                  time2=exit.age,
                                  event=event) ~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  event_p + DIS + log(FNAC) +
                               DIS_p + ESREAL5_p + hijo + bw, data = subset(pen.coupl, SEXO="female"),
                             dist = "gompertz")

GOMP.FEMALE.A

### Same shape and rate for both sexes




## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 3a) Full Model with categories
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### !!!!!! Without p.surv

# log income
# ----------
GOMP.log <- flexsurvreg(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ log(hhincome) + SEXO + ESREAL5 + mobil + HousReg + DIS + event_p +
                          log(FNAC) + DIS_p + ESREAL5_p + hijo + bw, data = pen.coupl,
                        dist = "gompertz")

GOMP.log

# 4 categories
# -------------
GOMP.c4 <- flexsurvreg(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ HHINC.4 + SEXO + ESREAL5 + mobil + HousReg + DIS + event_p +
                         log(FNAC) + DIS_p + ESREAL5_p + hijo + bw, data = pen.coupl,
                       dist = "gompertz")

GOMP.c4

# 3 categories
# -------------

GOMP.c3 <- flexsurvreg(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ HHINC.3 + SEXO + ESREAL5 + mobil + HousReg + DIS + event_p +
                         log(FNAC) + DIS_p + ESREAL5_p + hijo + bw, data = pen.coupl,
                       dist = "gompertz")

GOMP.c3

# AIC model test

AIC <- matrix(ncol = 2, nrow = 3)

AIC[1,2] <- AIC(GOMP.c3)
AIC[2,2] <- AIC(GOMP.c4)
AIC[3,2] <- AIC(GOMP.log)

Income <- c("3 categories", "4 categories", "log income")
colnames(AIC) <- c("Distribution", "AIC")
AIC <- transform(AIC, Distribution = as.character(Income), 
                 AIC = as.factor(AIC))
#### clean up AIC
AIC$AIC <- as.numeric(levels(AIC$AIC)[AIC$AIC])
AIC[order(AIC$AIC),]                                        ## 3 categories! seems to be the best fitting model


# survival curve - 3 cats
par(mfrow=c(1,2))
plot(GOMP.c3, xlim=c(65,100), main="3 categories")
legend("topright",legend=c("KME","Gompertz Curve"), 
       lty=c(1,1),col=c("black","red"), cex=0.75)
plot(GOMP.log, xlim=c(65,100), main="log income")
legend("topright",legend=c("KME","Gompertz Curve"), 
       lty=c(1,1),col=c("black","red"), cex=0.75)
par(mfrow=c(1,1))

### Once more compare to the Cox model

## C) Full model with log income
COX.ULTIMO <- coxph(Surv(time = entry.age.r,
                         time2 = exit.age,
                         event = event)~ HHINC.3 + SEXO + ESREAL5 + mobil + HousReg + event_p + DIS +
                      log(FNAC) + DIS_p + ESREAL5_p + hijo + bw, data = pen.coupl)

AIC(COX.ULTIMO)
# 524697
AIC(GOMP.c3)
# 209614.5

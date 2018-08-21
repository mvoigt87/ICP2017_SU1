#### ------------------------------------------------------- ####
####         Model comparison and got testing                ####               
#### ------------------------------------------------------- ####
### 1  Testing different covariate combination and stratifications

### 2  Testing different income variables

### 3  Testing survival models

### 4  Find the best-fitting model

### ------------------------------------------------------------------------------------------------- ### 

rm(list = ls())

### 0.1. Loading data set
# load("031_RETIND.RData")
# load("041_RETPART.RData")
load("031b_RETIND.RData")

# ----------------------------- 

### 0.2 load necessary package
library(reshape)
library(tidyverse)
library(survival)
## to explore the survival data
library(forcats)
library(data.table)
library(broom)
library(stargazer)
# Use for a parametric model
library(flexsurv)
# for the cool plots
library(survminer)
# for checking for different distributions
library(fitdistrplus)

##### Individual level survival regression analysis

## change the reference for some categorical variables
retire.A$HousReg <- as.factor(as.character(retire.A$HousReg))
retire.A <- within(retire.A, HousReg <- relevel(HousReg, ref = "owned"))
retire.A <- within(retire.A, ESREAL3 <- relevel(ESREAL3, ref = "Secondary or higher Educ."))
retire.A <- within(retire.A, pensize <- relevel(pensize, ref = "more than 2000 Euro"))
# retire.A <- within(retire.A, pensize <- relevel(pensize, ref = "650-999 Euro"))
retire.A <- within(retire.A, pensize.3 <- relevel(pensize.3, ref = "more than 1200 Euro"))
# retire.A <- within(retire.A, pensize.3 <- relevel(pensize.3, ref = "600-1199 Euro"))
retire.A <- within(retire.A, hh <- relevel(as.factor(hh), ref = "single"))
retire.A <- within(retire.A, civil.status <- relevel(as.factor(civil.status), ref = "other forms"))

## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##

## 1.2. Covariate Cocktail
## -----------------------


# Model with only income
cox.a <- coxph(Surv(time=entry.age,
                         time2=exit.age,
                         event= event)~ pensize.3 + strata(SEXO),
                    data=retire.A)


# Adding the other wealth variables
cox.b <- coxph(Surv(time=entry.age,
                         time2=exit.age,
                         event=event) ~ pensize.3 + ESREAL3 + mobil + HousReg + strata(SEXO),
                    data=retire.A)

# Adding contextual variables
cox.c <- coxph(Surv(time=entry.age,
                         time2=exit.age,
                         event=event) ~ pensize.3 + ESREAL3 + mobil + HousReg + start.date + #DIS + 
                                        FNAC + civil.status + hh + strata(SEXO),
                    data=retire.A)
# model comparison
anova(cox.a, cox.b)   ## LR CHisq (df=4) = 993.11 ***
anova(cox.b, cox.c)   ## LR CHisq (df=7) = 1055.5 ***

rm(cox.a, cox.b, cox.c)

## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## 1.3 Stratified Cox Model
# As alternative to the separate models we apply a stratified model which allows to account for the different
# baseline mortality of the two sexes - for the sake of visibility the other model is prefered

cox.strat.1 <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + # DIS + 
                          FNAC + civil.status + hh + strata(SEXO)
                     , data=retire.A)
summary(cox.strat.1)
# cox.zph(cox.strat.1)

## 1.4 Model including interaction effects
ret.interaction.sex <- coxph(formula = Surv(time=entry.age,
                                            time2=exit.age,
                                            event=event) ~ (pensize.3 + ESREAL5 + mobil + HousReg + # DIS + 
                                                              FNAC + civil.status + hh)*SEXO - SEXO + strata(SEXO),
                             data    = retire.A,
                             ties    = c("efron","breslow","exact")[1])
ret.interaction.sex

### Compare the stratified model to the interaction model - (ANOvA)
anova(ret.interaction.sex, cox.strat.1)
###           loglik   Chisq Df P(>|Chi|)    
### Model1  -1470072                           
### Model2  -1470159  130.88 11 < 2.2e-16 ***
### This model is not statistically significantly different from the no interaction model at the 0.05 level, 
### thus, we conclude that the model without interaction is adequate.

## !!! The stratified model it is !!! - to better show the differences two separate models were chosen

## 3.2.3 Separate models for females and males 
## (Code based on Kleinbaum: http://rstudio-pubs-static.s3.amazonaws.com/5096_0880aaaf0df94f3b8533a1c024738246.html)
# 
# ret.separate <- lapply(split(retire, retire$SEXO),
#                        FUN = function(DF) {
#                          coxph(Surv(time=entry.age.r,
#                                     time2=exit.age,
#                                     event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
#                                  FNAC + DIS + civil.status + hh, retire)
#                        })
# ret.separate

## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##

                    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
                    ### COX MODEL with different income measures ###
                    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###

## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##


## male population
## ---------------

# Model with 4 categories
cox.male.a.a <- coxph(Surv(time=entry.age,
                         time2=exit.age,
                         event=event) ~ pensize,
                    data=subset(retire.A, SEXO=="male"))

# Model with 3 categories
cox.male.a.b <- coxph(Surv(time=entry.age,
                         time2=exit.age,
                         event=event) ~ pensize.3,
                    data=subset(retire.A, SEXO=="male"))

# Income as continuous measure
cox.male.a.c <- coxph(Surv(time=entry.age,
                           time2=exit.age,
                           event=event) ~ INCOME,
                      data=subset(retire.A, SEXO=="male"))

# LOG Income as continuous measure
cox.male.a.d <- coxph(Surv(time=entry.age,
                           time2=exit.age,
                           event=event) ~ log(INCOME),
                      data=subset(retire.A, SEXO=="male"))


# LR tests

anova(cox.male.a.a, cox.male.a.b)   ## LR CHisq (df=2) = 96.6 ***
anova(cox.male.a.b, cox.male.a.c)   ## LR CHisq (df=1) = 32.747 ***
anova(cox.male.a.b, cox.male.a.d)   ## LR CHisq (df=0) = 4.277 ***  ### simple LR Test prefers model cox.male.a.d (LOG income)!

### For discussion on income measures i.e.: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1468-0084.2008.00531.x 


## female population
## ---------------

# Model with only income (3 cats)
cox.female.a <- coxph(Surv(time=entry.age,
                           time2=exit.age,
                           event=event) ~ pensize.3,
                      data=subset(retire.A, SEXO=="female"))
  
# Model with only income (4 cats)
cox.female.b <- coxph(Surv(time=entry.age,
                             time2=exit.age,
                             event=event) ~ pensize,
                        data=subset(retire.A, SEXO=="female"))

# Income as continuous measure
cox.female.c <- coxph(Surv(time=entry.age,
                           time2=exit.age,
                           event=event) ~ INCOME,
                      data=subset(retire.A, SEXO=="female"))

# LOG Income as continuous measure
cox.female.d <- coxph(Surv(time=entry.age,
                           time2=exit.age,
                           event=event) ~ log(INCOME),
                      data=subset(retire.A, SEXO=="female"))


### Model testing for females

## LRT
anova(cox.female.a, cox.female.b)   ## LR CHisq (df=1) = 9.2999 *** (model a with 4 cats)
anova(cox.female.b, cox.female.c)   ## LR CHisq (df=2) = 0.02378 * (INCOME)
anova(cox.female.c, cox.female.d)   ## LR CHisq (df=0) = 3.2953 *** (model d with log income)

## AIC
AIC <- matrix(ncol = 2, nrow = 4)
AIC[1,2] <- AIC(cox.female.a)
AIC[2,2] <- AIC(cox.female.b)
AIC[3,2] <- AIC(cox.female.c)
AIC[4,2] <- AIC(cox.female.d)
Income <- c("3 Categories", "4 Categories", "Continuous", "Log")
colnames(AIC) <- c("Distribution", "AIC")
AIC <- transform(AIC, Distribution = as.character(Income), 
                 AIC = as.factor(AIC))
#### clean up AIC
AIC$AIC <- as.numeric(levels(AIC$AIC)[AIC$AIC])
AIC[order(AIC$AIC),]
## AIC seems to highlight the model with 4 categories

### Full model comparison
### ---------------------

## Males (4 cats)
cox.male.fa <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                          FNAC + age.ret + civil.status + hh,
                     data=subset(retire.A, SEXO=="male"))
## Males (3 cats)
cox.male.fb <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                       FNAC + age.ret + civil.status + hh,
                     data=subset(retire.A, SEXO=="male"))
## Males (continuous)
cox.male.fc <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ INCOME + ESREAL5 + mobil + HousReg + 
                       FNAC + age.ret + civil.status + hh,
                     data=subset(retire.A, SEXO=="male"))
## Males (log)
cox.male.fd <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ log(INCOME) + ESREAL5 + mobil + HousReg + 
                       FNAC + age.ret + civil.status + hh,
                     data=subset(retire.A, SEXO=="male"))

anova(cox.male.fa, cox.male.fb) # LR Chisquare 130.14  (df=1) *** (model fb with 3 categories)
anova(cox.male.fb, cox.male.fc) # LR Chisquare 151.55  (df=1) *** (model fc with continuous income)
anova(cox.male.fc, cox.male.fd) # LR Chisquare 0.7189  (df=0) *** (model fd with log income)        !!!


## Females (4 cats)
cox.female.fa <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                       FNAC + age.ret + civil.status + hh,
                     data=subset(retire.A, SEXO=="female"))
## Females (3 cats)
cox.female.fb <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                       FNAC + age.ret + civil.status + hh,
                     data=subset(retire.A, SEXO=="female"))
## Females (continuous)
cox.female.fc <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ INCOME + ESREAL5 + mobil + HousReg + 
                       FNAC + age.ret + civil.status + hh,
                     data=subset(retire.A, SEXO=="female"))
## Females (log)
cox.female.fd <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ log(INCOME) + ESREAL5 + mobil + HousReg + 
                       FNAC + age.ret + civil.status + hh,
                     data=subset(retire.A, SEXO=="female"))

anova(cox.female.fa, cox.female.fb) # LR Chisquare 7.724 (df=1) ** (model fb with 3 categories)
anova(cox.female.fb, cox.female.fc) # LR Chisquare 9.07  (df=1) ** (model fc with continuous income)
anova(cox.female.fc, cox.female.fd) # LR Chisquare 0.3681  (df=0) *** (model fd with log income) !!!



## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##

### Further model and assumption test

mm <- cox.zph(cox.male.fd)
ff <- cox.zph(cox.female.fd)

# Proportional Hazards Assumption - pensionsize variables
ggcoxzph(mm,resid=T, se=T, var=c(1:3), caption = "Schoenfeld Residuals by time",
         ggtheme = theme_minimal(),font.main = 12)
# assumption is only hardly met by the group who receives 1000-1999 Euro per month

# Proportional Hazards Assumption - rest
# ggcoxzph(mm,resid=T, se=T, var=c(4:13), caption = "Schoenfeld Residuals by time",
#          ggtheme = theme_minimal(),font.main = 12) 
# assumption is not met for the secondary and primary education group and the partner variable

# residual check
ggcoxdiagnostics(cox.male.fd,type = "schoenfeld")
ggcoxdiagnostics(cox.female.fd,type = "schoenfeld")

##--- female population   
# Proportional Hazards Assumption
ggcoxzph(ff,resid=T, se=T, var = c(1:3), caption = "Schoenfeld Residuals by time",
         ggtheme = theme_minimal(),font.main = 12)

# PHA is not met in the analysis of pension size for women

ggcoxzph(ff,resid=T, se=T, var = c(4:13), caption = "Schoenfeld Residuals by time",
         ggtheme = theme_minimal(),font.main = 12)
# PHA is met for secondary education, no car, and the civil statuses


## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##



### 5. Comparison with parametric survival models


### 5.1. What parametric distribution would fit the best
  
AIC <- matrix(ncol = 2, nrow = 3)

# exponential
model.1 <- flexsurvreg(Surv(time=retire.A$entry.age,
                          time2=retire.A$exit.age,
                          event=retire.A$event) ~ 1, dist = "exp")

AIC[1,2] <- AIC(model.1)

# Weibull - some kind of error with this one
# model.2 <- flexsurvreg(Surv(time=entry.age.r,
#                             time2=exit.age,
#                             event=event) ~ 1, dist = "weibull")
# 
# AIC[2,2] <- AIC(model.2)

# lognormal
model.3 <- flexsurvreg(Surv(time=retire.A$entry.age,
                            time2=retire.A$exit.age,
                            event=retire.A$event) ~ 1, dist = "lnorm")

AIC[2,2] <- AIC(model.3)

# Gamma - same weird error
# model.4 <- flexsurvreg(Surv(time=entry.age.r,
#                             time2=exit.age,
#                             event=event) ~ 1, dist = "gamma")
# 
# AIC[4,2] <- AIC(model.4)

# Gompertz
model.5 <- flexsurvreg(Surv(time=retire.A$entry.age,
                            time2=retire.A$exit.age,
                            event=retire.A$event) ~ 1, dist = "gompertz")

AIC[3,2] <- AIC(model.5)

Dist <- c("exp", "lnorm", "gompertz")
colnames(AIC) <- c("Distribution", "AIC")
AIC <- transform(AIC, Distribution = as.character(Dist), 
                 AIC = as.factor(AIC))
#### clean up AIC
AIC$AIC <- as.numeric(levels(AIC$AIC)[AIC$AIC])
AIC[order(AIC$AIC),]

      # Distribution     AIC
      # 3     gompertz 1031670
      # 2        lnorm 1038268
      # 1          exp 1135792

      ### Adult mortality follows a Gompertz distribution ###


### ----------------------------------------------- ###

# plot the fit compared to KME

plot(model.5, ylab="Survival probability", 
     xlab="Age", main = "Gompertz Survival Plot", xlim=c(65,101))
legend("topright",legend=c("KM Plot","Fitted"), 
       lty=c(1,1),col=c("black","red"), cex=0.75)

# from the paper on the flexsurv package
# https://cran.r-project.org/web/packages/flexsurv/vignettes/flexsurv.pdf

    # median.Gompertz <- function(shape, rate) {
    #   qgompertz(0.5, shape = shape, rate = rate)
    # }
    # summary(model.5, fn = median.Gompertz, t = 1, B = 10000)


### Compare Cox with Parametric model with income (categorical) as only covariates (male)
### --------------------------------------------------------------------------------------

# 1 a) men (individual, only income (cat.), COX)
cox.male.1 <- coxph(Surv(time=entry.age,
                          time2=exit.age,
                          event=event) ~ pensize.3,data=subset(retire.A, SEXO=="male"))

summary(cox.male.1)

        ### ----------------------------------------------------------
        ### Proportional hazards
        ph.test <- cox.zph(cox.male.1)
        
        plot(ph.test[1], main = "pensize.3")
        ### ----------------------------------------------------------

# -----------------------------------------------                
# 1 b) men (individual, only income (cat.), Gomp)
# -----------------------------------------------
Gomp.male.1 <- flexsurvreg(Surv(time=entry.age,
                                 time2=exit.age,
                                 event=event) ~ pensize.3, data = subset(retire.A, SEXO="male"),
                            dist = "gompertz")

### AIC
### ---
AIC(cox.male.1)
AIC(Gomp.male.1)

# comparing the models visually
inc_df <- with(retire.A,
               data.frame(pensize.3 = c("less than 600 Euro", "600-1199 Euro", "more than 1200 Euro")))

pred.cox.g <- survfit(cox.male.1, newdata = inc_df)

pred.cox.g <- tidy(pred.cox.g) 

## Go a little around to get the three categories
pred.cox.g1 <- pred.cox.g %>% dplyr::select(estimate.1, conf.high.1, conf.low.1, time) %>% mutate(estimate=estimate.1) %>% 
                mutate(income="less than 600 Euro") %>% mutate(conf.high=conf.high.1) %>% mutate(conf.low=conf.low.1) %>% 
                mutate(model="Cox") %>% dplyr::select(-estimate.1,-conf.high.1,-conf.low.1)
pred.cox.g2 <- pred.cox.g %>% dplyr::select(estimate.2, conf.high.2, conf.low.2, time) %>% mutate(estimate=estimate.2) %>% mutate(conf.high=conf.high.2) %>% 
                mutate(conf.low=conf.low.2) %>% mutate(income="600-1199 Euro") %>% mutate(model="Cox") %>% 
               dplyr::select(-estimate.2,-conf.high.2,-conf.low.2)
pred.cox.g3 <- pred.cox.g %>% dplyr::select(estimate.3, conf.high.3, conf.low.3, time) %>% mutate(estimate=estimate.3) %>% mutate(conf.high=conf.high.3) %>% 
               mutate(conf.low=conf.low.3) %>% mutate(income="more than 1200 Euro") %>% mutate(model="Cox") %>% 
               dplyr::select(-estimate.3,-conf.high.3,-conf.low.3)

pred.cox.g <- union(pred.cox.g1,pred.cox.g2) %>% union(pred.cox.g3)



# extracting information from gompertz model
# ------------------------------------------
est.Gomp.1 <- data.frame(summary(Gomp.male.1))
  
# group < 600
est.Gomp.g1 <- est.Gomp.1 %>% dplyr::select(pensize.3.less.than.600.Euro.est, pensize.3.less.than.600.Euro.lcl, 
                                            pensize.3.less.than.600.Euro.ucl, pensize.3.less.than.600.Euro.time) %>%    
                                mutate(estimate=pensize.3.less.than.600.Euro.est) %>% 
                                mutate(income="less than 600 Euro") %>% 
                                mutate(conf.high=pensize.3.less.than.600.Euro.ucl) %>% 
                                mutate(conf.low=pensize.3.less.than.600.Euro.lcl) %>% 
                                mutate(time=pensize.3.less.than.600.Euro.time) %>% 
                                mutate(model="Gompertz") %>% 
                                dplyr::select(-pensize.3.less.than.600.Euro.est,-pensize.3.less.than.600.Euro.lcl,
                                             -pensize.3.less.than.600.Euro.ucl,-pensize.3.less.than.600.Euro.time)
  

# group 600-1200
est.Gomp.g2 <- est.Gomp.1 %>% dplyr::select(pensize.3.600.1199.Euro.est, pensize.3.600.1199.Euro.lcl, 
                                            pensize.3.600.1199.Euro.ucl, pensize.3.600.1199.Euro.time) %>%    
  mutate(estimate=pensize.3.600.1199.Euro.est) %>% 
  mutate(income="600-1199 Euro") %>% 
  mutate(conf.high=pensize.3.600.1199.Euro.ucl) %>% 
  mutate(conf.low=pensize.3.600.1199.Euro.lcl) %>% 
  mutate(time=pensize.3.600.1199.Euro.time) %>% 
  mutate(model="Gompertz") %>% 
  dplyr::select(-pensize.3.600.1199.Euro.est,-pensize.3.600.1199.Euro.lcl,
                -pensize.3.600.1199.Euro.ucl,-pensize.3.600.1199.Euro.time)

# group > 1200
est.Gomp.g3 <- est.Gomp.1 %>% dplyr::select(pensize.3.more.than.1200.Euro.est, pensize.3.more.than.1200.Euro.lcl, 
                                            pensize.3.more.than.1200.Euro.ucl, pensize.3.more.than.1200.Euro.time) %>%    
  mutate(estimate=pensize.3.more.than.1200.Euro.est) %>% 
  mutate(income="more than 1200 Euro") %>% 
  mutate(conf.high=pensize.3.more.than.1200.Euro.ucl) %>% 
  mutate(conf.low=pensize.3.more.than.1200.Euro.lcl) %>% 
  mutate(time=pensize.3.more.than.1200.Euro.time) %>% 
  mutate(model="Gompertz") %>% 
  dplyr::select(-pensize.3.more.than.1200.Euro.est,-pensize.3.more.than.1200.Euro.lcl,
                -pensize.3.more.than.1200.Euro.ucl,-pensize.3.more.than.1200.Euro.time)



# combine the models
comp.plot <- dplyr::union(pred.cox.g,est.Gomp.g1) %>% union(est.Gomp.g2) %>% union(est.Gomp.g3) 

# plot fit
# --------
comp.plot %>% ggplot() +
  geom_step(mapping = aes(x=time, y=estimate, color=model, linetype=income), size = 1.15)         +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age")                                   +
  scale_colour_manual(values = c("orange","chartreuse"), name="")    +
  scale_linetype_manual(values=c("longdash", "dotted", "solid"), name="")+
  theme_minimal()


### ------------ ###
### Hazard rate  ###
### ------------ ###

# Survfit for the coxph object  ##

Cox.SURVFIT <- survfit(cox.male.1)

# Proportion that fail at each evaluated time period  ##

Cox.SURVFIT.PROP.FAIL <-
  Cox.SURVFIT$n.event/Cox.SURVFIT$n.risk					

# Length of time over which these failure occurs  ##

Cox.SURVFIT.TIME <- Cox.SURVFIT$time

Cox.SURVFIT.TIME.0 <-
  c(0,Cox.SURVFIT.TIME[-length(Cox.SURVFIT.TIME)])

# Instantaneous Hazard rate  ##

Cox.SURVFIT.INSTANTANEOUS.HAZARD <-
  Cox.SURVFIT.PROP.FAIL/(Cox.SURVFIT.TIME -
                                   Cox.SURVFIT.TIME.0)


##########################################################################################

# GOMPERTZ - estimated hazard (shape and rate from the model output, x - time)

haz.Gompertz <- function(x, shape, rate) {
  hgompertz(x, shape = shape, rate = rate)
}

# give parameters and times
x <- Cox.SURVFIT.TIME                ## to make it the same time scale
shape <- Gomp.male.1$coefficients[1]
rate <- 0.00000385                   ## something went wrong with the rate (always negative)

haz.Gomp <- as.vector(haz.Gompertz(x=x, shape = shape, rate = rate))


plot.df.cox <- as.data.frame(cbind(Cox.SURVFIT.TIME,Cox.SURVFIT.INSTANTANEOUS.HAZARD)) %>% 
            mutate(time=Cox.SURVFIT.TIME) %>% mutate(haz=Cox.SURVFIT.INSTANTANEOUS.HAZARD) %>% mutate(model= "Cox") %>%
            dplyr::select(-Cox.SURVFIT.TIME,-Cox.SURVFIT.INSTANTANEOUS.HAZARD)
plot.df.cox %>% ggplot() +
  geom_line(aes(x=time, y=haz))


plot.df.gomp <- as.data.frame(cbind(Cox.SURVFIT.TIME,haz.Gomp)) %>% mutate(haz=haz.Gomp) %>% 
                mutate(time=Cox.SURVFIT.TIME) %>% mutate(model= "Gompertz") %>% 
                dplyr::select(-Cox.SURVFIT.TIME,-haz.Gomp)

plot.df.gomp %>% ggplot() +
  geom_line(aes(x=time, y=haz))



######################################################################################################################
######################################################################################################################

### Models with categorical all covariates (male)
### ----------------------------------------------

# 3 a) men (individual, full model, Cox)
cox.male.3a <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                        FNAC + DIS + civil.status + hh,
                        data=subset(retire, SEXO=="male"))

# 2 b) men (individual, full model, Gomp)
Gomp.male.3b <- flexsurvreg(Surv(time=entry.age.r,
                                 time2=exit.age,
                                 event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                              FNAC + DIS + civil.status + hh, data = subset(retire, SEXO="male"),
                            dist = "gompertz")

anova(cox.male.3a,Gomp.male.3b)
AIC(cox.male.3a)
AIC(Gomp.male.3b)


### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###

pm0 <- flexsurvreg(Surv(time=entry.age,
                        time2=exit.age,
                        event=event) ~ 1, data = subset(retire.A, SEXO="male"),
                   dist = "gompertz")

pm1 <- flexsurvreg(Surv(time=entry.age,
                        time2=exit.age,
                        event=event) ~ pensize, data = subset(retire.A, SEXO="male"),
                   dist = "gompertz")

pm2 <- flexsurvreg(Surv(time=entry.age,
                        time2=exit.age,
                        event=event) ~ INCOME, data = subset(retire.A, SEXO="male"),
                   dist = "gompertz")


# ----------------
# Plot differences
# ----------------

## see: https://github.com/kassambara/survminer/issues/67 - One better approach is to predict survival for all individuals in the cohort,
## and then take the average of the predicted curves by groups of interest (for example, sex, age group, etc.)

#### 1. Models without covariates (individual data, males)

# predicted survival curves from Cox model without covariates

pred.cox <- survfit(Surv(time=entry.age,
                         time2=exit.age,
                         event=event) ~ 1, data = subset(retire.A, SEXO="male"))

plot(x=pred.cox$time, y=pred.cox$surv, type="l",lty = 1, lwd = 1)

# adding fitted values for the Gompertz
lines(pm0, type = "survival",lty = 1, lwd = 1, col.ci = NULL)



#### 2. Models with income as categorical variable (individual data, males)

pred.cox.inc <- survfit(Surv(time=entry.age,
                             time2=exit.age,
                             event=event) ~ pensize.3 , data = subset(retire.A, SEXO="male"))
pci <- tidy(pred.cox.inc)


plot.x <- pci  %>%  ggplot(aes(x=time,y=estimate,color=strata)) + geom_step() + theme_bw() + scale_color_discrete(name="")

plot.x + theme(legend.position = c(0.25, 0.15))


###### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

### add estimates from the parametric model 

#### IDEA: Add them to the other dataframe first and then use the model type as linetype

dat.pm1 <- data.frame(summary(pm1))


# base plot

plot(x=pred.cox.inc$time, y=pred.cox.inc$surv, col=pred.cox.inc$strata, type="l", lty = 1, lwd = 1)

# adding fitted values for the Gompertz
lines(pm1, type = "survival",lty = 2, lwd = 1)
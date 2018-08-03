#### ------------------------------------------------------- ####
####         Model comparison and got testing                ####               
#### ------------------------------------------------------- ####

### 0.1. Loading data set
load("031_RETIND.RData")

# ----------------------------- 
# 637345 individuals
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


                    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
                    ### COX MODEL with different income measures ###
                    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###

## male population
## ---------------

# Model with only income
cox.male.a <- coxph(Surv(time=entry.age.r,
                         time2=exit.age,
                         event=event) ~ pensize,
                    data=subset(retire, SEXO=="male"))

# Income as continuous measure
cox.male.a.b <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ INCOME,
                      data=subset(retire, SEXO=="male"))

# LOG Income as continuous measure
cox.male.a.c <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ log(INCOME),
                      data=subset(retire, SEXO=="male"))


# LR tests

anova(cox.male.a, cox.male.a.b)   ## LR CHisq (df=2) = 96.6 ***
anova(cox.male.a, cox.male.a.c)   ## LR CHisq (df=2) = 102.34 ***
anova(cox.male.a.b, cox.male.a.c) ## LR CHisq (df=0) = 5.7209 ***  ### simple LR Test prefers this model!

### For discussion on income measures i.e.: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1468-0084.2008.00531.x 


# FULL Model (contextual and other ses variables)
cox.male.c <- coxph(Surv(time=entry.age.r,
                         time2=exit.age,
                         event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                                        FNAC + DIS + civil.status + hh,
                    data=subset(retire, SEXO=="male"))


## female population
## ---------------

# Model with only income (3 cats)
cox.female.a <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize.3,
                      data=subset(retire, SEXO=="female"))
  
# Model with only income (4 cats)
cox.female.b <- coxph(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ pensize,
                        data=subset(retire, SEXO=="female"))

# Income as continuous measure
cox.female.c <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ INCOME,
                      data=subset(retire, SEXO=="female"))

# LOG Income as continuous measure
cox.female.d <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ log(INCOME),
                      data=subset(retire, SEXO=="female"))


### Model testing for females

## LRT
anova(cox.female.a, cox.female.b)   ## LR CHisq (df=1) = 16.635 *** (model a with 4 cats)
anova(cox.female.a, cox.female.c)   ## LR CHisq (df=2) = 2.3786
anova(cox.female.a, cox.female.d)   ## LR CHisq (df=1) = 7.0205 ** (model d with log income)

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

## Males (4 cats)
cox.male.fa <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                          FNAC + DIS + civil.status + hh,
                     data=subset(retire, SEXO=="male"))
## Males (3 cats)
cox.male.fb <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                       FNAC + DIS + civil.status + hh,
                     data=subset(retire, SEXO=="male"))
## Males (continuous)
cox.male.fc <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ INCOME + ESREAL5 + mobil + HousReg + 
                       FNAC + DIS + civil.status + hh,
                     data=subset(retire, SEXO=="male"))
## Males (log)
cox.male.fd <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ log(INCOME) + ESREAL5 + mobil + HousReg + 
                       FNAC + DIS + civil.status + hh,
                     data=subset(retire, SEXO=="male"))

anova(cox.male.fa, cox.male.fb) # LR Chisquare 44.686  (df=1) *** (model fb with 3 categories)
anova(cox.male.fb, cox.male.fc) # LR Chisquare 83.117  (df=1) *** (model fc with continuous income)
anova(cox.male.fc, cox.male.fd) # LR Chisquare 0.7254  (df=0) *** (model fd with log income)


## Females (4 cats)
cox.female.fa <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                       FNAC + DIS + civil.status + hh,
                     data=subset(retire, SEXO=="female"))
## Females (3 cats)
cox.female.fb <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize.3 + ESREAL5 + mobil + HousReg + 
                       FNAC + DIS + civil.status + hh,
                     data=subset(retire, SEXO=="female"))
## Females (continuous)
cox.female.fc <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ INCOME + ESREAL5 + mobil + HousReg + 
                       FNAC + DIS + civil.status + hh,
                     data=subset(retire, SEXO=="female"))
## Females (log)
cox.female.fd <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ log(INCOME) + ESREAL5 + mobil + HousReg + 
                       FNAC + DIS + civil.status + hh,
                     data=subset(retire, SEXO=="female"))

anova(cox.female.fa, cox.female.fb) # LR Chisquare 40.024 (df=1) *** (model fb with 3 categories)
anova(cox.female.fb, cox.female.fc) # LR Chisquare 38.817  (df=1) *** (model fc with continuous income)
anova(cox.female.fc, cox.female.fd) # LR Chisquare 11.99  (df=0) *** (model fd with log income) !!!


## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##


### 5. Comparison with parametric survival models


### 5.1. What parametric distribution would fit the best
  
AIC <- matrix(ncol = 2, nrow = 3)

# exponential
model.1 <- flexsurvreg(Surv(time=retire$entry.age.r,
                          time2=retire$exit.age,
                          event=retire$event) ~ 1, dist = "exp")

AIC[1,2] <- AIC(model.1)

# Weibull - some kind of error with this one
# model.2 <- flexsurvreg(Surv(time=entry.age.r,
#                             time2=exit.age,
#                             event=event) ~ 1, dist = "weibull")
# 
# AIC[2,2] <- AIC(model.2)

# lognormal
model.3 <- flexsurvreg(Surv(time=retire$entry.age.r,
                            time2=retire$exit.age,
                            event=retire$event) ~ 1, dist = "lnorm")

AIC[2,2] <- AIC(model.3)

# Gamma - same weird error
# model.4 <- flexsurvreg(Surv(time=entry.age.r,
#                             time2=exit.age,
#                             event=event) ~ 1, dist = "gamma")
# 
# AIC[4,2] <- AIC(model.4)

# Gompertz
model.5 <- flexsurvreg(Surv(time=retire$entry.age.r,
                            time2=retire$exit.age,
                            event=retire$event) ~ 1, dist = "gompertz")

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

median.Gompertz <- function(shape, rate) {
  qgompertz(0.5, shape = shape, rate = rate)
}
summary(model.5, fn = median.Gompertz, t = 1, B = 10000)


### Compare Cox with Parametric model with income (categorical) as only covariates (male)
### --------------------------------------------------------------------------------------

# 1 a) men (individual, only income (cat.), COX)
cox.male.1 <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize.3,data=subset(retire, SEXO=="male"))

summary(cox.male.1)

        ### ----------------------------------------------------------
        ### Proportional hazards
        ph.test <- cox.zph(cox.male.1)
        
        plot(ph.test[1], main = "pensize.3")
        ### ----------------------------------------------------------

# -----------------------------------------------                
# 1 b) men (individual, only income (cat.), Gomp)
# -----------------------------------------------
Gomp.male.1 <- flexsurvreg(Surv(time=entry.age.r,
                                 time2=exit.age,
                                 event=event) ~ pensize.3, data = subset(retire, SEXO="male"),
                            dist = "gompertz")

### AIC
### ---
AIC(cox.male.1)
AIC(Gomp.male.1)

# comparing the models visually
inc_df <- with(retire,
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

##########################################################################################

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

# To solve later

# plot.df <- union(plot.df.cox, plot.df.gomp)
# 
# plot.df %>% ggplot() +
#             geom_line(aes(x=time, y=haz, color= model))






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

pm0 <- flexsurvreg(Surv(time=entry.age.r,
                        time2=exit.age,
                        event=event) ~ 1, data = subset(retire, SEXO="male"),
                   dist = "gompertz")

pm1 <- flexsurvreg(Surv(time=entry.age.r,
                        time2=exit.age,
                        event=event) ~ pensize, data = subset(retire, SEXO="male"),
                   dist = "gompertz")

pm2 <- flexsurvreg(Surv(time=entry.age.r,
                        time2=exit.age,
                        event=event) ~ INCOME, data = subset(retire, SEXO="male"),
                   dist = "gompertz")


# ----------------
# Plot differences
# ----------------

## see: https://github.com/kassambara/survminer/issues/67 - One better approach is to predict survival for all individuals in the cohort,
## and then take the average of the predicted curves by groups of interest (for example, sex, age group, etc.)

#### 1. Models without covariates (individual data, males)

# predicted survival curves from Cox model without covariates

pred.cox <- survfit(Surv(time=entry.age.r,
                         time2=exit.age,
                         event=event) ~ 1, data = subset(retire, SEXO="male"))

plot(x=pred.cox$time, y=pred.cox$surv, type="l",lty = 1, lwd = 1)

# adding fitted values for the Gompertz
lines(pm0, type = "survival",lty = 1, lwd = 1, col.ci = NULL)



#### 2. Models with income as categorical variable (individual data, males)

pred.cox.inc <- survfit(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ pensize.3 , data = subset(retire, SEXO="male"))
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







# # Different way
#
#   Cox.cat.plot <- ggsurvplot(survfit(cox.male.3a, data = subset(retire, SEXO="male")), palette = "Dark2", break.time.by=5, xlim=c(65, 100),
#   ggtheme = theme_minimal())
# 
# Cox.cat.plot

# library(ggfortify)
# 
# CMS <- survfit(coxph(Surv(time=entry.age.r,
#            time2=exit.age,
#            event=event) ~ pensize.3,
#       data=subset(retire, SEXO=="male")))
# 
# Cox.Pl.sin <- autoplot(CMS, surv.linetype = 'dashed', surv.colour = 'blue',
#          conf.int.fill = 'dodgerblue3', conf.int.alpha = 0.5, censor = FALSE)


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


#####
##### Flus and famines
##### ----------------

rm(list = ls())

### 0.1 Load data set
 # load("010_sscc.RData")

#load("data/025_INDMOR-CT.RData")
#load("../TesisMathias/DatosRegistroAndalucia/IO/exploratory.analysis/010_sscc.RData")

# str(sscc)
# head(sscc)
# summary(sscc$FNAC)
## 1420924 observations = pension spells of any kind (retirement, disability, widowhood)
## Oldest individual 113, the youngest 9

load("datosandalucia/010_ma.RData")
load("datosandalucia/010_sma.RData")


## 0.2 Usefull packages
library(reshape)
library(plyr)                   
library(tidyverse)
library(survival)
library(forcats)
library(data.table)
library(broom)


## Check for re-entries (probably return migrants - padron based (?))
sum(ma$id %in% unique(sma$id)) # 
setdiff(unique(sma$id),ma$id)  # 0 differences




## 1. Extract in utero cohorts 08/1918 - 10/1920 (in 2001 between 83 and 86 and in 2011 between 93 and 96)
## ----------------------------------------------------------------------------------------------------

# Short reasoning: These individuals would have been exposed to the virus in-utero, were young adults during the
# the civil war (high mortality and emigration risk). 
# They were all eligible for receiving a pension, too.

## With the overcomplicated date expression it would be between 1918.58 - 1920.83 (calculated)

flufam.18 <- sscc %>% dplyr::filter(FNAC<1925) %>% dplyr::filter(FNAC>1915) %>%
  # mark the flu cohorts (in utero)
  dplyr::mutate(flu.c = ifelse(FNAC>1918.58 & FNAC<1920.83, "flu.c", ifelse(FNAC<1918.58, "older", "younger")))       
# 81447 individuals

## 2. Descriptive comparison of SES variables
## ---------------------------------------

# A) Simply compare retirement income

# first check - when did they enter reitrement
summary(flufam.18$start.date_Retirement)

# delete outlier (everybody retiring after 2005 - 80 years) BUT keep NAs
flufam.18 <- flufam.18 %>% 
  # sex variable
mutate(SEXO = factor(ifelse(SEXO=="Men","male","female"))) %>% 
# Income for later!
  
# Change the income variables
dplyr::mutate(INCOME = ifelse(income_Retirement>0,income_Retirement,
                                  ifelse(income_Disability>0,income_Disability,0))) %>% 
# Widowhood pension as additional income source
dplyr::mutate(INC.CW = INCOME+income_Widowhood) # %>%                   # Exclude everybody with just a widowhood pension
#dplyr::filter(income_Disability!=0 | income_Retirement!=0)

# mean income values by sex and cohort
aggregate(flufam.18$INCOME,by=list(flufam.18$SEXO,flufam.18$flu.c),FUN=mean)


summary(flufam.18$INC.CW[flufam.18$SEXO=="male"])
summary(flufam.18$INC.CW[flufam.18$SEXO=="female"])


# Compare education level
# -----------------------
round(prop.table(table(flufam.18$ESREAL5[flufam.18$SEXO=="male"],flufam.18$flu.c[flufam.18$SEXO=="male"]),2),3)
round(prop.table(table(flufam.18$ESREAL5[flufam.18$SEXO=="female"],flufam.18$flu.c[flufam.18$SEXO=="female"]),2),3)

# Compare their survival and mortality rates
# ------------------------------------------

# Entry age
flufam.18 <- flufam.18 %>%  
              mutate(entry.age = 2001-FNAC) %>% 
# Age at exit (death-event, study end or emigration - censorship)
              mutate(exit.age = data.out- FNAC) %>% 
# Create event variable  
              mutate(event=ifelse(end.cause2=="D",1,0))

# Distribution of events/censoring
flufam.18 %>%
  ggplot(aes(x=exit.age, fill=as.factor(event))) +
  geom_histogram(bins = 37) +
  scale_fill_discrete(name = "")+
  theme_bw()

# left truncated KMEs for different cohorts
kme.tot <-  survfit(coxph(Surv(time= entry.age,
                             time2 = exit.age,
                             event = event)~1, data = flufam.18), type="kaplan-meier")
kme.tot



# By cohort

# Proportion of survivors at census 2001 for pre-flu, flu, post-flu cohorts
# - old= 1920 / 5-9 years , flu.c= 1920 / 0-4 years, young= 1930 / 5-9 years

pop.and <- read.csv("PopAndalusia19001930.csv",header = T, sep = ";")
class(pop.and)
pop.and <- data.table(pop.and)

      # dcast(pop.and[,.N,keyby=.(sex, agegr)], agegr~sex)
      # pop.and[,.N,.(AGE=agegr=="0 to 4",SEX=sex=="male")]

# 1920 - flu.c
# ------------
# male: 231.483  female: 226.155

# 1920 - older
# ------------
# male: 237.612  female: 229.062

# 1930 - younger
# ------------
# male: 270.269  female: 259.153

# proportion alive at the beginning of the study
class(flufam.18)
flufam.18 <- data.table(flufam.18)
dcast(flufam.18[,.N,keyby=.(SEXO, flu.c)], flu.c~SEXO)

#            Men Women  Prop. Men Prop.Women
#    older  2562  7123    0.01078    0.03110
#    flu.c  4252  9651    0.01837    0.04267
#  younger 19927 37932    0.07373    0.14637



# for starting at the same age 
flufam.18 <- flufam.18 %>% dplyr::filter(exit.age >= 84)

kme.1 <-  survfit(coxph(Surv(time= entry.age,
                             time2 = exit.age,
                             event = event)~1, data = subset(flufam.18,flu.c=="younger")),
                  data=subset(flufam.18,flu.c=="younger"), type="kaplan-meier")


kme.2 <-  survfit(coxph(Surv(time= entry.age,
                             time2 = exit.age,
                             event = event)~1, data = subset(flufam.18,flu.c=="flu.c")),
                  data=subset(flufam.18,flu.c=="flu.c"), type="kaplan-meier")

kme.3 <-  survfit(coxph(Surv(time= entry.age,
                             time2 = exit.age,
                             event = event)~1, data = subset(flufam.18,flu.c=="older")),
                  data=subset(flufam.18,flu.c=="older"), type="kaplan-meier")


kme.1 <- tidy(kme.1) %>% dplyr::select(estimate,time) %>% mutate(cohort="younger")
kme.2 <- tidy(kme.2) %>% dplyr::select(estimate,time) %>% mutate(cohort="flu cohort")
kme.3 <- tidy(kme.3) %>% dplyr::select(estimate,time) %>% mutate(cohort="older")

kme.coh <- union(kme.1, kme.2) %>% union(kme.3) %>% 
  ggplot() +
  geom_step(mapping = aes(x=time, y=estimate, color=cohort)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age")                                   +
  scale_color_manual(values=c("#FFA223", "#000000", "#A9A9A9"), name=" ") +
  theme_bw()
kme.coh <- kme.coh + theme(legend.position = c(0.15, 0.15))



# change levels
flufam.18 <- within(flufam.18, flu.c <- relevel(as.factor(flu.c), ref = "older")) 



cox.m <- coxph(Surv(time= entry.age,
           time2 = exit.age,
           event = event)~ flu.c + estudios4 + dependiente, data = subset(flufam.18, sexo=="male"))

cox.f <- coxph(Surv(time= entry.age,
                    time2 = exit.age,
                    event = event)~ flu.c, data = subset(flufam.18, sexo=="female"))

summary(cox.m)
summary(cox.f)



## Extract in utero cohorts for the famines in the 1940s
## -----------------------------------------------------
# similar idea - probably a strong assumption to think they have all stayed in Andalusia

flufam.40 <- sscc %>% dplyr::filter(FNAC<1950) %>% dplyr::filter(FNAC>1935) %>%
  # mark the flu cohorts (in utero)
  dplyr::mutate(flu.c = ifelse(FNAC>1940 & FNAC<1945, "fam.c", ifelse(FNAC<1940, "older", "younger")))

flufam.40 <- flufam.40 %>%  
  mutate(entry.age = 2011-FNAC) %>% 
  # Age at exit (death-event, study end or emigration - censorship)
  mutate(exit.age = data.out- FNAC) %>% 
  dplyr::filter(exit.age>entry.age) %>% 
  # Create event variable  
  mutate(event=ifelse(end.cause2=="D",1,0))

flufam.40 <- flufam.40 %>% dplyr::filter(exit.age >= 72)

# Distribution of events/censoring
flufam.40 %>%
  ggplot(aes(x=exit.age, fill=as.factor(event))) +
  geom_histogram(bins = 37) +
  scale_fill_discrete(name = "")+
  theme_bw()


# for starting at the same age 
kme.1 <-  survfit(coxph(Surv(time= entry.age,
                             time2 = exit.age,
                             event = event)~1, data = subset(flufam.40,flu.c=="younger")),
                  data=subset(flufam.40,flu.c=="younger"), type="kaplan-meier")


kme.2 <-  survfit(coxph(Surv(time= entry.age,
                             time2 = exit.age,
                             event = event)~1, data = subset(flufam.40,flu.c=="fam.c")),
                  data=subset(flufam.40,flu.c=="flu.c"), type="kaplan-meier")

kme.3 <-  survfit(coxph(Surv(time= entry.age,
                             time2 = exit.age,
                             event = event)~1, data = subset(flufam.40,flu.c=="older")),
                  data=subset(flufam.40,flu.c=="older"), type="kaplan-meier")


kme.1 <- tidy(kme.1) %>% dplyr::select(estimate,time) %>% mutate(cohort="younger")
kme.2 <- tidy(kme.2) %>% dplyr::select(estimate,time) %>% mutate(cohort="fam cohort")
kme.3 <- tidy(kme.3) %>% dplyr::select(estimate,time) %>% mutate(cohort="older")

kme.coh <- union(kme.1, kme.2) %>% union(kme.3) %>% 
  ggplot() +
  geom_step(mapping = aes(x=time, y=estimate, color=cohort)) +
  scale_y_continuous(name = "Survival Probability")                  +
  scale_x_continuous(name = "Age")                                   +
  scale_color_manual(values=c("#FFA223", "#000000", "#A9A9A9"), name=" ") +
  theme_bw()
kme.coh <- kme.coh + theme(legend.position = c(0.15, 0.15))


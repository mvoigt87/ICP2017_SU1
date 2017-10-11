
##### 
### 1 Data generation and creation of potential covariates (household income etc)

### 2 Descriptive tables

### 3 Survival analysis for the married couples

### 4 Output-Tables
### ------------------------------------------------------------------------------------------------- ### 



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
  
## 454294 individuals

## 1.1.1 Check for individuals who are also receiving a pension

id.a <- as.vector(part.R$kID)
id.b <- as.vector(part.R$kIDcon)

r.test <- as.data.frame(intersect(id.a,id.b))
 # ----------------------------- 
 # about 140000 individuals are residing togehter and receive a social security or disability pension
 # ----------------------------- 

## 1.1.2 extract couples with social security pension
colnames(r.test)[1] <- "kID"

pen.coupl <- left_join(r.test,part.R, by="kID")

 # -- quick check
 table(pen.coupl$ECIVIL)
 table(pen.coupl$ECIVIL,pen.coupl$cohab2011)

pen.coupl <- pen.coupl %>% filter(cohab2011==T) 

 # ----------------------------- 
 # 134421 individuals left
 # ----------------------------- 

rm(r.test,part.R)

### 1.2. Partner variables

 # 1.2.1 Generate a copy of the data set

cbind(colnames(pen.coupl))

r.test.a <- pen.coupl %>% dplyr::select(c(1:3,11:29,33:43)) %>% 
  # delete/rename the kIDcon variable
  mutate(PID = kIDcon) %>%
  # rename the kID variable
  mutate(kIDcon=kID) %>% dplyr::select(-kID)

 # rename the variables of the partner
cbind(colnames(r.test.a))
  # add p to colnames to identify the partner variable
colnames(r.test.a)[1:20] <- str_c( colnames(r.test.a)[1:20],"_p" )
colnames(r.test.a)[22:32] <- str_c( colnames(r.test.a)[22:32],"_p" )
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
   mutate(p.surv = factor(ifelse(partner.death==1,"lost partner","partner alive"))) %>% 
   # 1.4.2 Calculate Household income
   # theoretically a time varying variable depending on the widowhood status
   ## for now a simplified version
   mutate(hhincome=ifelse(partner.death == 0,INCOME+INCOME_p,INC.TOT)) %>% 
   
   # 1.4.3 Edit variable number of household members
   mutate(hh= factor(ifelse(NMIEM==2, "with partner only","large household")))
 
  # -----------------------------  
 table(pen.coupl$hh)

  # income variable 
 hist(pen.coupl$hhincome, breaks = 30)
 
 # distribution invites to look at 3 groups (less than 1000 Euro, 1000-1999, more then 2000)
 
 pen.coupl <- pen.coupl %>% mutate(HHINC = factor(ifelse(hhincome<1000,"less than 1000 Euro",
                                                         ifelse(hhincome<=2000,"1000-2000 Euro","more than 2000 Euro"))))
 
 pen.coupl <- within(pen.coupl, HHINC <- relevel(HHINC, ref = "more than 2000 Euro"))
 
 # -----------------------------  
 
# 1.5 Age difference of the partners
 
 pen.coupl <- pen.coupl %>% mutate(age.diff=age2011-age2011_p) %>% 
   ## and as categorical variable (checked for minimum value)
   mutate(age.diff.c = as.factor(ifelse(age.diff < -10, ">10 y younger",
                                        ifelse(age.diff <= - 1,"1-10 y younger",
                                               ifelse(age.diff <= 1, "same age",
                                                      ifelse(age.diff <= 10,"1-10 y older",
                                                             ">10 y older"))))))
 
 pen.coupl <- within(pen.coupl, age.diff.c <- relevel(age.diff.c, ref = "same age")) 

# -----------
 summary(pen.coupl$age.diff)     # minimum value -28.40
 
 pen.coupl %>% ggplot(aes(x=age.diff, fill=SEXO)) +
   geom_histogram(bins=50)+
   scale_fill_discrete(name = "")+
   theme_minimal()
 
### %%%%%%%%%%%%%%%%%%%%%%% ###
### Reference group changes ###
 
 # Edit partnerdeath variable
 table(pen.coupl$partner.death)
 pen.coupl <- within(pen.coupl, p.surv <- relevel(p.surv, ref = "partner alive")) 

 # Partner education
 pen.coupl <- within(pen.coupl, ESREAL5_p <- relevel(ESREAL5_p, ref = "No or Incomplete Educ."))

 
 
 
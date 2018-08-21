#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
#### Script with cleaned data and fixed inconistencies  ####
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####

##### 1. Study population
  ### 1.1 Age limits for the elderly population
##### 2. Preparing the main variables for the analysis
  ### 2.1 event variable
  ### 2.2 sex and pension size
  ### 2.3 Independent categorical variables
##### 3. Save the data set for the analysis step



rm(list = ls())

### 0.1 Load data set
 load("010_sscc.RData")
#load("../TesisMathias/DatosRegistroAndalucia/IO/exploratory.analysis/010_sscc.RData")

str(sscc)
head(sscc)
summary(sscc$FNAC)
## 1420924 observations = pension spells of any kind (retirement, disability, widowhood)
## Oldest individual 113, the youngest 9

## 0.2 Usefull packages
library(reshape)
library(plyr)                   
library(tidyverse)
library(survival)
library(forcats)
library(data.table)

##### Study population
  ### To answer how survival differences in the retired population of Spain are determined by social inequalities,
  ### the analysis is centered around Andalusians who receive a public pension between 2011 and 2015,
  ### all individuals who received any form of individual active public pension are included (retirement and disability).
  ### The idea is that somebody is under risk from the point when he or she has entered retirement. Since data
  ### covers all individuals who have entered or remained recipients of public pension between 2011 and 2015,
  ### it is left-truncated (meaning we do not observe the all person years under risk left from the
  ### event) and we have to account for that. Furthermore, we have to account for unobserved heterogeneity due to
  ### different frailties and the endogenous relationship of health - early retirement/low pention benefits - death.

### Age for start and end of retirement
plot(table(floor(sscc$start.date_Retirement-sscc$FNAC)), main='Edad de comienzo')
plot(table(floor(sscc$end.date_Retirement-sscc$FNAC)),  main='Edad de finalización')

### Age for start and end of disability
plot(table(floor(sscc$start.date_Disability-sscc$FNAC)), main='Edad de comienzo')
plot(table(floor(sscc$end.date_Disability-sscc$FNAC)),  main='Edad de finalización')

##### 1. Create a working dataset (name: retire)
##### --------------------------------------------

### 1.1. Check if all individuals appear only once
kk<- sscc[,1]
sum(duplicated(kk)) ## no dublicates/no more double spells

### 1.2 Quick check for zero pensions and zero contribution years
# Retirement
summary(sscc$income_Retirement[!is.na(sscc$years.start.follow_Retirement)]) # minimum income is 1 cent
# Disability
summary(sscc$income_Disability[!is.na(sscc$years.start.follow_Disability)])

### 1.3 Check of the start year variables
summary(sscc$start.date_Disability)
# Minimum year is 1870 => !Someone who is 95 years old in 2011 (oldest individual in the study) 
# was born in 1916 - the start of the disability period at 1870 is impossible
sscc %>% dplyr::mutate(tt = ifelse(start.date_Disability<1916,TRUE,FALSE)) %>% dplyr::count(tt)
hist(sscc$start.date_Disability[sscc$start.date_Disability<1950])
## There are 76 individuals with impossible start dates regarding their age in 2011 - !Excluded from the analysis

sscc <- sscc %>% dplyr::mutate(ff = ifelse(!is.na(start.date_Disability) & start.date_Disability<1916,1,0)) %>% 
  filter(ff==0) %>% dplyr::select(-ff) # 1420848 individuals left

## ---------- ##
## Retirement

summary(sscc$start.date_Retirement)
sscc %>% dplyr::mutate(tt = ifelse(start.date_Retirement<1981,TRUE,FALSE)) %>% dplyr::count(tt)
# 2753 individuals of the oldest cohort in 2011 have retired "earlier"

# Part of the disabled population who has not worked
as.data.table(sscc) -> sscc
dcast(sscc[,.N,.(Retire=income_Retirement>0,Disability=income_Disability>0)], Retire~Disability)
sscc[,.N,.(Retire=income_Retirement>0,Disability=income_Disability>0)]
#    Retire Disability      N
# 1:   TRUE      FALSE 670368   Jubilados
# 2:  FALSE      FALSE 303838   Viudedad
# 3:  FALSE       TRUE 444327   Proveniente de discapacidad
# 4:   TRUE       TRUE   2315   Comz<zpagina Pension con Dicapacidad  !Just a handful of individuals who receive both


sscc[ ,type.pen:=paste0(ifelse(is.na(years.start.follow_Retirement),'-','r'), 
                    ifelse(is.na(years.start.follow_Disability),'-','d'),
                    ifelse(is.na(years.start.follow_Widowhood),'-','w'))]

dcast(sscc[,.N,keyby=.(SEXO,type.pen)], type.pen~SEXO)

#    type.pen    Men  Women
# 1:      --w   7067 296771 
# 2:      -d- 275632 120473  <--
# 3:      -dw   7958  40264
# 4:      r-- 432393 148707  <-- just retirement pension
# 5:      r-w  16131  73137  <-- retirement and widowhood
# 6:      rd-   1939    190  <-- retirement and disability
# 7:      rdw     96     90  <-- all three forms


##### 2. Select age groups and eligible individuals
retire <- sscc %>% 
  ## Create an age at 2011 (start of the observation period) to extract the age groups 60-95
  ## This will be different from the age at entry to the study which can be later than 2011
dplyr::mutate(age2011 = 2011-FNAC) %>%
    ## When does the clock start?
      # 1. Individual is 65 years and older in 2011
      # 2. Individual who reach age 65 in the course of time until 2016
      # 3. Implicitely every Andalusian who receives a public pension (retirement and disability)
      #        (1. or 2.) and 3.  
    ## Extract everybody who is or turns 65 within the follow up period  
dplyr::mutate(entry.age.r = ifelse(years.start.follow_Retirement>start.date_Retirement,
                                     years.start.follow_Retirement,
                                     start.date_Retirement)-FNAC) %>%
  # disability
dplyr::mutate(entry.age.d = ifelse(years.start.follow_Disability>start.date_Disability,
                                     years.start.follow_Disability,
                                     start.date_Disability)-FNAC) %>%
  # widow
dplyr::mutate(entry.age.w = ifelse(years.start.follow_Widowhood>start.date_Widowhood,
                                     years.start.follow_Widowhood,
                                     start.date_Widowhood)-FNAC) %>%
  ## NOW select the cases you are interested in for the individual analysis (retirement and disability)
dplyr::mutate(entry.age=pmin(entry.age.d,entry.age.r, na.rm=T))  %>%
  ## There often problems with the very old individuals (data errors in combination with small case numbers)
filter(entry.age<=95) %>% 
  ## Furthermore everybody who receives a disability pension will be marked
dplyr::mutate(DIS = ifelse(!is.na(start.date_Disability),1,0))

### leaves us with 1113933 individuals - further selection follows depending on research question

summary(retire$entry.age)  
summary(retire$entry.age.r)  # correct age groups will be selected later (< 65)
summary(retire$entry.age.d)  # same here 

hist(retire$entry.age.r,breaks=34)

# The distribution of entry ages looks reasonable - especially the high number of 65 years old 
# where disabled people could only enter with age 65 (there were 5 years where)
  

## -------------------


### 1.5 Control if age, sex and event distribution are correct

        # age 2011
        summary(retire$age2011)
        hist(retire$age2011, breaks=37)
        # only retired individuals
        hist(retire$age2011[!is.na(retire$start.date_Retirement)], breaks=37)
        ## looks like chunk of a population pyramid (good!)

        # age at exit (from the new "data.out" variable)
        retire %>% mutate(age.exit = data.out-FNAC) %>% 
          ggplot(aes(x=age.exit, fill=cause)) +
          geom_histogram(bins = 37) +
          scale_fill_discrete(name = "")

        
        # event
        table(retire$end.cause2)   ## Social security information
        
        #       C      CD        D 
        #  947566       44   166323 
        
        table(retire$cause)        ##  different values (?)
        
        #          death   out.migration end.observation 
        #         147586            4562          702237
        
        table(retire$end.cause2,retire$cause)
        
        ##        death  out.migration end.observation
        ##  C       0          4088          702069
        ## CD      41             2               1
        ##  D  147545           472             167
        
        ### Only a few cases are not consistent in both data sources
        ### Base source for further proceeding is the information from the BDLPA (little more updated)
        
        round(prop.table(table(retire$cause)),digits = 3)

        
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #####                  

        
##### 2 - Event variable where outmigration is set to a right censoring
        
retire <- retire %>% dplyr::mutate(event = as.numeric(ifelse(cause=="death",1,0))) %>% 
          ##### 2.2 Individual Exit Age  - age at death or censorship
          dplyr::mutate(exit.age = data.out - FNAC)
        
          summary(retire$exit.age)
          
          #    Min.  1st Qu.  Median    Mean 3rd Qu.    Max. 
          #   65.01   71.90   77.21   77.86   83.14   99.99
        
##### 3 - Sex
        
table(retire$SEXO)
retire <- retire %>% mutate(SEXO = factor(ifelse(SEXO=="Men","male","female")))
      
##### 4 - Retirement income
summary(retire$income_Retirement)
summary(retire$income_Disability)
retire %>% dplyr::mutate(tt = ifelse(income_Disability>0 & income_Retirement!=0,T,F)) %>% dplyr::count(tt)
      # 2314 receive both, disability and retirement income
        
        # --- test
        kk <- subset(retire,income_Disability>0 & income_Retirement!=0)
        hist(kk$income_Retirement)
        hist(kk$income_Disabilit)
        kk %>% dplyr::mutate(mm = ifelse(start.date_Disability<start.date_Retirement,T,F)) %>% dplyr::count(mm)  
        # majority of these cases has an entry to retirement after they have received a disability pension
        # cases will be treated as follows: they will be marked with a disability marker and their income will be
        # set to their retirement pension

### Two extra income Variable  - (1. Combinded Disability and Pension income and 2. Like 1 but with widowhood income) 
retire <- retire %>% dplyr::mutate(INCOME = ifelse(income_Retirement>0,income_Retirement,
                                            ifelse(income_Disability>0,income_Disability,0))) %>% 
          ## Now to include widowhood pension as additional income source
dplyr::mutate(INC.CW = INCOME+income_Widowhood) %>%                   # Exclude everybody with just a widowhood pension
dplyr::filter(income_Disability!=0 | income_Retirement!=0)


 summary(retire$INCOME)
 hist(retire$INCOME)
 summary(retire$INC.CW)
 hist(retire$INC.CW)
 
##### 5 - Principal pension variable
# random thresholds 
retire <- retire %>% mutate(pensize = factor(ifelse(retire$INCOME<650,"less than 650 Euro",
                                                        ifelse(retire$INCOME<1000, "650-999 Euro",
                                                               ifelse(retire$INCOME<2000, "1000-1999 Euro", "more than 2000 Euro")))))
## change reference for the pension size variable to the highest income category
retire <- within(retire, pensize <- relevel(pensize, ref = "more than 2000 Euro"))
 
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ######
 
### Alternative B  with 3 categories
 
retire <- retire %>% mutate(pensize.3 = factor(ifelse(retire$INCOME<600, "less than 600 Euro",
                                                       ifelse(retire$INCOME<1200, "600-1199 Euro", "more than 1200 Euro"))))
## change reference for the pension size variable to the highest income category
retire <- within(retire, pensize.3 <- relevel(pensize.3, ref = "more than 1200 Euro"))
 
round(prop.table(table(retire$pensize.3,retire$SEXO),2),2) # column percentage
 
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ######
 
### Transform it into a categorical variable
retire <- retire %>% mutate(pensize.CW = factor(ifelse(retire$INC.CW<650,"less than 650 Euro",
                                                     ifelse(retire$INC.CW<1000, "650-999 Euro",
                                                            ifelse(retire$INC.CW<2000, "1000-1999 Euro", "more than 2000 Euro")))))
 ## change reference for the pension size variable to the highest income category
retire <- within(retire, pensize.CW <- relevel(pensize.CW, ref = "more than 2000 Euro"))

### 6. Independent categorical variables
 

# Contribution period
# ------------------- 
 
hist(retire$contrib.years_Retirement)
summary(retire$contrib.years_Retirement)
# problem 1 : how much did the individuals with a disability contributed
# problem 2 : Changes in the necessary legal contribution period to protect elder cohorts (dictatorship)
 
 
#### Changed code to avoid exclusion of disabled individuals
 # retire <- retire %>% 
 #   ## Extract the 40 cases with more than 60 years of contribution (15-75,20-80 cases are highly rare already)
 #   filter(contrib.years_Retirement<=60 | DIS==1) %>% 
 #   ## also extract the 22061 cases with zero years of contribution will be excluded
 #   filter(contrib.years_Retirement>=0.1 | DIS==1) %>% 
 #   ## create a factor variable
 #   mutate(contrib.years = as.numeric(contrib.years_Retirement)) %>% 
 #   mutate(contrib.years = factor(ifelse(contrib.years<20, "less than 20 years",
 #                                ifelse(contrib.years<41, "20-40 years",
 #                                       "more than 40 years"))))
 

# Year/Age when retirement starts           
# -------------------------------
summary(retire$start.date_Retirement)

# age at entering retirement
retire %>% dplyr::mutate(tt = ifelse(start.date_Retirement - FNAC <45 & income_Retirement!=0,T,F)) %>% dplyr::count(tt)
# 61 individuals retire before 45 - exclude!

# build variable
retire <- retire %>% mutate(age.ret = start.date_Retirement - FNAC) %>% filter(age.ret>=45)
 
summary(retire$age.ret)


# EDUCATION           
# ---------
 
table(retire$ESREAL5)
 ## collapse categories incomplete and illiterate
 
retire$ESREAL5<- revalue (retire$ESREAL5, c("Illiterate"="No or Incomplete Educ.", "Without studies"="No or Incomplete Educ.",
                                   "Primary Educ."="Primary Educ.", "Secondary Educ."="Secondary Educ.",
                                   "Tertiary Educ."="Tertiary Educ."))
 
retire <- within(retire, ESREAL5 <- relevel(ESREAL5, ref = "No or Incomplete Educ."))

 ## Education variable - further collapse to three categories (Changed after sensitivity analysis - 06.02.18)
 
retire$ESREAL3 <- revalue(retire$ESREAL5, c("No or Incomplete Educ."="No or Incomplete Educ.", "Primary Educ."="Primary Educ.",
                                                   "Secondary Educ."="Secondary or higher Educ.", "Tertiary Educ."="Secondary or higher Educ."))
 
round(prop.table(table(retire$ESREAL3)), digits = 2)
 

# CIVIL STATUS
# ------------
table(retire$cohab2002)
table(retire$ECIVIL)
table(retire$cohab2011)
table(retire$ECIVIL,retire$cohab2011) # widowed within observation

### Create a variable with the widowhood information from 2001 + 2011 and the cohabitation

retire <- retire %>% mutate(cohab2011 = ifelse(cohab2011==TRUE,T,F)) %>% 
          mutate(civil.status = factor(ifelse(retire$ECIVIL=="Widower" | !is.na(start.date_Widowhood)<=2011,
                                                         "widowed",
                                                         ifelse(retire$ECIVIL=="Married" & cohab2011==TRUE,"married",
                                                         "other forms"))))
retire$civil.status[is.na(retire$civil.status)] <- "other forms"
round(prop.table(table(retire$civil.status)),digits=2)

## This way of coding is chosen to assure to have a reliable information on the civil state in 2011
## It is possible that we exclude some married individuals who are not cohabitating (LAT relationships)


# House ownership
# ---------------
retire <- retire %>% dplyr::mutate(HousReg = ifelse(HousReg!="Own","Not owned", "owned"))
round(prop.table(table(retire$HousReg)),digits = 2)

# Car / Mobility variable 
# -----------------------
retire <- retire %>% mutate(mobil = factor(ifelse(car!="no car","car(s) available", "no car available")))
round(prop.table(table(retire$mobil)),digits = 2)


# Household size & children in household
# --------------------------------------

prop.table(table(retire$CONPARHIJ))
retire <- retire %>% mutate(hh= factor(ifelse(CONPARHIJ==1, "single",
                                              ifelse(CONPARHIJ==3,"single with children",
                                              ifelse(CONPARHIJ==2,"with partner","partner and children")))))
retire <- within(retire, hh <- relevel(hh, ref = "single"))





#### Additional selection for further analysis of retired and disabled individuals

### reorder variable position
class(retire)
retire <- data.table(retire)
retire[,.(kID, FNAC, SEXO, ECIVIL, civil.status, ESREAL, ESREAL5, ESREAL3,
          SITU, 
          HousReg, car, mobil, CONPARHIJ, NMIEM,  hh,
          contrib.years_Retirement, 
          income_Disability, income_Retirement, income_Widowhood, 
          years.start.follow_Disability, years.start.follow_Retirement, years.start.follow_Widowhood, 
          start.date_Disability, start.date_Retirement, start.date_Widowhood, 
          end.date_Disability, end.date_Retirement, end.date_Widowhood, data.out, 
          entry.age.r, entry.age.d, entry.age.w, type.pen,  entry.age,  exit.age,  age2011, 
          end.cause, end.cause2, cause, age.ret,
          kIDcon, FViudedad, cohab2002, cohab2011, 
          DIS, event, 
          INCOME, INC.CW, pensize, pensize.3, pensize.CW
)] -> retire

## Population A: 
##  1.) Individuals with more than 65 
##  2.) who have received a retirement pension within the observation period (2011-15) 
##  3.) If these individuals have received another form of social security suport (disability, widowhood)
##      they will be treated as censored.
##  4.) Exclude the retirees with pensions less than 350 € per month = that would be below minimum and these individuals
##      are like to have a non-contributory complementation

retire[ ( start.date_Retirement<2011 & age2011 >=65 & ( is.na(start.date_Disability) ) & 
            ( is.na(start.date_Widowhood) |  start.date_Widowhood>2011)  
          |
            (exit.age > 65 & age2011 <= 65 &  age2011 >= 60 &  !is.na(start.date_Retirement)   & 
               ( is.na(start.date_Disability)   ) & 
               ( is.na(start.date_Widowhood) |  (start.date_Widowhood > start.date_Retirement & start.date_Widowhood>2011) ) ) ),
        .(kID, FNAC,  age2011,  SEXO, civil.status, 
          ESREAL5, ESREAL3,
          HousReg, car, mobil,
          CONPARHIJ, NMIEM, hh,
          # contrib.years_Retirement,  contrib.years, 
          INCOME =  income_Retirement,  pensize, pensize.3,
          years.start.follow=  years.start.follow_Retirement,
          start.date =  start.date_Retirement, start.date_Disability, 
          end.date= end.date_Retirement,  data.out, age.ret,
          # type.pen, 
          entry.age,  exit.age, 
          end.cause, end.cause2, cause, 
          # kIDcon, FViudedad, 
          start.date_Widowhood ,  
          # cohab2002, cohab2011, 
          DIS, event)
        ] -> retire.A

## 554276 individuals
# retire.A[entry.age<65 & (data.out-FNAC) < 65 ] -> pp ; length(pp$kID)
# retire.A[(!is.na(start.date_Disability)) |  (!is.na(start.date_Widowhood)),] ->  pp
# pp[,.N, .(D=!is.na(start.date_Disability), W=  !is.na(start.date_Widowhood))]

## All retirees who were less than 65 in 2011 but turn 65 in the course of the observation period are set to 65 entry age

retire.A[entry.age<65,entry.age:=65]

# 3.) censor everybody who experience the death of a partner (receives widowhood pension) - his/her income is not reflection of
# their working life anymore

retire.A[start.date_Widowhood < data.out, ':='(data.out=start.date_Widowhood, 
                                               end.date=start.date_Widowhood,
                                               exit.age=start.date_Widowhood-FNAC,
                                               end.cause2='C', cause='Widowhood') ]
# 4.)
retire.A[INCOME<300,.N] # 6257 individuals with low pensions 

retire.A[INCOME>300] -> retire.A

# to avoid inconsitencies
retire.A[entry.age < exit.age] -> retire.A

# changes in the event variable
retire.A <- retire.A %>% mutate(event = ifelse(end.cause2=="C",0,1))
prop.table(table(retire.A$event))

##### 7 - Save prepared data set
  
# dataset with only selected retired cases
# save(retire, file='031_RETIND.RData')
save(retire.A, file='031b_RETIND.RData')

  
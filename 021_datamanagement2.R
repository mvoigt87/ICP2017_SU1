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



### 0.1 Load data set
load("010_sscc.RData")

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
  ### To answer how health differences in the retired population of Spain are determined by social inequalities,
  ### the analysis is centered around the Andalusians who receive a public pension between 2011 and 2015.
  ### All individuals who received any form of individual active public pension are included (retirement and disability).
  ### The idea is that somebody is under risk from the point when he or she has entered retirement. Since data
  ### covers all individuals who have entered or remained recipients of public pension between 2011 and 2015,
  ### Therefore, the data is left-truncated (meaning we do not observe the all person years under risk left from the
  ### event) and we have to account for that. Furthermore, we have to account for unobserved heterogeneity due to
  ### different frailties and the endogenous relationship of health - early retirement/low pention benefits - death.



##### 1.1. Create a working dataset (retire)

### 1.1.2 Quick check for dublicates
kk<- sscc[1]
sum(duplicated(kk)) ## no dublicates/no more double spells

### 1.1.3 Quick check for zero pensions and zero contribution years
summary(sscc$income_Retirement[!is.na(sscc$years.start.follow_Retirement)])
# minimum income is 1 cent
summary(sscc$income_Disability[!is.na(sscc$years.start.follow_Disability)])

### 1.1.4 Check of the start year variables
summary(sscc$start.date_Disability)
# Minimum year is 1870 => !Someone who is 95 years old in 2011 (oldest individual in the study) 
# was born in 1916 - the start of the disability period at 1870 is impossible
sscc %>% dplyr::mutate(tt = ifelse(start.date_Disability<1916,TRUE,FALSE)) %>% dplyr::count(tt)
hist(sscc$start.date_Disability[sscc$start.date_Disability<1950])
## There are 66 individuals with impossible start dates regarding their age in 2011
#  Excluded from the analysis
sscc <- sscc %>% dplyr::mutate(ff = ifelse(!is.na(start.date_Disability) & start.date_Disability<1916,1,0)) %>% 
  filter(ff==0) %>% select(-ff)

## ---------- ##
## Retirement

summary(sscc$start.date_Retirement)
# Minimum age at retirement = 1965 
# - oldest individuals reached age 65 in 1981
sscc %>% dplyr::mutate(tt = ifelse(start.date_Retirement<1981,TRUE,FALSE)) %>% dplyr::count(tt)
# 2753 individuals of the oldest cohort in 2011 have retired "early"

# count the disabled individuals who had worked and who did not
as.data.table(sscc) -> sscc
dcast(sscc[,.N,.(Retire=income_Retirement>0,Disability=income_Disability>0)], Retire~Disability)


##### 1.2. Creating the working dataset
retire <- sscc %>% 
  ## Create an age at 2011 (start of the observation period) to extract the age groups 60-95
  ## This will be different from the age at entry to the study which can be later than 2011
  dplyr::mutate(age2011 = 2011-FNAC) %>%
  ## When does the clock start?
  # 1. Individual is 65 years and older in 2011
  # 2. Individual who reach age 65 in the course of time until 2016
  # 3. Implicitely every Andalusian who receives a public pension (retirement and disability)
  # 4. To consider are the widows - for households their pension could be considered
  #    as part of their lifetime earnings
  ## Extract everybody who is or turns 65 within the follow up period  
  # Therefore the disability and retirement entry age will be combined
  dplyr::mutate(entry.age.r = years.start.follow_Retirement-FNAC) %>%
  # disability
  dplyr::mutate(entry.age.d = years.start.follow_Disability-FNAC) %>% 
  # widow
  dplyr::mutate(entry.age.w = years.start.follow_Widowhood-FNAC) %>% 
  ## NOW select the cases you are interested in for the individual analysis (not the widows)
  ## Difficulty: Disability pension will automatically become a retirement pension with 65
  dplyr::mutate(entry.age.r = ifelse(is.na(entry.age.r) & entry.age.d>64, entry.age.d, entry.age.r)) %>% 
  ## Exclude the widows for now
  filter(!is.na(entry.age.r)) %>% 
  ## There often problems with the very old individuals (data errors in combination with small case numbers)
  filter(entry.age.r<=95) %>% 
  ## the same applies to the cases who are 65 or younger (oriented at the legal retirement age 65)
  filter(entry.age.r>=65) %>% 
  ## Furthermore everybody who receives a disability pension will be marked
  dplyr::mutate(DIS = ifelse(!is.na(start.date_Disability),1,0))


           ### leaves us with 757665 individuals
           
summary(retire$entry.age.r)  
hist(retire$entry.age.r,breaks=34)
#     Min. 1st Qu.  Median    Mean  3rd Qu.     Max. 
#   65.00   67.19   72.68   73.69   78.83   95.00 
# The distribution of entry ages looks reasonable - especially the high number of 65 years old 
# where disabled people could only enter with age 65 (there were 5 years where)
  
                ##   9827 cases who retired earlier than 61
  
                ##   (with 55 the number is 1239984)
## -------------------


### 1.3 Age, sex and event distribution

        # age 2011
        summary(retire$age2011)
        hist(retire$age2011, breaks=37)
        ## looks like cut of a population pyramid (good!)

        # age at exit (from the new "data.out" variable)
        retire %>% mutate(age.exit = data.out-FNAC) %>% 
          ggplot(aes(x=age.exit, fill=cause)) +
          geom_histogram(bins = 37) +
          scale_fill_discrete(name = "")
        # Important to show the Kaplan Meier Estimator beginning at age 65 (this is when the censoring sets in)
        
        # event
        table(retire$end.cause2)   ## Social security information
        
        #       C      CD        D 
        #  706157      44   148184 
        
        table(retire$cause)        ##  
        
        #          death   out.migration end.observation 
        #         147586            4562          702237
        
        table(retire$end.cause2,retire$cause)
        
        ##        death  out.migration end.observation
        ##  C       0          4088          702069
        ## CD      41             2               1
        ##  D  147545           472             167
        
        ### Only a few cases are not consistent in both data sources
        ### Base source for further proceeding is the information from the Longevity FU
        
        round(prop.table(table(retire$cause)),digits = 3)

        
##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #####                  

        
##### 2.1 Create an event variable where outmigration translate to a right censoring
        
retire <- retire %>% dplyr::mutate(event = as.numeric(ifelse(cause=="death",1,0))) %>% 
          ##### 2.2 Individual Exit Age  - age at death or censorship
          dplyr::mutate(exit.age = data.out - FNAC)
        
          summary(retire$exit.age)
          
          #    Min.  1st Qu.  Median    Mean 3rd Qu.    Max. 
          #   65.01   71.90   77.21   77.86   83.14   99.99
        
##### 2.3 Sex and Pensionsize
        
      table(retire$SEXO)

      retire <- retire %>% mutate(SEXO = factor(ifelse(SEXO=="Men","male","female")))
      
   ### Retirement income
      summary(retire$income_Retirement)
      summary(retire$income_Disability)
      retire %>% dplyr::mutate(tt = ifelse(income_Disability>0 & income_Retirement!=0,T,F)) %>% dplyr::count(tt)
      # 1796 receive disability and retirement income
        
        # --- test
        kk <- subset(retire,income_Disability>0 & income_Retirement!=0)
        hist(kk$age2011)
        table(kk$SEXO)
        table(kk$ESREAL5)
        
        hist(kk$income_Retirement)
        hist(kk$income_Disabilit)
        kk %>% dplyr::mutate(mm = ifelse(start.date_Disability<start.date_Retirement,T,F)) %>% dplyr::count(mm)  
        # majority of these cases has an entry to retirement after they have received a disability pension
        #
        # cases will be treated as follows: they will be marked with a disability marker and their income will be
        # set to their retirement pension

### Income Variable  - including widowhood pension  
retire <- retire %>% dplyr::mutate(INCOME = ifelse(income_Retirement>0,income_Retirement,
                                            ifelse(income_Disability>0,income_Disability,0))) %>% 
          ## Now to include widowhood pension as additional income source
          dplyr::mutate(INC.CW = INCOME+income_Widowhood) %>%
          ## But exclude everybody who just has received a widowhood pension
          ## These people will enter in the second part of the analysis - theoretically don´t have an income
          ## which is related to their life time employment activity
          dplyr::filter(income_Disability!=0 | income_Retirement!=0)
 

 summary(retire$INCOME)
 hist(retire$INCOME)
 summary(retire$INC.CW)
 hist(retire$INC.CW)
 
### Principal pension variable
 
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






### 2.3 Independent categorical variables
 
 # ------------------- #
 # Contribution period #
 # ------------------- #
 
 hist(retire$contrib.years_Retirement)
 summary(retire$contrib.years_Retirement)
 # problem: how much did the individuals with a disability contributed
 
 
 #### Changed code to avoid exclusion of disabled individuals
 
 
 retire <- retire %>% 
   ## Extract the 40 cases with more than 60 years of contribution (15-75,20-80 cases are highly rare already)
   filter(contrib.years_Retirement<=60 | DIS==1) %>% 
   ## also extract the 22061 cases with zero years of contribution will be excluded
   filter(contrib.years_Retirement>=0.1 | DIS==1) %>% 
   ## create a factor variable
   mutate(contrib.years = as.numeric(contrib.years_Retirement)) %>% 
   mutate(contrib.years = factor(ifelse(contrib.years<20, "less than 20 years",
                                ifelse(contrib.years<41, "20-40 years",
                                       "more than 40 years"))))
 
 # ------------------- #
 # EDUCATION           #
 # ------------------- #
 
 table(retire$ESREAL5)
 ## collapse categories incomplete and illiterate
 
 retire$ESREAL5<- revalue (retire$ESREAL5, c("Illiterate"="No or Incomplete Educ.", "Without studies"="No or Incomplete Educ.",
                                   "Primary Educ."="Primary Educ.", "Secondary Educ."="Secondary Educ.",
                                   "Tertiary Educ."="Tertiary Educ."))
 
 retire <- within(retire, ESREAL5 <- relevel(ESREAL5, ref = "No or Incomplete Educ."))
 round(prop.table(table(retire$ESREAL5)), digits = 2)
 
 
 ## Education variable - further collapse to three categories (Changed after sensitivity analysis - 06.02.18)
 
 retire$ESREAL3 <- revalue(retire$ESREAL5, c("No or Incomplete Educ."="No or Incomplete Educ.", "Primary Educ."="Primary Educ.",
                                                   "Secondary Educ."="Secondary or higher Educ.", "Tertiary Educ."="Secondary or higher Educ."))
 
 round(prop.table(table(retire$ESREAL3)), digits = 2)
 
 # ------------------- #
 # CIVIL STATUS        #
 # ------------------- #
table(retire$cohab2002)
table(retire$ECIVIL)
table(retire$cohab2011)
table(retire$ECIVIL,retire$cohab2002)

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

# -------------------------- #
# House ownership            #
# -------------------------- #

# collapse the variables

retire <- retire %>% dplyr::mutate(HousReg = ifelse(HousReg!="Own","Not owned", "owned"))
round(prop.table(table(retire$HousReg)),digits = 2)


# ----------------------- #
# Car / Mobility variable #
# ----------------------- #
table(retire$car)
retire <- retire %>% mutate(mobil = factor(ifelse(car!="no car","car(s) available", "no car available")))
round(prop.table(table(retire$mobil)),digits = 2)

# ----------------------- #
# Household size          #
# ----------------------- #
# collapsing categories according to tests

retire <- retire %>% mutate(hh= factor(ifelse(NMIEM==2, "with partner only","larger household")))
retire <- within(retire, hh <- relevel(hh, ref = "larger household"))


###### 3. Small checks and saving

### --------------------- ###  
### Scanning the data     ###
### Visual intuitive test ###
### --------------------- ###

## visualizing education/pension income
  retire %>%  ggplot(aes(x=INCOME, fill=ESREAL5)) +
  geom_histogram(bins = 20)+
  labs(color="")+
  theme_bw()  
  
## Event distribution
  
  # by sex
  retire %>% mutate(event.x= as.factor(ifelse(event==1,"death","censor"))) %>% 
  ggplot(aes(x=SEXO,fill=event.x)) +
  geom_bar(stat = "count")
  
  # the common contribution: more male deaths in these age groups
  
  # by education 
  round(100*(prop.table(table(retire$event, retire$ESREAL5),2)),digits = 3)
  retire %>% mutate(event.x= as.factor(ifelse(event==1,"death","censor"))) %>% 
    ggplot(aes(x=ESREAL5,fill=event.x)) +
    geom_bar(stat = "count")
 
  # by pension size 
  round(prop.table(table(retire$event,retire$pensize),2),digits = 3) # column percentage
  # income gradient visible
  
  round(prop.table(table(retire$event,retire$pensize.3),2),digits = 3) # column percentage
  # income gradient visible
  
##### 3.2 Save prepared data set
  
  save(retire, file='031_RETIND.RData')
  
  rm()
  
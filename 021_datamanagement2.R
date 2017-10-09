#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####
#### Script with cleaned data and fixed inconistencies  ####
#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ####

##### 1. Study population
  ### 1.1. Age limits for the elderly population
  ### 1.3  Event variable
##### 2. Preparing the main variables for the analysis
  ### 2.1. sex and pension size
  ### 2.2. Independent categorical variables
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
# library(data.table)

##### Study population
  ### To answer how health differences in the retired population of Spain are determined by social inequalities,
  ### the analysis is centered around the Andalusians who receive a public pension between 2011 and 2015.
  ### All individuals who received any form of individual active public pension are included (retirement and disability).
  ### The idea is that somebody is under risk from the point when he or she has entered retirement. Since data
  ### covers all individuals who have and entered or remained recipients of public pension between 2011 and 2015,
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
sscc %>% mutate(tt = ifelse(start.date_Disability<1916,TRUE,FALSE)) %>% count(tt)
hist(sscc$start.date_Disability[sscc$start.date_Disability<1950])
## There are 66 individuals with impossible start dates regarding their age in 2011
#  Excluded from the analysis
sscc <- sscc %>% mutate(ff = ifelse(!is.na(start.date_Disability) & start.date_Disability<1916,1,0)) %>% 
  filter(ff==0) %>% select(-ff)

## ---------- ##
## Retirement

summary(retire$start.date_Retirement)
# Minimum age at retirement = 1965 
# - oldest individuals reached age 65 in 1981
retire %>% mutate(tt = ifelse(start.date_Retirement<1981,TRUE,FALSE)) %>% count(tt)
# 958 individuals of the oldest cohort in 2011 have retired "early"


##### 1.2. Creating the working dataset
retire <- sscc %>% 
  ## Create an age at 2011 (start of the observation period) to extract the age groups 60-95
  ## This will be different from the age at entry to the study which can be later than 2011
  mutate(age2011 = 2011-FNAC) %>%
  ## When does the clock start?
  # 1. Individual is 65 years and older in 2011
  # 2. Individual who reach age 65 in the course of time until 2016
  # 3. Implicitely every Andalusian who receives a public pension (retirement and disability)
  # 4. To consider are the widows - for households their pension could be considered
  #    as part of their lifetime earnings
  ## Extract everybody who is or turns 65 within the follow up period  
  # Therefore the disability and retirement entry age will be combined
  mutate(entry.age.r = years.start.follow_Retirement-FNAC) %>%
  # disability
  mutate(entry.age.d = years.start.follow_Disability-FNAC) %>% 
  # widow
  mutate(entry.age.w = years.start.follow_Widowhood-FNAC) %>% 
  ## NOW select the cases you are interested in for the individual analysis (not the widows)
  ## Difficulty: Disability pension will automatically become a retirement pension with 65
  mutate(entry.age.r = ifelse(is.na(entry.age.r) & entry.age.d>64, entry.age.d, entry.age.r)) %>% 
  ## Exclude the widows for now
  filter(!is.na(entry.age.r)) %>% 
  ## There often problems with the very old individuals (data errors in combination with small case numbers)
  filter(entry.age.r<=95) %>% 
  ## the same applies to the cases who are 60 or younger (oriented at the legal early retirement age 61)
  filter(entry.age.r>=61)

           ### leaves us with 854450 individuals
           
summary(retire$entry.age.r)  
hist(retire$entry.age.r,breaks=34)
#     Min. 1st Qu.  Median    Mean  3rd Qu.     Max. 
#    61.00   65.61   70.96   72.47   77.96   95.00
  
                ##   9827 cases who retired earlier than 61
  
                ##   (with 55 the number is 1239984)
## -------------------


### 1.3 Age, sex and event distribution

        # age 2011
        summary(retire$age2011)
        hist(retire$age2011, breaks=37)
        ## looks like cut of a population pyramid (good!)
        
        # age at entry to retirement
        retire %>% mutate(age.ret = start.date_Retirement-FNAC) %>% 
          ggplot(aes(x=age.ret)) +
          geom_histogram(bins = 37)
        ### Majority of entries to retirement at age 65 - substantial number of people enter before
        
        # age at exit (from the new "data.out" variable)
        retire %>% mutate(age.exit = data.out-FNAC) %>% 
          ggplot(aes(x=age.exit, fill=cause)) +
          geom_histogram(bins = 37) +
          scale_fill_discrete(name = "")
        # Important to show the Kaplan Meier Estimator beginning at age 65 (this is when the censoring sets in)
        
        # event
        table(retire$end.cause2)   ## Social security information
        
        #       C      CD        D 
        #  706164     50 148236 
        
        table(retire$cause)        ##  
        
        #          death   out.migration end.observation 
        #         147644            4564          702242
        
        table(retire$end.cause2,retire$cause)
        
        ##        death  out.migration end.observation
        ##    C       0          4090          702074
        ##   CD      47             2               1
        ##    D  147597           472             167
        
        ### Only a few cases are not consistent in both data sources
        ### Base source for further proceeding is the information from the Longevity FU
        
        round(prop.table(table(retire$cause)),digits = 3)
        
### 1.4. Create an event variable where outmigration translate to a right censoring
        
        retire <- retire %>% mutate(event = as.numeric(ifelse(cause=="death",1,0)))
        

##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #####                

##### 2.1 Individual Entry Age
        
        ### small test of variables where the disability entry follows the entry to retirement
        retire %>% mutate(ay = ifelse(years.start.follow_Disability>years.start.follow_Retirement,TRUE,FALSE)) %>% 
          count(ay)
        # 269 case have a later disability then retirement entry - 1944 of the ones who retire 
        # were disabled at some point earlier
        # There is additional potential for confusion before age 65 since there is no indication about a) the degree
        # of disability (did they continue working etc.) and b) it is unknown when somebody who is disabled has entered
        # retirement. This will make the entry age variable obsolete but introduces information on disability.
        # - The entry age should be set to 65 -
        
##### 2.2 Individual Entry Age
        
        
##### 2.3 Sex and Pensionsize
        

      table(retire$SEXO)
        retire <- retire %>% mutate(SEXO = factor(ifelse(SEXO=="Men","male","female")))
      
        
      summary(retire$income_Retirement)
      
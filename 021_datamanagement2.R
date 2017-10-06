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

retire <- sscc %>% 
  ## Create an age at 2011 (start of the observation period) to extract the age groups 60-95
  ## This will be different from the age at entry to the study which can be later than 2011
  mutate(age2011 = 2011-FNAC) %>%
  ## Extract age groups 60-95     
  ## (changed from 55 to 60 because of the extremely low case numbers before 60; 
  ## also data driven because we cannot assure that someone who received a disability pension)
  filter(age2011>=60) %>% filter(age2011<=95) #%>% 
              ## ! Not sure about this: extract for now all individuals who have only received a widowhood pension
  
##  1.120.882 Individuals  (with 55 the number is 1239984)
## -------------------


### 1.1.2 Quick check for dublicates
        kk<- retire[1]
        sum(duplicated(kk)) ## no dublicates/no more double spells

### 1.1.3 Quick check for zero pensions and zero contribution years
       summary(retire$income_Retirement[!is.na(retire$years.start.follow_Retirement)])
       # minimum income is 1 cent
       summary(retire$income_Disability[!is.na(retire$years.start.follow_Disability)])
        
### 1.2 Age, sex and event distribution

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
        
        # event
        table(retire$end.cause2)   ## Social security information
        
        #       C      CD        D 
        #  913962      49   206871 
        
        table(retire$cause)        ##  
        
        #    death   out.migration end.observation 
        #   205997            6469          908416 
        
        table(retire$end.cause2,retire$cause)
        
        ##        death  out.migration end.observation
        ##    C       0          5775          908187
        ##   CD      46             2               1
        ##    D  205951           692             228
        
        ### Only a few cases are not consistent in both data sources
        ### Base source for further proceeding is the information from the Longevity FU
        
        round(prop.table(table(retire$cause)),digits = 3)
        
### 1.3. Create an event variable where outmigration translate to a right censoring
        
        retire <- retire %>% mutate(event = as.numeric(ifelse(cause=="death",1,0)))
        

##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #####                

##### 2.1 Individual Entry Age
        
        ## When you turn 65 and are in retirement or receive a disablity pension
        summary(retire$years.start.follow_Retirement)
        summary(retire$years.start.follow_Disability)
        
        ## 
        retire <- retire %>% mutate(age.entry = ifelse())
        
##### 2.2 Sex and Pensionsize
        

      table(retire$SEXO)
        retire <- retire %>% mutate(SEXO = factor(ifelse(SEXO=="Men","male","female")))
      
        
      summary(retire$income_Retirement)
      
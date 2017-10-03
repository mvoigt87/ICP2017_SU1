#### %%%%%%%%%%%%%%%%%%%%%% #####
#### SU 1 - Retirement data #####
#### %%%%%%%%%%%%%%%%%%%%%% #####

### ------------------------------------------------------------- ###
### 1. Data sets from Frans R file - Individual level data

  # 1.1 Data set for the individual analysis of pensioners
  # 1.2 Tidy data
  # 1.3 Reorganize and rename categories
  # 1.4 Graphical checks

### ------------------------------------------------------------- ###

load("060_ss_cc2002.RData")
# load("060_ss_cc2002_old.RData")

### 0.1 load necessary packages
library(reshape)
library(plyr)                   # plyr is not part of the tidyverse package
library(tidyverse)
library(survival)
library(forcats)


### 1.1 Data set for individual analysis - preparation

# Since it is possible that an individual has multiple pension spells (i.e. receives a disability pension before 
# reaching the eligibility age for retirement), the first step will be assign pensions to individuals. Therefore
# it is necessary to create a "wide" data structure with regard to the pension spells.


### 1.1.1 Joining the data sources

###    join the pension to the census individuals (using innerjoin = there are some individuals in the
###    social security data which were not in the 2001 census)

### 
### NEW: Entry year for all will be 2011 + I will analyze the mortality for everybody who receives a pension
### idea is to have a synthetic cohort

retire <- inner_join(ss.benefits,ss.benefits.c2002,by="kID") %>%  
  # global start year for our observation is 2011
  # age at the entering poin to our risk set                       !!! Person years under risk !!!
  mutate(age = years.of.entry - FNAC) %>%
  # extract the age groups 50-90 (age at census year)
  filter(age>=55) %>% filter(age<=90)

# 1.381.307 people at risk = before: 1.371.099 people

# 1.1.2 order by kID (because yu have an unordered long dataset and subset)

# easy way
retire <- retire[order(retire$kID) , ]

# see double entries = individuals with two different social security spells
# (remember "dublicated" function)
r.test <- retire[duplicated(retire[1]) | duplicated(retire[1], fromLast=TRUE),] 
# 266466 cases receive more than one form of social security benefits

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
kk<- retire[1]  # comprueba
sum(duplicated(kk))                                 # 133336 of 1.381.307 cases
sum(duplicated(kk, fromLast=TRUE))                  # 133336 of 1371099 cases
sum(duplicated(kk) | duplicated(kk, fromLast=TRUE)) # 266466 of 1371099 cases
sum(duplicated(kk) & duplicated(kk, fromLast=TRUE))  # 206 triplicados o más (triplets)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

sum(duplicated(r.test$kID))  # 133336
length(unique(r.test$kID))   # 133130  personas con mas de una pension.

  # 133130 cases receive more than one form of social security benefits

  # 1.248.177 = expected number of individuals after grouping

# ------------------------------------------------------------------------------------------- #
# 1.1.3 Preparing the different kinds of pensions spells to be assigned to only one individual

### Year of entry in disability and pension size as variable
retire$dis.y <- 0
retire$dis.y[retire$benefit.type=="Incapacidad"] <- retire$start.date[retire$benefit.type=="Incapacidad"]
# ------------------- #
max(retire$IMP2016[retire$benefit.type=="Incapacidad"])   
hist(retire$IMP2016[retire$benefit.type=="Incapacidad"])
retire$dis.pen <- 0
retire$dis.pen[retire$benefit.type=="Incapacidad"] <- retire$IMP2016[retire$benefit.type=="Incapacidad"]
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  #

### Year of entry in retirement and pension size as variable
retire$retire.y <- 0
retire$retire.y[retire$benefit.type=="Jubilación"] <- retire$start.date[retire$benefit.type=="Jubilación"]
# ------------------- #
max(retire$IMP2016[retire$benefit.type=="Jubilación"])   
hist(retire$IMP2016[retire$benefit.type=="Jubilación"])
retire$ret.pen <- 0
retire$ret.pen[retire$benefit.type=="Jubilación"] <- retire$IMP2016[retire$benefit.type=="Jubilación"]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  #
### Year of entry in widowhood and pension size as variable
retire$widow.y <- 0
retire$widow.y[retire$benefit.type=="Viudedad"] <- retire$start.date[retire$benefit.type=="Viudedad"]
# ------------------- #
max(retire$IMP2016[retire$benefit.type=="Viudedad"]) 
hist(retire$IMP2016[retire$benefit.type=="Viudedad"])        ## interesting distribution
retire$wid.pen <- 0
retire$wid.pen[retire$benefit.type=="Viudedad"] <- retire$IMP2016[retire$benefit.type=="Viudedad"]
# ---------------------------------------------------------------- #

#### Change the "character" variables for the following step - (applying the "minimum function")
 str(retire)
 table(retire$end.cause)
 round(prop.table(table(retire$end.cause)),digits = 3)
 
 #       C        D 
 #   83.6%    16.4%

 
   # "end.cause" into a binary numeric variable
    retire <- retire %>% mutate(event=if_else(end.cause=="C",0,1))
    
   # tenen, SVIV, VEHIC - categories will be renamed in English at a later step

   retire$TENEN <- as.numeric(as.character(retire$TENEN))
   retire$SVIV <- as.numeric(as.character(retire$SVIV))
   retire$VEHIC <- as.numeric(as.character(retire$VEHIC))
   retire$DEPEN <- as.numeric(as.character(retire$DEPEN))


####### --------------------------------------------------------------------- #########



### 1.2.1 Shape data in tidy format = only one row per individual (assigning different socsec benefits to same individual)
glimpse(retire)

retire <- retire %>% select(-benefit.type,-IMP2016, -end.cause) %>% 
          group_by(kID) %>% summarise_all(funs(max))

### leaves us with 1.247.971 individuals                             #### Number is bigger than in earlier versions !!!


### 1.2.2 Further cleaning: Exclude everybody without retirement pension 

  retire <- retire %>% 
    # to extract all cases which have retired and exclude the ones who just receive widowhood/disability pension
    filter(retire.y!=0) %>% 
    filter(ret.pen!=0) %>% 
    ## create a variable to account for health effects -  answered in the census (DEPEN) he or she needs help or
    ## receives a disability pension
    mutate(health.st = ifelse(dis.y!=0 & dis.y<retire.y, 1, 0)) %>% mutate(health.st = ifelse(DEPEN==1,1,health.st))
  
  table(retire$health.st) 
  
  # 0 = 666424   1 = 3064 (0.45 % of the retirees are frail regarding this definition (DAHLY?))
  
  ### total case number 670504          #### !!! This number has changed due to the change in the entry age variable
  

  


####### ---------------------------------------------------------------------- #########

### 1.3 Reorganize and translate variable names

### 1.3.1 create a new factor variable for sex, education, occupation situation in 2002 (SITP), 
###       years paid in social security, Household members in 2002 (NMIEM), Number of "house mates"/partner (CON/CONPARHIJ), 
###       TENEN, car ownership (VEHIC), CSE - socioeconomic situation in 2002, Room number (NHAB)
  
  ## -------------- ##
  ## Event variable ##
  ## -------------- ##
  
  retire <- retire %>% mutate(exit = factor(ifelse(event==1,"dead","censored")))
  
  ev.tbl <- table(retire$exit)
  round(prop.table(ev.tbl),digits = 2)
  
  # 93965 deaths occured which translates to 14% of the individuals within the observation period
  # 576539 cases were censored
  ## adjusted to most current numbers
  
  ## ---------------- ##
  ## Age at exit      ##
  ## ---------------- ##
  
  retire <- retire %>% mutate(age.exit = end.date - FNAC)
  summary(retire$age.exit)

  #       Min.  1st Qu.  Median    Mean  3rd Qu.     Max. 
  #      55.89   69.00   74.66   75.51   81.50     94.99   ## newest population is slightly younger on average

  
  ## ---- ##
  ## Sex  ##
  ## ---- ##  
  
  retire$sex <- as.factor(retire$SEXO)

  # since there are only two sexes and no missings the following code is sufficient 
  retire$sex <- "male"    
  retire$sex[retire$SEXO==6] <- "female"
  retire$sex <- as.factor(retire$sex)
  relevel (retire$sex,'female') ->  retire$sex
  sex.tb <- table(retire$sex)
  round(100*sex.tb/sum(sex.tb), digits=2) 
  
    #   female         male 
    #   32.98%       67.02% 
    # ---------------------- #  
    # Values which probably represent the average differences in labor force participation
    # of this generations   -  slightly more men in the "new" data
  
  

  ## ---------------------------- ##
  ## Estado civil - civil status  ##
  ## ---------------------------- ##
  table(retire$ECIVIL) ## 3525 zeros which will be excluded for further analysisi
  # assuming the zeros mean that the civil status was unknown (missing), the should be deleted (to be determent)
  retire <- retire %>% filter(ECIVIL>0)
  # Change the categories
  retire$ECIVIL <- factor(retire$ECIVIL)
  retire$ECIVIL <- revalue(retire$ECIVIL, c("1"="single", "2"="married", "3"="widowed", 
                                                  "4"="divorced/sep", "5"="divorced/sep"))
  civ.stat.tbl <- table(retire$ECIVIL)
  round(100*(civ.stat.tbl/sum(civ.stat.tbl)),digits = 2)
  
  # single      married      widowed divorced/sep 
  #   8.31        81.26         7.90         2.54  # in percent (%)

  
  
  
  ## -------------------------- ##
  ## Education - highest degree ##
  ## -------------------------- ##
  table(retire$ESREAL)
  # 

  retire$ESREAL <- as.factor(retire$ESREAL)
  retire$ESREAL <- revalue(retire$ESREAL, c("1"="Illiterate", "2"= "Incomplete", "3"="Primary Educ.",
                                            "4"="Secondary Educ.", "5"="Secondary Educ.", "6"="Secondary Educ.",
                                            "7"="Secondary Educ.", "8"="Tertiary Educ.", "9"="Tertiary Educ.",
                                            "10"="Tertiary Educ."))
  
  deg.tbl <- table(retire$ESREAL)
  round(100*(deg.tbl/sum(deg.tbl)),digits = 2)
  
 #  Illiterate      Incomplete   Primary Educ.  Secondary Educ.   Tertiary Educ. 
 #        5.73           34.82           27.51            25.33             6.62      # in percent (%)
  
  ### And for the sake of a clean output I will collapse the education categories
  
  retire <- retire %>% mutate(EDU = factor(ifelse(ESREAL!="Secondary Educ." & ESREAL!="Tertiary Educ.",
                                                  "no or low education","Secondary/Tertiary Educ.")))
  retire <- within(retire, EDU <- relevel(EDU, ref = "no or low education"))
  round(prop.table(table(retire$EDU)), digits = 2)
  
  
  
  ## -------------------------- ##
  ## Occupation status en 2002  ##
  ## -------------------------- ##
  
  table(retire$SITP)
  ## What are the 9-er - see if they were the ones without job
  retire$SITP <- factor(retire$SITP)
  retire$SITP <- revalue (retire$SITP, c("1"="Businessmen/Profesional w. empl.", "2"="Businessmen/Profesional w.o. empl.",
                                         "3"="Employee w. indefinite contract", "4"="Employee with temporary contract",
                                         "5"="Others", "6"="Others", "9"="Not applicable"))
  
  occ.tbl <- table(retire$SITP)
  round(prop.table(occ.tbl,margin = NULL), digits = 2)
  
  
  ## -------------------------- ##
  ## Entry to retirement        ##
  ## -------------------------- ##
  
  summary(retire$retire.y)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 1967    1998    2006    2004    2011    2016
  
  ## We will deal with left truncation in the new study design

    # retire %>% ggplot(aes(x=retire.y))+
    # geom_bar()
    ## interesting pattern (what happened at the years with spikes?)
    
    
  ## -------------------------- ##
  ## Years in social security   ##
  ## -------------------------- ##
  
  hist(retire$contrib.years)
  summary(retire$contrib.years)
    #    Min.   1st Qu.  Median     Mean  3rd Qu.     Max. 
    #     0.00   24.00   35.00      32.11   41.00    98.00 
    # Median of 36 indicates that the ones how paid in social security were normally occupied for a long time
    # Could be a hint for the insider-outsider society that made it difficult for young people to enter the market
  
  ### What are the zeros?
  summary(subset(retire, contrib.years == 0, select=dis.pen))        # these cases probably need to be excluded
  
   #     ret.pen        
  # Min.   :   687.5  
  # 1st Qu.: 39026.6  
  # Median : 40172.4  
  # Mean   : 43580.7  
  # 3rd Qu.: 40172.4  
  # Max.   :257293.7  
  ### these are the stats for the ones with 0 contribution years = an error???
  
  nrow(subset(retire, contrib.years == 0, select=years.of.entry))
  nrow(subset(retire, contrib.years == 0 & sex=="female", select=years.of.entry))
  # 22061 (17494 females) - to be debated 
  

  ### Who are the ones with an extreme number of contribution years (outlier?)
  
  outlierKD(retire, contrib.years)      ### cool function using the "eval()" command
                                        ### (https://datascienceplus.com/rscript/outlier.R)
  
 # Outliers identified: 39 from 666243 observations
 # Proportion (%) of outliers: 0.00585372003908484
 # Mean of the outliers: 74.8974358974359
 # Mean without removing outliers: 32.1050247432243
 # Mean if we remove outliers: 32.1025196486362
  
  # set back to 1 graph panel for the graphic window
  par(mfrow = c(1, 1))
  
  # test for the highest not-outlier
  n <- length(retire$contrib.years)
  sort(retire$contrib.years,partial=n-1)[n-39]
  

  ## The suggested highest value which is not an outlier is 56 (cut the maximum contribution years at 60)
  
  ### Basically confirms that contributions times of more than 60 years are unlikely 
  ### (especially a contribution time of 98 years is probably a data error)
  
  retire <- retire %>% 
    ## Extract the 40 cases with more than 60 years of contribution (15-75,20-80 cases are highly rare already)
    filter(contrib.years<=60) %>% 
    ## also extract the 22061 cases with zero years of contribution will be excluded
    filter(contrib.years>=0.1) %>% 
    ## create a factor variable
    mutate(contrib.years = as.numeric(contrib.years)) %>% 
    mutate(con.y = factor(ifelse(contrib.years<20, "less than 20 years",
                                       ifelse(contrib.years<41, "20-40 years",
                                              "more than 40 years"))))
    
    # 644849 cases left  
      
    summary(retire$contrib.years)
    table(retire$con.y)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   # 1.0    25.0    35.0    33.2    41.0    60.0 
   #       20-40 years   less than 20 years   more than 40 years 
   #            369578                94654               179870
   
    
  ## -------------------------- ##
  ## Pension income             ##
  ## -------------------------- ##
  
  # back from cents to Euro per month (divide through 100)
  retire <- retire %>% mutate(ret.pen=ret.pen/100)
  # distribution
  hist(retire$ret.pen)
  summary(retire$ret.pen)
  #       Min.   1st Qu.   Median      Mean   3rd Qu.      Max. 
  #       0.01    595.80   745.50    958.70   1194.00   3085.00 
  # a large number of people seem to receive a kind of standard/minimum pension (between 500 and 650 Euro)
  
  
  
  
  retire <- retire %>% mutate(pensize = factor(ifelse(retire$ret.pen<500,"less than 500 Euro",
                                  ifelse(retire$ret.pen<1000, "500-999 Euro",
                                         ifelse(retire$ret.pen<2000, "1000-1999 Euro", "more than 2000 Euro")))))

  ## change reference for the pension size variable to the highest income category
  retire <- within(retire, pensize <- relevel(pensize, ref = "more than 2000 Euro"))
  
  
  ## ---------------------------------------- ##
  ## Constructed entry age to retirement      ##    
  ## ---------------------------------------- ##
  
  retire <- retire %>% mutate(ret.age = retire.y-FNAC)
  summary(retire$ret.age)
  # this doesn´t look right!
      outlierKD(retire,ret.age)                              ### Analysis ran again 03.10.17
     ## 20141 Outlier!!! = 3.1% of the cases
      # Outliers identified: 20141 from 644849 observations
      # Proportion (%) of outliers: 3.12336686573136
      # Mean of the outliers: 65.6456759843106
      # Mean without removing outliers: 63.9394510497807
      # Mean if we remove outliers: 63.8844412269412
      
   count(retire$ret.age[retire$ret.age<40])
   table(retire$con.y[retire$ret.age<40])   # tried with various variables but could not identify the problem
   hist(retire$FNAC[retire$ret.age<40])     # seems to be an error in the retire year variable
  
  # set back to 1 graph panel for the graphic window
  par(mfrow = c(1, 1))
  
  # I will choose a somewhat reasonable time frame from 45 to 85 (gives still enough room for extraordinary cases)
  retire <- retire %>% filter(ret.age>=45) %>% filter(ret.age<=85)
  hist(retire$ret.age, breaks = 39)
  ### probably close to the real distribution with many entries during age 65-66
  
  
  
  ## reshape into a categorical variable (early, in-time, late)
  retire <- retire %>% mutate(ret.age.c = factor(ifelse(ret.age<64.1,"early retirement",
                                                        ifelse(ret.age<66,"in time retirement","late retirement"))))
  
  ret.time.tbl <- table(retire$ret.age.c)
  round(100*(prop.table(ret.time.tbl)),digits = 2)
  
  #  early retirement  in time retirement    late retirement 
  #             35.48               56.79               7.73   # in percent (%)    ### slightly changed in current version
  
  
  
  table(retire$age>retire$age.exit)                        # 2800 cases (has changed!)
  summary(retire$age[retire$age>retire$age.exit])
  summary(retire$age.exit[retire$age>retire$age.exit])
  
  ### !!!
  ### Basically a transformation of the time variable for death/censorship
  hist(retire$age.exit[retire$age>retire$age.exit])
  summary(retire$end.date[retire$age>retire$age.exit])
  summary(retire$years.of.entry[retire$age>retire$age.exit])
  ret.test <- subset(retire, retire$age>retire$age.exit)
  ### These are the ones where entry and the end data is set to 2010.99 - but the entry age is finer defined through the birth year
  
  #### !!! For now excluded from the analysis
  
  retire <- retire %>% filter(age.exit>age)
  
  
  ## ---------------------------- ##
  ## Household size               ##
  ## ---------------------------- ##
  
  ## And the household size variable - living with partner or more household members
  retire <- retire %>% mutate(hh= factor(ifelse(NMIEM==2, "with partner only","larger household")))
  retire <- within(retire, hh <- relevel(hh, ref = "larger household"))
  
  
  ## review the distribution
  round(prop.table(table(retire$hh)), digits = 2)
  
  #  larger household   with partner only 
  #             0.72%               0.28%
  
  
  ## ---------------------------- ##
  ## TENEN - tenent regime        ##
  ## ---------------------------- ##
  t.tbl <- table(retire$TENEN)
  
  ## recoding of categories
  retire <- retire %>% mutate (TENEN = factor(ifelse( TENEN<=3,"Own","Other Form")))
  retire <- within(retire, TENEN <- relevel(TENEN, ref = "Other Form"))
  cbind(colnames(retire))
  colnames(retire)[25] <- "HousReg"
  
  t.tbl <- table(retire$HousReg)
  round(100*(prop.table(t.tbl)), digits=2)
  rm(t.tbl)
  
  #         Own       Other Form 
  #      90.78%            9.22%
  
  ## ---------------------------- ##
  ## NHAB - number of rooms       ##
  ## ---------------------------- ##
  
  table(retire$NHAB)
  
  retire <- retire %>% mutate(room = factor(ifelse(NHAB<=3, "3 or less rooms",
                                                    ifelse(NHAB<=5,"4-5 rooms",
                                                           ifelse(NHAB<=8, "6-8 rooms", "more than 8 rooms")))))
  
  NH.tbl <- table(retire$room)
  round(100*(prop.table(NH.tbl)),digits = 2)
  
  ### This variable is somewhat hard to interpret (is it a socioeconomic indicator?)
  
  # 3 or less rooms         4-5 rooms         6-8 rooms   more than 8 rooms 
  #             8.32             53.03             35.10              3.56  # in percent (%)
  
  
  ## -------------------------------------------- ##
  ## VEHIC - number of motor vehicles/cars        ##
  ## -------------------------------------------- ##
  
  table(retire$VEHIC)
  
  retire <- retire %>% mutate(car = factor(ifelse(VEHIC>=1,"car(s) available","no car")))
  retire <- within(retire, car <- relevel(car, ref = "car(s) available"))
  

  ####### -------------------------------------------------------------------------------------- #########

  
  
### 1.4 Some graphical checks
  
  
  ### --------------------- ###  
  ### Scanning the data     ###
  ### Visual intuitive test ###
  ### --------------------- ###

  # visualizing education/pension income
  PENED.plot <- ggplot(retire, aes(x=ret.pen, fill=ESREAL)) +
    geom_histogram(bins = 20)+
    labs(color="")+
    theme_bw()  
    
  # visualizing education/years of contributions
    YearED.plot <- ggplot(retire, aes(x=contrib.years, fill=ESREAL)) +
                geom_histogram(binwidth = 1)+
                  labs(color="")+
                  theme_bw()
  
  ### this plot could be interesting for the presentation (highlighting the groups in 4-5 different plots)  
  
 # visualizing education/pension years !!!
  YP.plot <- ggplot(retire, aes(x=contrib.years, y=ret.pen)) +
    geom_point(aes(color = ESREAL))+
    labs(color="")+
    theme_bw()
  
 # Visualize the pension income by entry age
  EA.PI.plot <- ggplot(retire, aes(x=contrib.years, y=ret.pen)) +
                geom_point(aes(color = ret.age.c))+
                labs(color="")+
                theme_bw()
  
  
  ## Contribution of contribution years 
  retire %>% ggplot(aes(x=con.y, fill=ESREAL))+
    geom_bar()
  
  ## Visualization of entry age by education
  ggplot(data=retire, aes(x=ret.age, fill=ESREAL))+
    geom_histogram(bins=35)
  
############################################################################################
  
  ### 1.4.1. Retirement variables in context
  
  ps.ra.tbl<- table(retire$pensize,retire$ret.age.c)
  round(prop.table(ps.ra.tbl,2), digits = 2)         # Column percentage
  round(prop.table(ps.ra.tbl,1), digits = 2)         # row percentage

  
  ps.cy.tbl <- table(retire$pensize,retire$conyear)
  round(prop.table(ps.cy.tbl,2),digits = 2)         # Column percentage
  
  
  ### 1.4.2. Distribution of events (deaths) by the retirement variables
  
  # visualizing event distribution 
  
  # event distribution by sex
  s.e.tbl <- table(retire$exit,retire$sex)
  round(prop.table(s.e.tbl), digits = 3)

    retire %>% ggplot(aes(x=sex,fill=exit))+
    geom_bar(stat = "count")
  rm(s.e.tbl)  
  
  # event distribution by education
  e.e.tbl <- table(retire$exit, retire$ESREAL)
  round(100*(prop.table(e.e.tbl,2)),digits = 3) # Column percentage (most deaths in the lowest education group)
  # plot
  retire %>% ggplot(aes(x=ESREAL,fill=exit))+
    geom_bar(stat = "count")
  rm(e.e.tbl) 

  # event distribution by pension size
  p.e.tbl <- table(retire$exit, retire$pensize)
  round(100*(prop.table(p.e.tbl, 2)),digits=2)   # column percentage (most deaths in the lowest income group)
  # plot
  retire %>% ggplot(aes(x=pensize,fill=exit))+
    geom_bar(stat = "count")
  rm(p.e.tbl)  
  

  
  #### save data set ready for the analysis
  
  save(retire, file='030_RetirementIND.RData')
  
  rm()
  

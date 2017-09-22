#### %%%%%%%%%%%%%%%%%%%%%% #####
#### SU 1 - Retirement data #####
#### %%%%%%%%%%%%%%%%%%%%%% #####

### ------------------------------------------------------------- ###
### 1. Data sets from Frans R file - Individual level data

  # 1.1 data set for the individual analysis of pensioners
  # 1.2 Tidy data
  # 1.3 Reorganize and rename categories
  # 1.4 Descriptive Stats

### ------------------------------------------------------------- ###

load("060_ss_cc2002.RData")
# load("060_ss_cc2002_old.RData")

### 0.1 load necessary packages
library(plyr)
library(tidyverse)
library(survival)
library(reshape)
library(forcats)

### 1.1 Data set for individual analysis - preparation

# Since it is possible that an individual has multiple pension spells (i.e. receives a disability pension before 
# reaching the eligibility age for retirement), the first step will be assign pensions to individuals. Therefore
# it is necessary to create a "wide" data structure with regard to the pension spells.


### 1.1.1 Joining the data sources

###    join the pension to the census individuals (using innerjoin = there are some individuals in the
###    social security data which were not in the 2001 census)

retire <- inner_join(ss.benefits,ss.benefits.c2002,by="kID") %>%  
  # create an age variable from the birth year
  mutate(entry.year = 2002) %>% 
  # age at 2002
  mutate(age = entry.year - FNAC) %>%
  # extract the age groups 40-80 (age at census year) & earliest entry in retirement in 2002
  filter(age>=40 & start.date>=2011) %>% filter(age<=80)


# 1.1.2 order by kID (because yu have an unordered long dataset and subset)

# easy way
retire <- retire[order(retire$kID) , ]

# see double entries = individuals with two different social security spells
# (remember "dublicated" function)
r.test <- retire[duplicated(retire[1]) | duplicated(retire[1], fromLast=TRUE),] 

  # 9120 cases receive more than one form of social security benefits

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

   # "end.cause" into a binary numeric variable
    retire <- retire %>% mutate(event=if_else(end.cause=="C",0,1))
    
   # tenen, SVIV, VEHIC - categories will be renamed in English at a later step

   retire$TENEN <- as.numeric(as.character(retire$TENEN))
   retire$SVIV <- as.numeric(as.character(retire$SVIV))
   retire$VEHIC <- as.numeric(as.character(retire$VEHIC))


####### --------------------------------------------------------------------- #########



### 1.2.1 Shape data in tidy format = only one row per individual (assigning different socsec benefits to same individual)
glimpse(retire)

retire <- retire %>% select(-benefit.type,-IMP2016, -end.cause) %>% 
          group_by(kID) %>% summarise_all(funs(max))


### 1.2.2 Further approach study population
  # - interested in entering in retirement (Jubilación) => individuals without retirement pension will be excluded
  # - to check for a possible health bias, we need the entry into disability if existent
  
  ## extract everybody who has retired between 2011 and 2015
  retire <- retire %>% filter(retire.y!=0) %>% 
    ## create a variable to account for health effects -  if someone receives disability pension before retiring
  mutate(health.st = ifelse(dis.y!=0 & dis.y<retire.y, 1, 0))
  
  table(retire$health.st) # 0=168861, 1=255 (0.15 % of the retirees are frail regarding this definition (DAHLY?))
  

  


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
  
  # 2776 deaths occured which means that 2% of the observed individual died within the observation period
  
  ## ---- ##
  ## Sex  ##
  ## ---- ##  
  
  retire$sex <- as.factor(retire$SEXO)
  retire %>% count(SEXO)
  # since there are only two sexes and no missings the following code is sufficient 
  retire$sex <- "male"    
  retire$sex[retire$SEXO==6] <- "female"
  retire$sex <- as.factor(retire$sex)
  relevel (retire$sex,'female') ->  retire$sex
  sex.tb <- table(retire$sex)
  round(100*sex.tb/sum(sex.tb), digits=2) 
  
    #   female        male 
    #  36.18 %     63.82 %
    # ---------------------- #  
    # Values which probably represent the average differences in labor force participation
    # of this generations

  ## ---------------------------- ##
  ## Estado civil - civil status  ##
  ## ---------------------------- ##
  retire %>% count(ECIVIL)
  # assuming the zeros mean that the civil status was unknown (missing), the should be deleted (to be determent)
  retire <- retire %>% filter(ECIVIL>0)
  # Change the categories
  retire$ECIVIL <- factor(retire$ECIVIL)
  retire$ECIVIL <- revalue(retire$ECIVIL, c("1"="single", "2"="married", "3"="widowed", 
                                                  "4"="divorced/sep", "5"="divorced/sep"))
  civ.stat.tbl <- table(retire$ECIVIL)
  round(100*(civ.stat.tbl/sum(civ.stat.tbl)),digits = 2)
  
  #            0       single      married      widowed divorced/sep 
  #         0.19         8.16        84.76         2.83         4.06

  
  
  
  ## -------------------------- ##
  ## Education - highest degree ##
  ## -------------------------- ##
  table(retire$ESREAL)
  # again zeros - for now they will be excluded
  retire <- retire %>% filter(ESREAL>0)
  retire$ESREAL <- as.factor(retire$ESREAL)
  retire$ESREAL <- revalue(retire$ESREAL, c("1"="Illiterate", "2"= "Incomplete", "3"="Primary Educ.",
                                            "4"="Secondary Educ.", "5"="Secondary Educ.", "6"="Secondary Educ.",
                                            "7"="Secondary Educ.", "8"="Tertiary Educ.", "9"="Tertiary Educ.",
                                            "10"="Tertiary Educ."))
  
  deg.tbl <- table(retire$ESREAL)
  round(100*(deg.tbl/sum(deg.tbl)),digits = 2)
  
  #      Illiterate      Incomplete   Primary Educ. Secondary Educ.  Tertiary Educ. 
  #           2.09%          21.71%         29.18%           37.00%         10.02%
  
  
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
  ## Years in social security   ##
  ## -------------------------- ##
  
  hist(retire$contrib.years)
  summary(retire$contrib.years)
    #    Min.   1st Qu.  Median     Mean  3rd Qu.     Max. 
    #    0.00    27.00    36.00    33.63   41.00    70.00
    # Median of 36 indicates that the ones how paid in social security were normally occupied for a long time
    # Could be a hint for the insider-outsider society that made it difficult for young people to enter the market
  ### What are the zeros?
  summary(subset(retire, contrib.years == 0, select=dis.pen))        # these cases are hard to understand
  nrow(subset(retire, contrib.years == 0, select=years.of.entry))    # 743 (705 females) - to be debated 
  
  retire$conyear <- factor(ifelse(retire$contrib.years<15,"less than 15 years",
                                    ifelse(retire$contrib.years<30,"15-29 years",
                                           ifelse(retire$contrib.years<45,"30-44 years","more than 45 years"))))
    
   
  ## -------------------------- ##
  ## Pension income             ##
  ## -------------------------- ##
  
  # back from cents to Euro per month (divide through 100)
  retire <- retire %>% mutate(ret.pen=ret.pen/100)
  # distribution
  hist(retire$ret.pen)
  summary(retire$ret.pen)
  #       Min.   1st Qu.   Median      Mean   3rd Qu.      Max. 
  #      9.321  615.300   788.900  1081.000  1401.000  3085.000 
  # a large number of people seem to receive a kind of standard/minimum pension (by 650)
  
  
  retire$pensize <- factor(ifelse(retire$ret.pen<500,"less than 500 Euro",
                                  ifelse(retire$ret.pen<1000, "500-999 Euro",
                                         ifelse(retire$ret.pen<2000, "1000-1999 Euro", "more than 2000"))))
  
  #### Change reference category for later analysis (ref=primary or secondary)
  retire <- within(retire, pensize <- relevel(pensize, ref = "500-999 Euro"))
  
  ## ---------------------------- ##
  ## Entry age to retirement      ##
  ## ---------------------------- ##
  retire <- retire %>% mutate(ret.age = retire.y-FNAC)
  summary(retire$ret.age)
  
  #     Min.  1st Qu.  Median    Mean 3rd Qu.    Max. 
  #    50.00   63.01   65.00   64.31   65.07   93.40
  
  ggplot(data=retire, aes(x=ret.age, fill=ESREAL))+
    geom_histogram(bins=35)
  
  ## reshape into a categorical variable (early, in-time, late)
  retire <- retire %>% mutate(ret.age.c = factor(ifelse(ret.age<64.1,"early retirement",
                                                        ifelse(ret.age<66,"in time retirement","late retirement"))))
  
  ret.time.tbl <- table(retire$ret.age.c)
  round(prop.table(ret.time.tbl),digits = 2)
  
  # early retirement  in time retirement    late retirement 
  #             0.34                0.58               0.08 
  
  
  ## ---------------------------- ##
  ## Socioeconomic status in 2002 ##
  ## ---------------------------- ##
  
  table(retire$CSE)
  ## too many categories - collapse responsible
  
  
  
  ## ---------------------------- ##
  ## TENEN - tenent regime        ##
  ## ---------------------------- ##
  
  ## ---------------------------- ##
  ## NHAB - number of rooms       ##
  ## ---------------------------- ##
  
  
  ## -------------------------------------------- ##
  ## VEHIC - number of motor vehicles/cars        ##
  ## -------------------------------------------- ##
  

  ####### -------------------------------------------------------------------------------------- #########

### 1.4 Descriptive Statistics
  
  
  ### --------------------- ###  
  ### Scanning the data     ###
  ### Visual intuitive test ###
  ### --------------------- ###
  
  # visualizing event distribution
  

  

  # visualizing education/pension income
  PENED.plot <- ggplot(retire, aes(x=ret.pen, fill=ESREAL)) +
    geom_histogram(bins = 20)+
    labs(color="")+
    theme_bw()  
    
  # visualizing education/pension years
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
  
############################################################################################
  
  ### 1.4.1. Retirement variables in context
  
  ps.ra.tbl<- table(retire$pensize,retire$ret.age.c)
  round(prop.table(ps.ra.tbl,2), digits = 2)         # Column percentage
  round(prop.table(ps.ra.tbl,1), digits = 2)         # row percentage

  
  ps.cy.tbl <- table(retire$pensize,retire$conyear)
  round(prop.table(ps.cy.tbl,2),digits = 2)         # Column percentage
  
  
  ### 1.4.2. Distribution of events (deaths) by the retirement variables
  
  # event distribution by sex
  s.e.tbl <- table(retire$exit,retire$sex)
  round(prop.table(s.e.tbl), digits = 3)

    retire %>% ggplot(aes(x=sex,fill=exit))+
    geom_bar(stat = "count")
  
  # event distribution by education
  
  # event distribution by occupation
  
  # event distribution by pension size
  
  # event distribution by entry age to retirement and years of contribution
  
  ### 1.4.4. Simple statistical tests for the event variable in combination with potential covariates
  
  ### 1.4.4. Descriptive tables for men and women
  
  
  

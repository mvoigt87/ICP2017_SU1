##### ----------------------------------------------------------------------------------------------------- #####
##### 010 - Descritptive Statistics from the HMD - South European countries
##### ----------------------------------------------------------------------------------------------------- #####


# LIBRARIES #
library(ggplot2)
library(gcookbook)
library(HMDHFDplus)
library(plyr)
library(grid)
library(gridExtra)
library(tidyr)
library(ggplot2)
library(readxl)
library(dplyr)

# set passworts
name.m <- "mathias.voigt4@uni-rostock.de"
name.f <- "m.voigt87@gmx.de"
pw.m <- "1320270854"
pw.f <- "43700"

### -------------------------------------------------- ###
### South European Countries LE at Birth and at age 50 ###
### -        -        -         -        -         -   ###
### and later for the life span disparity              ###


#### 1. Download lifetables from the HMD (total population)

## Lifetables Spain
LT.ESP <- readHMDweb(CNTRY="ESP", item="bltper_1x1", username=name.m, password=pw.m, fixup = T)

## Lifetables Italy
LT.ITA <- readHMDweb(CNTRY="ITA", item="bltper_1x1", username=name.m, password=pw.m, fixup = T)

## Lifetables Portugal
LT.PRT <- readHMDweb(CNTRY="PRT", item="bltper_1x1", username=name.m, password=pw.m, fixup = T)

## Lifetables Greece
LT.GRC <- readHMDweb(CNTRY="GRC", item="bltper_1x1", username=name.m, password=pw.m, fixup = T)

summary(LT.ESP)
# min year= 1908 max=2014
summary(LT.ITA)
# min year= 1872 max=2012
# 2012-1872 = 140

### 2. Binding the data together and show development for LE 0 and LE 65

## Spain

LT.ESP.x <- LT.ESP %>% select(ex,Age,Year) %>% mutate(country = "Spain") %>% 
          # subset only for the ex at birth (changeable)
          filter(Age == 0) %>% 
          # to assure the same end year (2012)
          filter(Year < 2013) %>% 
          # age won't be needed
          select(-Age)

## Italy

LT.ITA.x <- LT.ITA %>% select(ex,Age,Year) %>% mutate(country = "Italy") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 0) %>% 
  # age won't be needed
  select(-Age)

## Greece

LT.GRC.x <- LT.GRC %>% select(ex,Age,Year) %>% mutate(country = "Greece") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 0) %>% 
  # to assure the same end year (2012)
  filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Portugal

LT.PRT.x <- LT.PRT %>% select(ex,Age,Year) %>% mutate(country = "Portugal") %>% 
  # subset only for the ex at birth (changeable)
  filter(Age == 0) %>% 
  # to assure the same end year (2012)
  filter(Year < 2013) %>% 
  # age won't be needed
  select(-Age)

## Create a long dataset with all the countries

LE.all <- bind_rows(LT.ESP.x,LT.ITA.x) %>% bind_rows(LT.GRC.x) %>% bind_rows(LT.PRT.x) %>% 
          ## Cut to a uniform age range
          filter(Year>1980)

head(LE.all)
tail(LE.all)

rm(LT.ESP.x, LT.ITA.x, LT.GRC.x, LT.PRT.x)

### 3. Plot them

LE.all %>% ggplot() +
           # line plot
           geom_line(aes(x = Year, y = ex, color = country))  +
           geom_point(aes(x = Year, y = ex, color = country)) +
           scale_y_continuous(name = "Life expectancy at birth in years") +
           scale_color_discrete(name = "") +
           theme_bw()


### ------------------------------------------------ ###
###        Gender gap in LE the LT by sex            ###
### ------------------------------------------------ ###

###    Spain - Gender Gap        ###

### 1. Additionally download the female and male life table
LT.ESP.fem <- readHMDweb(CNTRY="ESP", item="fltper_1x1", username=name.m, password=pw.m, fixup = T)
LT.ESP.fem <- LT.ESP.fem %>% mutate(sex = "female") %>% 
              # extract only necessary variables
              select(-mx,-qx,-ax,-lx,-dx,-Lx,-Tx,-OpenInterval)
LT.ESP.mal <- readHMDweb(CNTRY="ESP", item="mltper_1x1", username=name.m, password=pw.m, fixup = T)
LT.ESP.mal <- LT.ESP.mal %>% mutate(sex = "male") %>% 
              # extract only necessary variables
              select(-mx,-qx,-ax,-lx,-dx,-Lx,-Tx,-OpenInterval)
head(LT.ESP.fem)

### 2. Combine data sets and calculate the gender gap in LE

# ESP.TOT <- LT.ESP %>% select(Year,Age, ex) %>% mutate(sex = "total") %>% 
#   bind_rows(LT.ESP.fem) %>%
#   bind_rows(LT.ESP.mal)
   
  
 ### create a Gap variable data set (female-male LE)
  ESP.GAP <- LT.ESP %>% select(Year,Age, ex) %>% mutate(sex = "total") %>%
             left_join(LT.ESP.fem, by= c("Year","Age")) %>% 
             left_join(LT.ESP.mal, by= c("Year","Age")) %>% 
             # now create a Gap variable and drop the unwanted variables
             mutate(GAP = ex.y - ex) %>% mutate(country = "Spain") %>% 
             select(-sex.x, -sex.y, -sex, -ex.x, -ex.y, -ex)

#### ------------------------------------------------------------------------------------------------ #####
   
             
   ###    Italy - Gender Gap        ###
             
   ### 1. Additionally download the female and male life table
       LT.ITA.fem <- readHMDweb(CNTRY="ITA", item="fltper_1x1", username=name.m, password=pw.m, fixup = T)
       LT.ITA.fem <- LT.ITA.fem %>% mutate(sex = "female") %>% 
       # extract only necessary variables
       select(-mx,-qx,-ax,-lx,-dx,-Lx,-Tx,-OpenInterval)
       LT.ITA.mal <- readHMDweb(CNTRY="ITA", item="mltper_1x1", username=name.m, password=pw.m, fixup = T)
       LT.ITA.mal <- LT.ITA.mal %>% mutate(sex = "male") %>% 
       # extract only necessary variables
       select(-mx,-qx,-ax,-lx,-dx,-Lx,-Tx,-OpenInterval)
       head(LT.ITA.fem)
             
   ### 2. Combine data sets and calculate the gender gap in LE
             
             # ITA.TOT <- LT.ITA %>% select(Year,Age, ex) %>% mutate(sex = "total") %>% 
             #   bind_rows(LT.ITA.fem) %>%
             #   bind_rows(LT.ITA.mal)
             
   ### create a Gap variable data set (female-male LE)
   ITA.GAP <- LT.ITA %>% select(Year,Age, ex) %>% mutate(sex = "total") %>%
           left_join(LT.ITA.fem, by= c("Year","Age")) %>% 
           left_join(LT.ITA.mal, by= c("Year","Age")) %>% 
           # now create a Gap variable and drop the unwanted variables
            mutate(GAP = ex.y - ex) %>% mutate(country = "Italy") %>% 
            select(-sex.x, -sex.y, -sex, -ex.x, -ex.y, -ex)
 
 #### ------------------------------------------------------------------------------------------------ ##### 
      
 ###    Greece - Gender Gap        ###     
            
 ### 1. Additionally download the female and male life table
            LT.GRC.fem <- readHMDweb(CNTRY="GRC", item="fltper_1x1", username=name.m, password=pw.m, fixup = T)
            LT.GRC.fem <- LT.GRC.fem %>% mutate(sex = "female") %>% 
              # extract only necessary variables
              select(-mx,-qx,-ax,-lx,-dx,-Lx,-Tx,-OpenInterval)
            LT.GRC.mal <- readHMDweb(CNTRY="GRC", item="mltper_1x1", username=name.m, password=pw.m, fixup = T)
            LT.GRC.mal <- LT.GRC.mal %>% mutate(sex = "male") %>% 
              # extract only necessary variables
              select(-mx,-qx,-ax,-lx,-dx,-Lx,-Tx,-OpenInterval)
            head(LT.GRC.fem)
            
            ### 2. Combine data sets and calculate the gender gap in LE
            
            # GRC.TOT <- LT.GRC %>% select(Year,Age, ex) %>% mutate(sex = "total") %>% 
            #   bind_rows(LT.GRC.fem) %>%
            #   bind_rows(LT.GRC.mal)
            
            ### create a Gap variable data set (female-male LE)
            GRC.GAP <- LT.GRC %>% select(Year,Age, ex) %>% mutate(sex = "total") %>%
              left_join(LT.GRC.fem, by= c("Year","Age")) %>% 
              left_join(LT.GRC.mal, by= c("Year","Age")) %>% 
            # now create a Gap variable and drop the unwanted variables
            mutate(GAP = ex.y - ex) %>% mutate(country = "Greece") %>% 
              select(-sex.x, -sex.y, -sex, -ex.x, -ex.y, -ex)

#### ------------------------------------------------------------------------------------------------ ##### 
        
    ###    Portugal - Gender Gap        ###     
            
        ### 1. Additionally download the female and male life table
        LT.PRT.fem <- readHMDweb(CNTRY="PRT", item="fltper_1x1", username=name.m, password=pw.m, fixup = T)
        LT.PRT.fem <- LT.PRT.fem %>% mutate(sex = "female") %>% 
              # extract only necessary variables
              select(-mx,-qx,-ax,-lx,-dx,-Lx,-Tx,-OpenInterval)
        LT.PRT.mal <- readHMDweb(CNTRY="PRT", item="mltper_1x1", username=name.m, password=pw.m, fixup = T)
        LT.PRT.mal <- LT.PRT.mal %>% mutate(sex = "male") %>% 
              # extract only necessary variables
              select(-mx,-qx,-ax,-lx,-dx,-Lx,-Tx,-OpenInterval)
        
        head(LT.PRT.fem)
            
      ### 2. Combine data sets and calculate the gender gap in LE
            
        # PRT.TOT <- LT.PRT %>% select(Year,Age, ex) %>% mutate(sex = "total") %>% 
        #   bind_rows(LT.PRT.fem) %>%
            #   bind_rows(LT.PRT.mal)
            
      ### 3. create a Gap variable data set (female-male LE)
          PRT.GAP <- LT.PRT %>% select(Year,Age, ex) %>% mutate(sex = "total") %>%
              left_join(LT.PRT.fem, by= c("Year","Age")) %>% 
              left_join(LT.PRT.mal, by= c("Year","Age")) %>% 
            # now create a Gap variable and drop the unwanted variables + create country
            mutate(GAP = ex.y - ex) %>% mutate(country = "Portugal") %>% 
            select(-sex.x, -sex.y, -sex, -ex.x, -ex.y, -ex)


###### --------------------------------------------------------------------------------------------- ######

### create the complete data set for ggplot
 
 GAP.TOT <- ESP.GAP %>% bind_rows(ITA.GAP) %>% bind_rows(GRC.GAP) %>% bind_rows(PRT.GAP) %>% 
          ### adjust the years (optional step)
          filter(Year>1900) #%>% filter(Year<2013)

 str(GAP.TOT)

            
 ## %%%% clean up %%%%%
 rm(LT.ESP.fem, LT.ESP.mal, LT.ITA.fem, LT.ITA.mal, LT.PRT.fem, LT.PRT.mal, LT.GRC.fem, LT.GRC.mal, ESP.GAP, ITA.GAP,
    PRT.GAP, GRC.GAP)            

##### ----------------------------------------------------------------------------------------- #####             
### 3. Plot female, male and overall life expectancy over time for e0 and e65 (exchangable)
# note: pipeline should not change the original data

 GAP.TOT %>% filter(Age == 0) %>% 
  ggplot(aes(x = Year, y = GAP, color = country)) +
  geom_line() +
  # change colors to the more intutive understanding
  scale_color_manual(values=c("green", "blue", "black","red"), name = "") +
  # change x-axis breaks and y axis title
  scale_x_continuous(breaks = c(1910,1930,1950,1970,1990,2010)) +
  scale_y_continuous(name = "Sex gap in life expectancy at birth")   +
  theme_bw()

 # %%% Plot for print (no colors)  
 GAP.TOT %>% filter(Age == 0) %>% 
   ggplot(aes(x = Year, y = GAP)) +
   geom_line(aes(linetype = country)) +
   # change x-axis breaks and y axis title
   scale_x_continuous(breaks = c(1910,1930,1950,1970,1990,2010)) +
   scale_y_continuous(name = "Sex gap in life expectancy at birth")   +
   scale_linetype_discrete(name = " ") +
   theme_bw()


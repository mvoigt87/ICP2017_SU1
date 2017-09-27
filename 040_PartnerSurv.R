### ------------------------------------------------------------------------------------------------- ###
###                     Partner Data - Descriptives, Analysis, Output                                 ###
### ------------------------------------------------------------------------------------------------- ###
#
##### 
### 1 Merging the partner data + Datamanagement + Simple statistical tests and graphical checks

### 2 Survival analysis for individuals in partnerships - (household income and widowhood effect)

### 3 Output-Tables

### ------------------------------------------------------------------------------------------------- ### 

### 0.1. Loading data set
load("030_RetirementIND.RData")
load("060_ss_cc2002.RData")

### 0.2 load necessary package
library(reshape)
library(plyr)
library(tidyverse)
library(survival)
library(forcats)
library(data.table)
library(broom)
library(stargazer)

### 1. Exploring and merging partner data

str(parned.c2002)

  # kID is the ID of an individual, who was married to an individual (their ID is kIDcon) 
  # in the ss.benefits data at 2002 and 2011. Therefore, the cleaned data of the retire data set could be
  # connected to the parned.c2002 data - keeping in mind that many variables have the same name

## 1.1. Select reasonable variables from retire and rename if necessary
cbind(colnames(retire))
retire <- subset(retire, select = c(1:7,10,11,17,25,28,30:49))

tbl_df(retire)

## 1.2. Prepare parned data 
cbind(colnames(parned.c2002))


part2 <- parned.c2002
 # add p to colnames to identify the partner variable
library(stringr)
colnames(part2)[3:15] <- str_c( colnames(part2)[3:15],"_p" )
tbl_df(part2)
  

## 1.3. Merge the two data sets - coupl
coupl <- inner_join(retire,part2, by="kID")
         ## 510653 cases

## quick check
 # civil status
 table(coupl$ECIVIL,coupl$ECIVIL_p)
 # some partner civil status do not coincide with individual - theoretically everybody should be married

 # sex
 table(coupl$sex,coupl$SEXO_p)
 ## 8 homosexual relationships
 
 # combinations with other pensioners
  id.a <- as.vector(coupl$kID)
  id.b <- as.vector(coupl$kIDcon)

  r.test <- as.data.frame(intersect(id.a,id.b))
 # 140762 individuals and their partners receive pensions at some point between 2002-2011
  
## 1.4. Create subcohort of people who both received a social security pension
  
  ## extract couples with social security pension
  colnames(r.test)[1] <- "kID"
  
  pen.coupl <- left_join(r.test,coupl, by="kID") %>% select(-CSE)
  
  ## and to also obtain knowledge about the household income, I will join the pension data from the partner
  r.small <- retire %>% select(kID,age,contrib.years, ret.pen, ret.age, ret.age.c,retire.y, event, exit, contrib.y.c, pensize) %>% 
    dplyr::rename(kIDcon=kID)
  colnames(r.small)[2:11] <- str_c( colnames(r.small)[2:11],"_p" )
  tbl_df(r.small)
  
  ### combine with pencoupl
  pen.coupl <- pen.coupl %>% left_join(r.small,by="kIDcon")
  
## 1.5. Calculate Household income for them
    pen.coupl <- pen.coupl %>% mutate(hhincome=ret.pen+ret.pen_p)

    # clean up:
    rm(part2,parned.c2002,ss.benefits,ss.benefits.c2002,id.a,id.b, r.test, r.small)
  
### 1.6 Datamanagement   
    
    # Edit variable number of household members
    table(pen.coupl$NMIEM)
    
    pen.coupl <- pen.coupl %>% mutate(hh= factor(ifelse(NMIEM==2, "with partner only",
                                                              ifelse(NMIEM<=7, "small household","large household"))))
   
    table(pen.coupl$hh)
    pen.coupl <- within(pen.coupl, hh <- relevel(hh, ref = "small household"))
    
    ## and the partner variables sex and education
    
    pen.coupl$sex_p <- as.factor(ifelse(pen.coupl$SEXO_p==1,"male","female"))
    
    pen.coupl$ESREAL_p <- as.factor(pen.coupl$ESREAL_p)
    pen.coupl$ESREAL_p <- revalue(pen.coupl$ESREAL_p, c("1"="Illiterate", "2"= "Incomplete", "3"="Primary Educ.",
                                              "4"="Secondary Educ.", "5"="Secondary Educ.", "6"="Secondary Educ.",
                                              "7"="Secondary Educ.", "8"="Tertiary Educ.", "9"="Tertiary Educ.",
                                              "10"="Tertiary Educ."))
    
    # interesting table
     # Assortative mating is important to consider when making assumptions about the household composition and income
    mix.ed <- table(pen.coupl$ESREAL,pen.coupl$ESREAL_p)
    round(prop.table(mix.ed,1), digits = 2) # row percentage
    
### ------------------------------------------------------------------------------------------------- ### 

    
    
### 2 Descriptive and Survival analysis for individuals in partnerships - (household income and widowhood effect)
    
    
## 2.1.
    
    ## ------ ##
    ##  event ##
    ## ------ ##
    
    # How do the partners deaths influence each other (of course dependent by age - just a check)
    ev.tbl <- table(pen.coupl$exit,pen.coupl$exit_p)
    round(prop.table(ev.tbl,2),2)
    chisq.test(ev.tbl)
    rm(ev.tbl)
    
    ## visual exploring exit by age and partner age
    pen.coupl %>% ggplot(aes(x=age,y=age_p)) +
      geom_point(aes(color=exit)) +
      scale_color_discrete(name = "") +
      facet_grid(. ~ sex)       
    
    
    ## ----- ##
    ##  sex  ##
    ## ----- ##
    
    s.e.tbl <- table(pen.coupl$exit,pen.coupl$sex)
    round(prop.table(s.e.tbl,2), digits = 2)
    rm(s.e.tbl)
    
    #             female   male
    #   censored   0.96%  0.87%
    #   dead       0.04%  0.13%
    
    pen.coupl %>% ggplot(aes(x=sex,fill=exit))+
      geom_bar(stat = "count")+
      scale_fill_discrete(name = "")              
    
    # histogram of age at exit distribution
    pen.coupl %>% ggplot(aes(x=age.exit,fill=exit))+
      geom_histogram(bins=40)+
      scale_fill_discrete(name = "") +
      facet_grid(. ~ sex)                               ## better to see differences by male and female
    
    
    ## ----------- ##
    ##  education  ##
    ## ----------- ##
    
    ed.tbl <- table(pen.coupl$exit,pen.coupl$ESREAL)
    ed.tbl2 <- table(pen.coupl$exit,pen.coupl$ESREAL_p)
    round(prop.table(ed.tbl,2),2)
    round(prop.table(ed.tbl2,2),2)
    rm(ed.tbl, ed.tbl2)
    ### distributions by own education and partners education are almost the same
    
    ## ------------------ ##
    ##  pension variables ##
    ## ------------------ ##
    
    p.tbl <- table(pen.coupl$exit, pen.coupl$pensize)
    round(prop.table(p.tbl,2),2)
    rm(p.tbl)
    ## not really strong differences
    
    p.tbl2 <- table(pen.coupl$pensize,pen.coupl$pensize_p)
    round(prop.table(p.tbl2,2),2)
    rm(p.tbl2)
    
    # household pension income                                      !!! not beautiful but interesting
    pen.coupl %>% ggplot(aes(x=hhincome, fill=exit)) +
                   geom_histogram(bins = 50) +
                   scale_fill_discrete(name = "")  +
                   facet_grid(.~ sex)
    
    # graph
    pen.coupl %>% ggplot(aes(x=))

### ------------------------------------------------------------------------------------------------- ### 

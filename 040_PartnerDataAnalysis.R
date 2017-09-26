### ------------------------------------------------------------------------------------------------- ###
###                     Partner Level Dataset - Descriptives and Survival Analysis                    ###
### ------------------------------------------------------------------------------------------------- ###
#
##### 
### 1 Testing and Cleaning the partner dataset + combining it with the census information

### 2 Descriptive tables and statistics

### 3 Survival analysis for the couples by household income

### 4 Output-Tables
### ------------------------------------------------------------------------------------------------- ### 

### 0.1. Loading data set
load("060_ss_cc2002.RData")

### 0.2 load necessary package
library(reshape)
library(tidyverse)
library(survival)
library(forcats)
library(data.table)
library(broom)
library(stargazer)

### 1.1 Test the properties of the partner data set and connect it to the census information

str(parned.c2002)

coupl <- parned.c2002

### If I got it right, the parned.c2002 dataset contains individuals who are cohabiting and married at two time
### points 2002 and 2011. Additionally one (or two of the partners has to receive a retirement pension)

# See if everybody got his partner
# (remember "dublicated" function)
coupl <- coupl[order(coupl$kID) , ]
r.test <- parned.c2002[duplicated(parned.c2002[1]) | duplicated(parned.c2002[2], fromLast=TRUE),] 

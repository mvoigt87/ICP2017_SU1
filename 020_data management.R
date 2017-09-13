#### %%%%%%%%%%%%%%%%%%%%%% #####
#### SU 1 - Retirement data #####
#### %%%%%%%%%%%%%%%%%%%%%% #####

### ------------------------------------------------------------- ###
### 1. Data sets from Frans R file - Individual level data

  # 1.1 Create data set for the individual analysis of pensioners
  # 1.2 
  # 1.3 Reorganize and rename categories
  # 1.4 Descriptive Stats

### ------------------------------------------------------------- ###

load("060_ss_cc2002.RData")

### 0.1 load necessary packages
library(tidyverse)
library(survival)

### 1.1 Data set for individual analysis

PENIN <- ss.benefits.c2002 %>% left_join(ss.benefits, by="kID")

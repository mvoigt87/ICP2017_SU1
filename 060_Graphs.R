### ---------------------------------------
### Graphs for publication or presentations
### ---------------------------------------

### 0.1. Loading data set
load("041_RETPART.RData")
# ----------------------------- 
# 831231 individuals
# -----------------------------

### 0.2 load necessary package
library(plyr)
library(reshape)
library(tidyverse)
library(survival)
library(survminer)
library(forcats)
library(data.table)
library(broom)
library(stargazer)
library(stringr)


# Visual test - Graph Income distribution

DINTBL.sw <- aggregate(pen.coupl$INCOME,by=list(pen.coupl$SEXO),FUN=mean)

# 1  female  636.993 Euro
# 2    male  800.6834 Euro

### Descriptive graphs

### Income distribution in GREY tones

INC.DIS <- pen.coupl %>% dplyr::mutate(grp.mean = ifelse(SEXO=="female",636.993,800.6834)) %>% 
  ggplot(aes(x=INCOME, color=SEXO)) +
  geom_histogram(aes(y=50*..density..),fill="white", alpha=0.5, position="dodge",binwidth = 50) +
  geom_vline(aes(xintercept=grp.mean, color=SEXO),
             linetype="dashed") +
  scale_color_manual(values=c("#000000", "#A9A9A9"), name=" ") +
  scale_x_continuous(name="Montly Public Pension Income (in €)", limits = c(0,2500)) +
  scale_y_continuous(name = "Relative Frequency") +
  theme_bw()

INC.DIS <- INC.DIS + theme(legend.position = c(0.85, 0.85))
INC.DIS

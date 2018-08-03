##### Graphs for the presentation at the IPC

# 0.1. packages

library(ggplot2)
library(gridExtra)
library(broom)
library(forestplot)


## 1.1 Extract the data from the cox model which has been estimated parallel


##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #####
load("031_RETIND.RData")


## male population
cox.male.a <- coxph(Surv(time=entry.age.r,
                         time2=exit.age,
                         event=event) ~ pensize + ESREAL5 + FNAC + DIS + civil.status + mobil +
                      HousReg + hh,
                    data=subset(retire, SEXO=="male"))

## female population
cox.female.b <- coxph(Surv(time=entry.age.r,
                           time2=exit.age,
                           event=event) ~ pensize + ESREAL5 + FNAC + DIS + civil.status + mobil +
                        HousReg + hh,
                      data=subset(retire, SEXO=="female"))

summary(cox.male.a)

summary(cox.female.b)

##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #####




## 1.1.2 male data

cox.male <- tidy(cox.male.a) %>% 
  ## add a column with exponentiated estimates
  mutate(SEX="male") %>% 
  mutate(exp.estimate = exp(estimate)) %>%
  mutate(exp.conf.low = exp(conf.low)) %>% 
  mutate(exp.conf.high = exp(conf.high)) %>% 
  ## add the category names
  mutate(categories = c("1000-1999  Eur/month","650-999 Eur/month","< 650 Eur/month",
                        "Tertiary Ed.","Secondary Ed.","Primary Ed.","Birth year (cohort)", "Received Disability Pension",
                        "Not married","Widowed","No car avail." ,"Owns house/apt.",
                        "Rents house/apt.","Lives only with partner"))

## 1.1.2 female data

cox.female <- tidy(cox.female.b) %>% 
 ## add a column with exponentiated estimates
 mutate(SEX="female") %>% 
  mutate(exp.estimate = exp(estimate)) %>%
  mutate(exp.conf.low = exp(conf.low)) %>% 
  mutate(exp.conf.high = exp(conf.high)) %>% 
 ## add the category names
 mutate(categories = c("1000-1999  Eur/month","650-999 Eur/month","< 650 Eur/month",
                        "Tertiary Ed.","Secondary Ed.","Primary Ed.","Birth year (cohort)", "Received Disability Pension",
                        "Not married","Widowed","No car avail." ,"Owns house/apt.",
                        "Rents house/apt.","Lives only with partner"))




## 1.2. Generate a dataset with the extracted values

# cox.dat <- bind_rows(cox.male,cox.female) %>% 
#   select(categories, SEX, estimate, p.value, conf.low, conf.high) %>% 
#   ## for readability exponentiated estimates are used
#   mutate(exp.estimate = exp(estimate)) %>%
#   mutate(exp.conf.low = exp(conf.low)) %>% 
#   mutate(exp.conf.high = exp(conf.high))


  ### Extract variables
cox.male <- cox.male[c(1:6,9:11),]
cox.female <- cox.female[c(1:6,9:11),]




### 1.3. Add absolute and relative counts

## Check numbers

table(retire$pensize[retire$SEXO=="male"])
round(prop.table(table(retire$pensize[retire$SEXO=="male"])), digits = 3)

table(retire$ESREAL5[retire$SEXO=="male"])
round(prop.table(table(retire$ESREAL5[retire$SEXO=="male"])), digits = 3)

table(retire$car[retire$SEXO=="male"])
round(prop.table(table(retire$car[retire$SEXO=="male"])), digits = 3)

table(retire$pensize[retire$SEXO=="female"])
round(prop.table(table(retire$pensize[retire$SEXO=="female"])), digits = 3)



###############################################################################################


# tabletext<-cbind(
#   c("Categories", "1000-1999  Eur/month","650-999 Eur/month","< 650 Eur/month",
#     "Tertiary Ed.","Secondary Ed.","Primary Ed.","Not married", "Widowed", "No Car"),
#   c("Rel. Frequ.", "29.0%", "34.0%","26.8%","7.2%", "26.9%", "27.7%","18.3%", "4.2%","24.9%"),
#   c("Hazard Ratio", "1.11", "1.06", "1.15", "0.96", "0.97", "0.98", "1.20", "1.15", "1.22"))
# 
# 
# forestplot(tabletext, txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Arial"),
#                                                     gpar(fontfamily = "",
#                                                          col = "#000044")),
#                                        ticks = gpar(fontfamily = "", cex=1),
#                                        xlab  = gpar(fontfamily = "Arial", cex = 1.5)),
#            cochrane_male,new_page = TRUE, is.summary=c(TRUE,rep(FALSE,24)), boxsize = .20,
#            hrzl_lines = list("1"=gpar(lwd=1, columns=1:3,col="#000044"),"2"=gpar(lwd=1, columns=1:3,col="#000044")), xlog=TRUE,
#            col=fpColors(box="royalblue",line="darkblue")) 






# tabletext<-cbind(
#   c("Categories", "1000-1999  Eur/month","650-999 Eur/month","< 650 Eur/month",
#     "Tertiary Ed.","Secondary Ed.","Primary Ed.","Not married", "Widowed", "No Car"),
#   c("Rel. Frequ.", "16.1%", "21.7%","57.9%","5.7%", "20.8%", "26.3%","34.2%", "16.7%","38.1%"),
#   c("Hazard Ratio", "1.05", "0.95", "1.08", "0.87", "0.93", "0.94", "1.17", "1.17", "1.07"))


# forestplot(tabletext, txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Arial"),
#                                                     gpar(fontfamily = "",
#                                                          col = "#000044")),
#                                        ticks = gpar(fontfamily = "", cex=1),
#                                        xlab  = gpar(fontfamily = "Arial", cex = 1.5)),
#            cochrane_female,new_page = TRUE, is.summary=c(TRUE,rep(FALSE,24)), boxsize = .20,
#            hrzl_lines = list("1"=gpar(lwd=1, columns=1:3,col="#000044"),"2"=gpar(lwd=1, columns=1:3,col="#000044")), xlog=TRUE,
#            col=fpColors(box="royalblue",line="darkblue")) 



#### &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& ####


##### NEW CODIGO


#### MEN

# Cochrane data from the 'rmeta'-package
cochrane_male <-
  structure(list(
    mean  = c(NA, 1.11, 1.03, 1.11, NA, 0.97, 0.97, 0.98, NA, 1.19, 1.17, NA, 1.22, NA),
    lower = c(NA, 1.07, 1.01, 1.07, NA, 0.94, 0.96, 0.97, NA, 1.17, 1.14, NA, 1.20, NA),
    upper = c(NA, 1.14, 1.07, 1.15, NA, 1.06, 0.99, 0.99, NA, 1.21, 1.19, NA, 1.23, NA)),
    .Names = c("HR", "lower", "upper"),
    row.names = c(NA, -11L),
    class = "data.frame")


#### WOMEN

# Cochrane data from the 'rmeta'-package
cochrane_female <-
  structure(list(
    mean  = c(NA, 0.99, 0.92, 1.04, NA, 0.89, 0.91, 0.93, NA, 1.18, 1.16, NA, 1.07, NA),
    lower = c(NA, 0.90, 0.84, 0.95, NA, 0.83, 0.88, 0.90, NA, 1.15, 1.13, NA, 1.04, NA),
    upper = c(NA, 1.09, 1.01, 1.14, NA, 0.95, 0.94, 0.95, NA, 1.22, 1.20, NA, 1.09, NA)),
    .Names = c("HR", "lower", "upper"),
    row.names = c(NA, -11L),
    class = "data.frame")



tabletext_mf<-cbind(
  c(" ", "1000-1999  Eur/month","650-999 Eur/month","< 650 Eur/month","Ref. > 2000 Eur/month",
    "Tertiary Ed.","Secondary Ed.","Primary Ed.","Ref. Incomplete Ed.", "Not married", "Widowed", "Ref. Married", "No Car", "Ref. 1 or more"),
  c("female", "15.7%", "24.9%","55.8%"," ","4.8%", "18.6%", "26.2%"," ","34.3%", "18.8%"," ","41.2%"," "),
  c("male", "27.4%", "34.1%","29.4%"," ","6.3%", "25.0%", "27.7%"," ","18.7%", "4.5%"," ","27.3%"," "))
  
  ### Table too big with these
  #c("HR -  female", "1.05", "0.95", "1.08", "0.87", "0.93", "0.94", "1.17", "1.17", "1.07"),
  #c("HR - male", "1.11", "1.06", "1.15", "0.96", "0.97", "0.98", "1.20", "1.15", "1.22"))


forestplot(tabletext_mf, 
           legend_args = fpLegend(pos = list(x=.15, y=0.10), 
                                  gp=gpar(col="#CCCCCC", fill="#F9F9F9")),
           legend = c("Male", "Female"), new_page = TRUE,
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           boxsize = .25, # We set the box size to better visualize the type
           line.margin = .1, # We need to add this to avoid crowding
           mean = cbind(cochrane_male[, "HR"], cochrane_female[, "HR"]),
           lower = cbind(cochrane_male[, "lower"], cochrane_female[, "lower"]),
           upper = cbind(cochrane_male[, "upper"], cochrane_female[, "upper"]),
           clip =c(-.125, 2), zero = 1, 
           col=fpColors(box=c("navyblue", "orange")),
           xlab="Hazard Ratios")






#### &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& ####
#### Partner Data
#### &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& ####






#### !!!!!! Run 041_PartnerSurv2 before applying the code below

COX.MALE <- tidy(COX.MALE)

COX.MALE <- COX.MALE %>% mutate(exp.estimate = exp(estimate)) %>%
  mutate(exp.conf.low = exp(conf.low)) %>% 
  mutate(exp.conf.high = exp(conf.high)) %>% 
  ## add the category names
  mutate(categories = c("HH inc. 1000-2000 Euro/month","HH inc. $<$ 1000 Euro/month", "Received Disability Pension", "Tertiary Educ.",
                        "Secondary Educ.", "Primary Educ.", "Birth Cohort", "Lost Partner", "$>$10 years older",
                        "$>$ 10 years younger", "1-10 years older", "1-10 years younger", "Disability partner", "Tertiary Educ. Partner",
                        "Secondary Educ. Partner", "Primary Educ. Partner", "No Cars Available", "Own House/Aptm.",
                        "Rent House/Aptm.", "Lives only with Partner", "Breadwinner"))

COX.MALE <- COX.MALE[c(1:2,4:6,21,17,13),]

C_male <-
  structure(list(
    mean  = c(NA, 1.37, 4.78, NA, 0.81, 0.85, 0.93, NA, 2.11, NA, 1.28, NA, 1.52, NA),
    lower = c(NA, 1.29, 4.46, NA, 0.73, 0.78, 0.88, NA, 2.08, NA, 1.25, NA, 1.49, NA),
    upper = c(NA, 1.46, 5.11, NA, 0.90, 0.93, 0.98, NA, 2.14, NA, 1.31, NA, 1.55, NA)),
    .Names = c("HR", "lower", "upper"),
    row.names = c(NA, -11L),
    class = "data.frame")




### Women
COX.FEMALE <- tidy(COX.FEMALE)

COX.FEMALE <- COX.FEMALE %>% mutate(exp.estimate = exp(estimate)) %>%
  mutate(exp.conf.low = exp(conf.low)) %>% 
  mutate(exp.conf.high = exp(conf.high)) %>% 
  ## add the category names
  mutate(categories = c("HH inc. 1000-2000 Euro/month","HH inc. $<$ 1000 Euro/month", "Received Disability Pension", "Tertiary Educ.",
                        "Secondary Educ.", "Primary Educ.", "Birth Cohort", "Lost Partner", "$>$10 years older",
                        "$>$ 10 years younger", "1-10 years older", "1-10 years younger", "Disability partner", "Tertiary Educ. Partner",
                        "Secondary Educ. Partner", "Primary Educ. Partner", "No Cars Available", "Own House/Aptm.",
                        "Rent House/Aptm.", "Lives only with Partner", "Breadwinner"))

COX.FEMALE <- COX.FEMALE[c(1:2,4:6,21,17,13),]


C_female <-
  structure(list(
    mean  = c(NA, 1.09, 1.67, NA, 1.00, 1.06, 1.06, NA, 1.98, NA, 1.10, NA, 1.73, NA),
    lower = c(NA, 0.99, 1.55, NA, 0.80, 0.95, 0.98, NA, 1.89, NA, 1.05, NA, 1.69, NA),
    upper = c(NA, 1.19, 1.80, NA, 1.21, 1.17, 1.14, NA, 2.06, NA, 1.15, NA, 1.78, NA)),
    .Names = c("HR", "lower", "upper"),
    row.names = c(NA, -11L),
    class = "data.frame")





###### PLOT

tabletext_mf2<-cbind(
  c(" ", "1000-1999  Eur/month","< 1000 Eur/month","Ref. > 2000 Eur/month","Tertiary Ed.",
   "Secondary Ed.","Primary Ed.","Ref. Incomplete Ed.","Breadwinner","Ref. Equal or less", "No Car", "Ref. 1 or more",
   "Disability", "Ref. No Disability"),
  c("female", "72.3%", "13.3%"," ","3.2%","16.3%", "27.8%"," ", "6.1%"," ","25.0%"," ", "24.5%"," "),
  c("male", "78.1%", "14.0%", " ","4.5%","18.2%", "26.9%"," ","29.3%"," ","25.0%"," ","26.3%", " "))




#c("HR -  female", "1.05", "0.95", "1.08", "0.87", "0.93", "0.94", "1.17", "1.17", "1.07"),
#c("HR - male", "1.11", "1.06", "1.15", "0.96", "0.97", "0.98", "1.20", "1.15", "1.22"))


forestplot(tabletext_mf2, 
           legend_args = fpLegend(pos = list(x=.85, y=0.95), 
                                  gp=gpar(col="#CCCCCC", fill="#F9F9F9")),
           legend = c("Male", "Female"), new_page = TRUE,
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           boxsize = .25, # We set the box size to better visualize the type
           line.margin = .1, # We need to add this to avoid crowding
           mean = cbind(C_male[, "HR"], C_female[, "HR"]),
           lower = cbind(C_male[, "lower"], C_female[, "lower"]),
           upper = cbind(C_male[, "upper"], C_female[, "upper"]),
           clip =c(-.125, 2.5), zero = 1,
           col=fpColors(box=c("navyblue", "orange")),
           xlab="Hazard Ratios")

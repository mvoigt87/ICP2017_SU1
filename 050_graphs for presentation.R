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
cox.zph( cox.male.a)
summary(cox.female.b)
cox.zph( cox.female.b)
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

#### MEN

# Cochrane data from the 'rmeta'-package
cochrane_male <-
  structure(list(
    mean  = c(NA, 1.11, 1.06, 1.15, 0.96, 0.97, 0.98, 1.20, 1.15, 1.22),
    lower = c(NA, 1.07, 1.02, 1.11, 0.92, 0.95, 0.96, 1.17, 1.12, 1.20),
    upper = c(NA, 1.16, 1.10, 1.20, 1.00, 0.99, 1.00, 1.23, 1.19, 1.24)),
    .Names = c("HR", "lower", "upper"),
    row.names = c(NA, -11L),
    class = "data.frame")

tabletext<-cbind(
  c("Categories", "1000-1999  Eur/month","650-999 Eur/month","< 650 Eur/month",
    "Tertiary Ed.","Secondary Ed.","Primary Ed.","Not married", "Widowed", "No Car"),
  c("Rel. Frequ.", "29.0%", "34.0%","26.8%","7.2%", "26.9%", "27.7%","18.3%", "4.2%","24.9%"),
  c("Hazard Ratio", "1.11", "1.06", "1.15", "0.96", "0.97", "0.98", "1.20", "1.15", "1.22"))


forestplot(tabletext, txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Arial"),
                                                    gpar(fontfamily = "",
                                                         col = "#000044")),
                                       ticks = gpar(fontfamily = "", cex=1),
                                       xlab  = gpar(fontfamily = "Arial", cex = 1.5)),
           cochrane_male,new_page = TRUE, is.summary=c(TRUE,rep(FALSE,24)), boxsize = .20,
           hrzl_lines = list("1"=gpar(lwd=1, columns=1:3,col="#000044"),"2"=gpar(lwd=1, columns=1:3,col="#000044")), xlog=TRUE,
           col=fpColors(box="royalblue",line="darkblue")) 





#### WOMEN

# Cochrane data from the 'rmeta'-package
cochrane_female <-
  structure(list(
    mean  = c(NA, 1.05, 0.95, 1.08, 0.87, 0.93, 0.94, 1.17, 1.17, 1.07),
    lower = c(NA, 0.93, 0.85, 0.96, 0.81, 0.89, 0.91, 1.12, 1.12, 1.04),
    upper = c(NA, 1.18, 1.07, 1.21, 0.96, 0.98, 0.97, 1.21, 1.22, 1.10)),
    .Names = c("HR", "lower", "upper"),
    row.names = c(NA, -11L),
    class = "data.frame")

tabletext<-cbind(
  c("Categories", "1000-1999  Eur/month","650-999 Eur/month","< 650 Eur/month",
    "Tertiary Ed.","Secondary Ed.","Primary Ed.","Not married", "Widowed", "No Car"),
  c("Rel. Frequ.", "16.1%", "21.7%","57.9%","5.7%", "20.8%", "26.3%","34.2%", "16.7%","38.1%"),
  c("Hazard Ratio", "1.05", "0.95", "1.08", "0.87", "0.93", "0.94", "1.17", "1.17", "1.07"))


# forestplot(tabletext, txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Arial"),
#                                                     gpar(fontfamily = "",
#                                                          col = "#000044")),
#                                        ticks = gpar(fontfamily = "", cex=1),
#                                        xlab  = gpar(fontfamily = "Arial", cex = 1.5)),
#            cochrane_female,new_page = TRUE, is.summary=c(TRUE,rep(FALSE,24)), boxsize = .20,
#            hrzl_lines = list("1"=gpar(lwd=1, columns=1:3,col="#000044"),"2"=gpar(lwd=1, columns=1:3,col="#000044")), xlog=TRUE,
#            col=fpColors(box="royalblue",line="darkblue")) 



#### &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& ####



# 
# forestplot(tabletext, txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Arial"),
#                                                     gpar(fontfamily = "",
#                                                          col = "#000044")),
#                                        ticks = gpar(fontfamily = "", cex=1),
#                                        xlab  = gpar(fontfamily = "Arial", cex = 1.5)),
#            cochrane_male,new_page = TRUE, is.summary=c(TRUE,rep(FALSE,24)), boxsize = .20,
#            hrzl_lines = list("1"=gpar(lwd=1, columns=1:3,col="#000044"),"2"=gpar(lwd=1, columns=1:3,col="#000044")), xlog=TRUE,
#            col=fpColors(box="royalblue",line="darkblue")
#            ) 
# 
# 
# 
# cochrane_male <- cochrane_male %>% mutate(sex="male")
# cochrane_female <- cochrane_female %>% mutate(sex="female")
# 
# cochrane <- bind_rows(cochrane_male,cochrane_female)



##### NEW CODIGO


tabletext_mf<-cbind(
  c(" ", "1000-1999  Eur/month","650-999 Eur/month","< 650 Eur/month",
    "Tertiary Ed.","Secondary Ed.","Primary Ed.","Not married", "Widowed", "No Car"),
  c("female", "16.1%", "21.7%","57.9%","5.7%", "20.8%", "26.3%","34.2%", "16.7%","38.1%"),
  c("male", "29.0%", "34.0%","26.8%","7.2%", "26.9%", "27.7%","18.3%", "4.2%","24.9%"))
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
  mutate(categories = c("HH inc. 1000-2000 Euro/month","HH inc. $<$ 1000 Euro/month", "$<$ 20 years contr.",
                        "$>$ 40 years contr.", "Received Disability Pension", "Tertiary Educ.",
                        "Secondary Educ.", "Primary Educ.", "Birth Cohort", "Lost Partner", "$>$10 years older",
                        "$>$ 10 years younger", "1-10 years older", "1-10 years younger", "parnter contr. $<$ 20 years",
                        "partner contr. $>$ 40 years", "Disability partner", "Tertiary Educ. Partner",
                        "Secondary Educ. Partner", "Primary Educ. Partner", "No Cars Available", "Own House/Aptm.",
                        "Rent House/Aptm.", "Lives only with Partner", "Breadwinner"))

COX.MALE <- COX.MALE[c(1:2,6:8,21,25,17),]

C_male <-
  structure(list(
    mean  = c(NA, 1.39, 6.01, 0.79, 0.85, 0.93, 1.27, 2.22, 1.89),
    lower = c(NA, 1.27, 5.46, 0.69, 0.78, 0.86, 1.21, 2.12, 0.94),
    upper = c(NA, 1.51, 6.62, 0.91, 0.93, 1.00, 1.33, 2.32, 3.78)),
    .Names = c("HR", "lower", "upper"),
    row.names = c(NA, -11L),
    class = "data.frame")




### Women
COX.FEMALE <- tidy(COX.FEMALE)

COX.FEMALE <- COX.FEMALE %>% mutate(exp.estimate = exp(estimate)) %>%
  mutate(exp.conf.low = exp(conf.low)) %>% 
  mutate(exp.conf.high = exp(conf.high)) %>% 
  ## add the category names
  mutate(categories = c("HH inc. 1000-2000 Euro/month","HH inc. $<$ 1000 Euro/month", "$<$ 20 years contr.",
                        "$>$ 40 years contr.", "Received Disability Pension", "Tertiary Educ.",
                        "Secondary Educ.", "Primary Educ.", "Birth Cohort", "Lost Partner", "$>$10 years older",
                        "$>$ 10 years younger", "1-10 years older", "1-10 years younger", "parnter contr. $<$ 20 years",
                        "partner contr. $>$ 40 years", "Disability partner", "Tertiary Educ. Partner",
                        "Secondary Educ. Partner", "Primary Educ. Partner", "No Cars Available", "Own House/Aptm.",
                         "Rent House/Aptm.", "Lives only with Partner", "Breadwinner"))

COX.FEMALE <- COX.FEMALE[c(1:2,6:8,21,25,17),]


C_female <-
  structure(list(
    mean  = c(NA, 1.03, 1.70, 1.01, 1.09, 1.01, 1.16, 1.61, 0.75),
    lower = c(NA, 0.89, 1.40, 0.76, 0.92, 0.89, 1.07, 1.38, 0.36),
    upper = c(NA, 1.20, 2.07, 1.33, 1.29, 1.15, 1.25, 1.87, 1.58)),
    .Names = c("HR", "lower", "upper"),
    row.names = c(NA, -11L),
    class = "data.frame")





###### PLOT

tabletext_mf2<-cbind(
  c(" ", "1000-1999  Eur/month","< 1000 Eur/month","Tertiary Ed.",
   "Secondary Ed.","Primary Ed.","Breadwinner", "No Car", "Disability"),
  c("female", "72.0%", "11.8%","4.3%","19.1%", "27.9%", "6.1%","21.6%", "16.7%"),
  c("male", "76.4%", "6.8%","6.2%","20.5%", "26.6%", "31.0%","21.6%", "0.4%"))
#c("HR -  female", "1.05", "0.95", "1.08", "0.87", "0.93", "0.94", "1.17", "1.17", "1.07"),
#c("HR - male", "1.11", "1.06", "1.15", "0.96", "0.97", "0.98", "1.20", "1.15", "1.22"))


forestplot(tabletext_mf2, 
           legend_args = fpLegend(pos = list(x=.85, y=0.85), 
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

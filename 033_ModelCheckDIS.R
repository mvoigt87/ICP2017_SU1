### Model test disabled individuals
### -------------------------------

## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##
## $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ##


### Testing possibly inverse effects of disability

ret.sin.dis <- retire %>% filter(DIS==0)
ret.con.dis <- retire %>% filter(DIS==1)

# see graphically if there are substantial differences
retire %>% ggplot(aes(x=pensize,fill=as.factor(event))) +
  geom_bar(stat = "count") +
  facet_grid(.~ DIS)+
  scale_fill_discrete(name = "") # at least visually there are more people with high disability rents and higher risk

round(prop.table(table(retire$pensize[retire$DIS==0],retire$event[retire$DIS==0]),2),3)

round(prop.table(table(retire$pensize[retire$DIS==1],retire$event[retire$DIS==1]),2),3)

## biggest difference in the group 1000-1999 € per month (more people in general in that group)
## NO indication of a flipped gradient!!!

# percentage disabled
nrow(ret.con.dis)/nrow(retire) # 25% of all individuals

## Sin
## ---
cox.male.sd <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                       FNAC + civil.status + hh,
                     data=subset(ret.sin.dis, SEXO=="male"))                   ### For men more significant results


cox.female.sd <- coxph(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                         FNAC + civil.status + hh,
                       data=subset(ret.sin.dis, SEXO=="female"))                 ### For women still not conclusive


Gomp.sd <- flexsurvreg(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ pensize + SEXO + ESREAL5 + mobil + HousReg + 
                         civil.status + hh, data = ret.sin.dis,
                       dist = "gompertz")

## Con
## ---

cox.male.cd <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                       FNAC + civil.status + hh,
                     data=subset(ret.con.dis, SEXO=="male"))                  ###  complete inconclusive (insign.)


cox.female.cd <- coxph(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ pensize + ESREAL5 + mobil + HousReg + 
                         FNAC + civil.status + hh,
                       data=subset(ret.con.dis, SEXO=="female"))               ### for women the inverse 
### relationship seems to hold

Gomp.cd <- flexsurvreg(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ pensize + SEXO + ESREAL5 + mobil + HousReg + 
                         civil.status + hh, data = ret.con.dis,
                       dist = "gompertz")

### Now for the couple data
coup.sin.dis <- pen.coupl %>% filter(DIS==0)
coup.con.dis <- pen.coupl %>% filter(DIS==1)

# see graphically if there are substantial differences
pen.coupl %>% ggplot(aes(x=HHINC.4,fill=as.factor(event))) +
  geom_bar(stat = "count") +
  facet_grid(.~ DIS)+
  scale_fill_discrete(name = "")


round(prop.table(table(pen.coupl$pensize[pen.coupl$DIS==0],pen.coupl$event[pen.coupl$DIS==0]),2),3)

round(prop.table(table(pen.coupl$pensize[pen.coupl$DIS==1],pen.coupl$event[pen.coupl$DIS==1]),2),3)

## Same as for the full data set - but same conclusion: No sign of a flipped gradient


## Sin
## ---
cox.male.sd <- coxph(Surv(time=entry.age.r,
                          time2=exit.age,
                          event=event) ~ HHINC.4 + ESREAL5 + mobil  +  HousReg +
                       DIS_p + ESREAL5_p + hijo + bw,
                     data=subset(coup.sin.dis, SEXO=="male"))                   ### low income groups strong disadv


cox.female.sd <- coxph(Surv(time=entry.age.r,
                            time2=exit.age,
                            event=event) ~ HHINC.4 + ESREAL5 + mobil  +  HousReg +
                         DIS_p + ESREAL5_p + hijo + bw,
                       data=subset(coup.sin.dis, SEXO=="female"))               ### For women still not conclusive!

GOMP.log <- flexsurvreg(Surv(time=entry.age.r,
                             time2=exit.age,
                             event=event) ~ log(hhincome) + SEXO + ESREAL5 + mobil + HousReg +  p.surv +
                          log(FNAC) + DIS_p + ESREAL5_p + hijo + bw, data = coup.sin.dis,
                        dist = "gompertz")



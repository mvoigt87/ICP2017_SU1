### Playing with the Gompertz model


#### Fit Gompertz model with log income

## 1) Parametric survival model -  log income
GOMP.MALE.A.C <- flexsurvreg(Surv(time=entry.age.r,
                                  time2=exit.age,
                                  event=event) ~ log(hhincome), data = subset(pen.coupl, SEXO="male"),
                             dist = "gompertz")

GOMP.MALE.A.C




## 2) Parametric survival model -  log income
GOMP.FEMALE.A.C <- flexsurvreg(Surv(time=entry.age.r,
                                    time2=exit.age,
                                    event=event) ~ log(hhincome), data = subset(pen.coupl, SEXO="female"),
                               dist = "gompertz")

GOMP.FEMALE.A.C



# survival curve
plot(GOMP.MALE.A.C, xlim=c(65,100))
lines(GOMP.FEMALE.A.C)
# hazard
plot(GOMP.MALE.A.C, xlim=c(65,100),ylim=c(-0.1,1),type = "hazard")


##### FULL MODELS

## 3d) Parametric survival model with log(income)  - there seem to be some kind of problem with the log income
GOMP.FEMALE.C.C <- flexsurvreg(Surv(time=entry.age.r,
                                    time2=exit.age,
                                    event=event) ~ log(hhincome) + ESREAL5 + mobil  +  HousReg +  p.surv + DIS + FNAC +
                                 DIS_p + ESREAL5_p + hijo + bw, data = subset(pen.coupl, SEXO="female"),
                               dist = "gompertz")

GOMP.FEMALE.C.C




### For Males - model fit

GOMP.MALE.A.C

# estimated shape and scale
a <- 
shape <- 0.1308294
rate <- 0.00000731
# vector of quantiles
time<-seq(65,100,0.1)

### Base survival - Gompertz function (http://www.statsathome.com/2017/06/07/fitting-non-linear-groth-curves-in-r/)

gompertz <- function(time, a, mu, lambda){
  y <- a*exp(-exp(mu*exp(1)/a*(lambda-time)+1))
  return(data.frame(time=time, y=y))
}

fit <- gompertz(time = time, a = 2 ,mu = shape ,lambda = rate)

plot(fit)




# Gompertz survival function
Gomp.Surv <- function(time,a=0,b=0,c=0){
  y(t) <- a*exp(-b*exp(-c*t))
  return(y)
}  

fit <- Gomp.Mod(t=pen.coupl$exit.age, b= 0.131,c=0.000007)







### model fit


fit.gompertz <- function(data, time){
  d <- data.frame(y=data, t=time)
  
  # Must have at least 3 datapoints at different times
  if (length(unique(d$t)) < 3) stop("too few data points to fit curve")
  
  # Pick starting values ###
  i <- which.max(diff(d$y))
  starting.values <- c(a=max(d$y), 
                       mu=max(diff(d$y))/(d[i+1,"t"]-d[i, "t"]), 
                       lambda=i)
  print("Starting Values for Optimization: ")
  print(starting.values)
  ##########################
  
  formula.gompertz <- "y~a*exp(-exp(mu*exp(1)/a*(lambda-t)+1))"
  nls(formula.gompertz, d, starting.values)
}
require(data.table)
require(ggplot2)
rm(list = ls())

## Funciones auxiliares ---------------------------------------------
#' NA2Cero 
NA2Cero <- function(e) { ifelse(is.na(e),0,e)}


###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## EXTRACCIÓN Y PREPARACIÓN DE LOS DATOS -------------------------------------
##     de la POBLACION PARTICIPANTE   
## 1. Transpone (despliega) la información de pensiones sobre cada pensionista
## 2. Recupera algunas variables censales de la cohorte de Jubilados, y
##    elimina los jubilados no censados en 2001. 
##    Y aquellos censados en colectivos para los que no se recogio informacion social:
##        ESREAL, ECIVIL, SITU == 0
###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load("~/repos/Dropbox/compartidas/csic/TesisMathias/DatosRegistroAndalucia/IO/060_ss_cc2002.RData")
# load("../060_ss_cc2002.RData")

levels(vital.folow.up$cause)[3] <-  "end.observation"  # mistake 

class(ss.benefits.c2002)
ss.benefits.c2002[ECIVIL==0, .(SEXO,NACI,ECIVIL,ESREAL,SITU,NMIEM )] # En Individuos que residen 
                                                                      # en colectivos: no se dispone de 
                                                                      # variables: ECIVIL, ESREAL, SITU ..

ss.benefits.c2002[ECIVIL==0,.N]  ## 5412 residian en colectivos: residencias, cuarteles... 
                                 ## Estos individuos se sacan del analisis por tener datos-faltantes 

### * despliga "pensiones": "decast" -------------------------------
summary(ss.benefits)
ss.benefits$n.benefit <- NULL   # All 1

names(ss.benefits)

levels(ss.benefits$benefit.type) <- c("Disability","Retirement","Widowhood")

setnames(ss.benefits, c("IMP2016","years.of.entry"),
                      c("income","years.start.follow"))

dcast(ss.benefits, kID  ~ benefit.type, value.var = c("contrib.years","income",
                                                     "years.start.follow","start.date","end.date","end.cause"
                                                     )) -> sscc
sum(duplicated(sscc$kID))  # 0 ok 


## Incidence: There is no "contrib.years" or exists when it should not exist (casualties)
sscc[income_Retirement>0,.N,  keyby=.(contr= contrib.years_Retirement>0 )] -> pp; pp[,rel:=N/sum(N)] ; pp   # 26.876 incidencias: 4% NC inferior a encuestas
sscc[income_Disability>0,.N,, keyby=.(contr= contrib.years_Disability >0 )] -> pp; pp[,rel:=N/sum(N)] ; pp  #  5.449 incidencias   1.2 % 
sscc[income_Widowhood>0,.N,   keyby=.(contr= contrib.years_Widowhood>0 )]   -> pp; pp[,rel:=N/sum(N)] ; pp  #  2.507 incidencias   0.5%

sscc[, contrib.years_Disability:=NULL]
sscc[, contrib.years_Widowhood :=NULL]

sscc[is.na(end.cause_Disability),end.cause_Disability:='|']
sscc[is.na(end.cause_Retirement),end.cause_Retirement:='|']
sscc[is.na(end.cause_Widowhood),end.cause_Widowhood:='|']

### compacta las causa de fin de las pensiones
sscc[ , end.cause  := paste(end.cause_Disability,end.cause_Retirement,end.cause_Widowhood, sep='')]
sscc[ , end.cause2 := sapply(strsplit(end.cause,'|'), function(e) { paste(sort(unique(e[e!='|']) ), collapse = '') }) ]

# sscc[,.N,end.cause]
# sscc[,.N,end.cause2]

sscc[,':='(end.cause_Retirement=NULL,end.cause_Disability=NULL,end.cause_Widowhood=NULL)]


## Pasa "income" a Euros en lugar de centesimas de Euros 

sscc[, ':='(income_Disability= round(NA2Cero(income_Disability)/100,2),
            income_Retirement=round(NA2Cero(income_Retirement)/100,2),
            income_Widowhood =round(NA2Cero(income_Widowhood)/100,2))]
     
merge(ss.benefits.c2002[ECIVIL!=0,.(kID, FNAC, SEXO, ECIVIL, ESREAL, SITU, TENEN, VEHIC, CONPARHIJ, NMIEM  )],
      sscc,
      by='kID') -> sscc 

sum(duplicated(sscc$kID))  # 0 no duplicados ok, ya que solo un tipo de prestación 

## * Comprueba posibles inconsistencias edades  ---------------------------------
sscc[,.N, keyby=.(gedad=ifelse(FNAC<1976,'+35.en.2011','<35.en.2011'))]  ## 

#++++++++++++++++++++++++++++
# * Etiquetado de los factores ------------------------------------
#++++++++++++++++++++++++++++
sscc[,.N, keyby=.(ECIVIL)]

etSEXO  <- c('Men','Women')
etECIVIL <- c ("Single",
   "Married",
   "Widower",
   "Separated",
   "Divorced")
etSITU   <- c ("Students",
               "Working",
               "Looking for the first job",
               "Unemployed",
               "Disability Pensioners",
               "Pensioners of widow or orphanhood",
               "Retirement Pensioners",
               "Another situation1",
               "Another situation2",
               "Doing or sharing homework",
               "Other situation")


etESREAL <- c ("Illiterates",
   "Without studies",
   "Primary education",
   "Lower secondary education",
   "Upper secondary education",
   "Basic Vocational training",
   "Advanced Vocational training",
   "Short-cycle tertiary education",
   "Bachelor's degree",
   "Doctorate")  
  


sscc[,':='( ECIVIL=factor(as.integer(ECIVIL),labels = etECIVIL),
            ESREAL=factor(as.integer(ESREAL),labels = etESREAL),
            SITU=factor(as.integer(SITU),labels = etSITU),
            SEXO=factor(as.integer(SEXO),labels = etSEXO)
)]

require(car)   #  agregación y recodificación
expRecode<-  "1='Illiterate';2='Without studies';3='Primary Educ.';4:7='Secondary Educ.';else='Tertiary Educ.'"
sortLevel <- rev(c('Illiterate','Without studies','Primary Educ.','Secondary Educ.','Tertiary Educ.'))
sscc[, ESREAL5:=factor(recode(as.integer(ESREAL),expRecode), levels = sortLevel)]

sscc[,TENEN := factor(ifelse( TENEN<=3,"Own",  ifelse(TENEN==4, "Rent","Other form")))]

sscc[,VEHIC := ifelse(VEHIC==1,"1 car",
                      ifelse(VEHIC==2, "2 cars",
                          ifelse(VEHIC==3,"3 cars","no car")))]

setnames(sscc,c('TENEN','VEHIC'),c('HousReg','car'))

sscc$car <- factor(sscc$car)


### Lista los nacidos despues de 1960 
sscc[,.(.N,  Widowhood=sum(income_Widowhood>0),
             Disability=sum(income_Disability>0),
             Retirement=sum(income_Retirement>0)),
     keyby=.(generation=ifelse(FNAC>1960,'After 1959', 'before 1960'))]  
 #  generation       N Widowhood Disability Retirement
 #  After 1959  120416     25115      96135        344
 # before 1960 1320317    424213     357344     680367

# 344 posibles incosistencias (Jubilados con menos de < 50 en 2011)
                    # de momento mejor quitarlos
sscc[ (FNAC>1960  & income_Retirement>0),.N]
sscc[!(FNAC>1960  & income_Retirement>0)] -> sscc 


##++++++++++++++++++++++++++++++++-
## * Enlaza participantes con seguimientos vitales -------------------------
##++++++++++++++++++++++++++++++++-
## suprimo campos utiles y no redundantes 
cbind(names(vital.folow.up))
selecciona <- c(2,5,6,1,10,13,14) 
names(vital.folow.up)[selecciona]
vital.folow.up[,c(selecciona), with=F]

merge(sscc, vital.folow.up[,c(selecciona), with=F] ) -> sscc2


sscc <- sscc2   ## Pasa DE 1440389 a 1449331  (faltan unos 50 seguimientos vitales ...)
rm(sscc2)

### borra objetos sobrantes, libera memoria
rm(parned.c2002, ss.benefits,ss.benefits.c2002,vital.folow.up)
rm(etECIVIL,etESREAL,etSEXO,etSITU,expRecode,sortLevel,pp,selecciona,NA2Cero)

### * Borro algunas inconisistencias --------------
## Bajas anterior al comienzo del seguimiento 1-1-2011
sscc[(end.date_Retirement - years.start.follow_Retirement) <= 0, .N]  # 3445 bajas anteriores al comienzo de estudio: 1-En-2011
sscc[end.date_Disability - years.start.follow_Disability <= 0, .N]    # 3415 
sscc[end.date_Widowhood - years.start.follow_Widowhood <= 0, .N]      # 3853

sscc[(end.date_Retirement - years.start.follow_Retirement <= 0) |  
       (end.date_Disability - years.start.follow_Disability <= 0) |
       (end.date_Widowhood - years.start.follow_Widowhood <= 0)]$kID-> retrasos  

length(retrasos)# 10227   # suprimo: son notificaciones retrasadas
sscc[!(kID %in% retrasos) ] ->  sscc   # suprimidos 

## * Modifica years.start.follow para que sea date.start.follow ---------------------
## Si el alta es el mismo año de comienzo del seguimiento, 
## usa la fecha de alta  como comienzo del seguimiento, si no comienza
## el seguimiento a principio de años de 1ª observación

sscc[,years.start.follow_Retirement:= 
       ifelse( floor(years.start.follow_Retirement) == floor(start.date_Retirement),
               start.date_Retirement,years.start.follow_Retirement  )  ]

sscc[,years.start.follow_Disability:= 
       ifelse( floor(years.start.follow_Disability) == floor(start.date_Disability),
               start.date_Disability,years.start.follow_Disability  )  ]

sscc[,years.start.follow_Widowhood:= 
       ifelse( floor(years.start.follow_Widowhood) == floor(start.date_Widowhood),
               start.date_Widowhood,years.start.follow_Widowhood  )  ]


##  Depuracion algunas incositencias en fechas de defuncion censura ---
## Fallecidos en BDLP pero no en pensiones: Concordancias ...
table(sscc$end.cause2, sscc$cause)   
#      death out.migration end.observarion
# C    26814         12974         1167437
# CD      55             2               1
# D   230881          1906             261

## No esta muy mal, la concordancia..  pero hay que depurar para evitar inconsistencias

table(sscc$end.cause2, sscc$cause)  
sscc[end.cause2=='C' & cause =='death', c(1,2,22,25,27,28)] # mayoria Defunciones despues de 2016
##  Depura estos datos: 
##  + Convierte "data.out" mayores de  2016 a maxima "end.date" y "cause" a "end.observation":
sscc[data.out>=2016, ':='( data.out = pmax(end.date_Retirement,end.date_Widowhood,end.date_Disability, na.rm = T),
                              cause = 'end.observation') ]
sscc[end.cause2=='C' & cause =='death', c(1,2,21:23,25,27,28)] #


### * Posibles Falso Positivos en IDP ---------------------
###  las defunciones más antiguas (anteriores al comienzo del seguimiento)
###  Pueden ser "falsos positivos" en la identificación: 
###  del IDP asignado bien al: registro  censal, al del MNP o al de la SS
###  Son  643 (0,04%), los escluyo del estudio ....
sscc[end.cause2=='C' & cause =='death' & data.out<2011 , .N  ]  # 643
sscc[end.cause2=='C' & cause =='death' & data.out<2011 , c(1,2,21:23,24,25,27,28)  ]  ## !! se estan perdiendo notificaciones de bajas ...

sscc[!(end.cause2=='C' & cause =='death' & data.out<2011)    ]   -> sscc  ## Borro lo casos con defunciones antiguas en MNP

### * Retrasos en la notificación de las defunciones  ------

hist(sscc[end.cause2=='C' & cause =='death' & data.out<end.date_Retirement]$data.out   )

### Mayoria son cercana a 2016 (son retrasos en la notificacion de la defuncion a la SS)
####     En estos casos pon "end.cause2" a valor 'D' y en "end.date_##" pon los datos de data.out 
sscc[end.cause2=='C' & cause =='death' & data.out<end.date_Retirement, c(1,2,21:23,24,25,27,28) ]
sscc[end.cause2=='C' & cause =='death' , .N                   ]  # 4729
sscc[end.cause2=='C' & cause =='death' & data.out>2015  , .N  ]  # 3234

sscc[end.cause2=='C' & cause =='death' &  floor(data.out) == 2012, c(1,2,21:23,24,25,27,28)  ]  ## !! se estan perdiendo notificaciones de bajas ...
                                                                       ##    hay censuras por perdida de seguimiento
                                                                       ##    que parecen por defunción: corregir

### lista casos en función de distancia entre fechas 
### 72 casos mas de un año y 99 < de un año  (171) y 4558 casos las discrepacias menores de un año
### son debidas a icidencia de gestion administrativa 

sscc[end.cause2=='C' & cause =='death' &  
       (data.out-pmax(end.date_Disability,end.date_Retirement,end.date_Widowhood, na.rm = T)) > 1 , 
     c(1,2,21:23,24,25,27,28)  ]  ## !! se estan perdiendo notificaciones de bajas ...

sscc[end.cause2=='C' & cause =='death',.N]  # 4729
## En estos  4729 casos vamos a hacer que prevalezca la informacion de la BDLP sobre defunciones,
##  cambio end.cause (pero no end.date_) ya que la varaible temporal que debo de usar en 
## el analisis es "data.out" y las causa es "cause"

sscc[end.cause2=='C' & cause =='death',
     ':='(end.cause2='D', end.cause = gsub('C','D',end.cause))] ## modifica los datos

## * Perdida de seguimiento antes de su comienzo ----------------


sscc[(data.out-pmax(years.start.follow_Disability,
                    years.start.follow_Retirement,
                    years.start.follow_Widowhood,na.rm=T)<=0),.N] # 6407 Casos 

sscc[(data.out-pmax(years.start.follow_Disability,
                    years.start.follow_Retirement,
                    years.start.follow_Widowhood,na.rm=T)<=0),
     c(1,15:17,21:23,27,24,25,28)] -> pp

table(pp$cause)
# death   out.migration end.observation 
#   794            5585              28 

pp[cause=='death']  # 794
pp[cause=='death',.N, keyby=.(floor(data.out))]   # Mayoria de casos  
                                                  # finales 2010 y comienzo del 2012
                                                  # incidencia de cierre administrativo del año.

pp[cause=='out.migration',.N, keyby=.(floor(data.out))]   # 5585:  salidas de la region mayoria antes de 2011

pp[cause=='end.observation',.N, keyby=.(floor(data.out))] # 1 del 2011 y resto de 2015
pp[cause=='end.observation',] # Alta el último dias de año 2015 

## Borro estas incidencias menores 

sscc[kID %in% pp$kID,.N]    #    6.407
sscc[!(kID %in% pp$kID)]  -> sscc  # Elimina 6.407 registros con incosistencias 


## * Borra registros con datos faltantes fecha/montante de pension ----

sscc[is.na(years.start.follow_Retirement) & income_Retirement>0, .N ]  #   18 casos
sscc[is.na(years.start.follow_Disability) & income_Disability>0, .N ]  # 1075   "
sscc[is.na(years.start.follow_Widowhood) & income_Widowhood>0, .N ]    # 1044   "

sscc[is.na(years.start.follow_Widowhood) & income_Widowhood>0,c(1,15,12,16,13,17,14,27,24)] # sin fecha de comienzo viudedad
sscc[is.na(years.start.follow_Disability) & income_Disability>0,c(1,15,12,16,13,17,14,27,24)] # sin fecha de comienzo incapacidad

### Borra registros con informacion incompleta 

sscc[(is.na(years.start.follow_Widowhood) & income_Widowhood>0) |
     (is.na(years.start.follow_Disability) & income_Disability>0 ) |
     (is.na(years.start.follow_Retirement) & income_Retirement>0), 
     .N ]    # 2132 registros para borras 

sscc[!((is.na(years.start.follow_Widowhood) & income_Widowhood>0) |
       (is.na(years.start.follow_Disability) & income_Disability>0 ) |
       (is.na(years.start.follow_Retirement) & income_Retirement>0))  ] -> sscc



## * Graba el objeto depurado ---------

# save(sscc,file = '010_sscc.RData')



# #### * Distribución de las pensiones por sexo, generacion y tipo de pensión --------------------
# retire[is.na(years.start.follow_Retirement),.N] # 18 incidencias retire[is.na(years.start.follow_Retirement),.N] # 18 incidencias 
# melt(sscc, id.vars = c('kID','FNAC','SEXO'),
#      measure.vars = c("income_Disability",   "income_Retirement",  "income_Widowhood" ),
#      variable.name = 'Type', value.name = 'Income'
# ) -> pp
# pp[Income>0] -> pp
# levels(pp$Type) <-  sapply(strsplit(levels(pp$Type),'_'), function(e) e[2])  
# ggplot()+ 
#   geom_vline(xintercept = c(seq(1930,1960,by =5)), color='violet', size =.3, linetype=2) +
#   geom_vline(xintercept = c(1937), color='blue', size =.4, linetype=2) +
#   geom_bar(data =pp, aes(x=FNAC, colour=Type) )  +
#  facet_grid(Type ~ SEXO   , scale='free') + theme_bw()  +   theme(legend.position="none") -> ggpp
#  
# png(file='010_WithSSPensions.png', width = 1080 , height = 800 )
# ggpp + 
#   ggtitle('Persons wiht a Social Security Pensions', subtitle = 'By type of benefit and birth generation')
# dev.off()

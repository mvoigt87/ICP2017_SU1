### Download data on cohorts, fu, and ses (from census)


## ==========================================================
## Procesar:  Fichero censal
## ==========================================================
# instalar 'memisc'  si fuera necesario
if(!require(readr)){  install.packages("readr")  ; require(readr)}
if(!require(memisc)){  install.packages("memisc")  ; require(memisc)}

rm(list = ls())

##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
## Importa datos del fichero plano de microdatos censales y las etiquetas de
## lectura para SPSS. Se usa el paquete "memisc" y la clase "data.set"
## de dicho paquete 
##--------------------------------------------------------------------------

### ==========================
### Descarga de microdatos de 
### la muestra andaluza 
### ==========================
# if (!dir.exists('Andalusia.data')) { dir.create('Andalusia.data')}

download.file('http://www.juntadeandalucia.es/institutodeestadisticaycartografia/descarga/longevidad/andalucia/datosandalucia.zip',
              destfile = 'datosandalucia.zip')
unzip('datosandalucia.zip',list = T)  
unzip('datosandalucia.zip' )
file.remove('datosandalucia.zip')


### ==========================
### Lectura de metainformación 
### ==========================
url.spss <- 'http://www.juntadeandalucia.es/institutodeestadisticaycartografia/descarga/longevidad/andalucia/mandalucia.sps'

# readLines( url.spss, encoding = 'latin1' ) -> sp
readLines( 'mandalucia.sps', encoding = 'latin1' ) -> sp

## Modificaciones menores de la metainformacion leida
# 1º borra linea en blancoy los comentarios
sp[! (grepl('^ *$',sp) | grepl('^\\*',sp)) ] -> sp

## Borra linea de SAVE 
sp[!grepl('^.*SAVE OUTFILE.*$',sp) ] -> sp
## BORRA FILE="***"
gsub('FILE=".+"','',sp) -> sp

## Quita formatos númericos con DECIMALES, R::memisc no los lee bien:  (F,1)
sub(' \\(F,1\\)','',sp) -> sp


# Separa por punto y final ----------------------
cortes <- grepl('\\.$',sp)
(1:length(cortes))[cortes] -> cortes
data.frame(inicio=c(1,cortes[-length(cortes)]+1),fin=cortes)-> cortes
cortes
#    inicio fin
# 1      1  63
# 2     64 125
# 3    126 176
# 4    177 188 <---- hasta aquí, instrucciones para leer ma.txt
#
# 5    189 198
# 6    199 207
# 7    208 211 <--- 3 últimos bloques para leer sma.txt

##---------------------------------------------------------------
## + Los 4 primeros bloques de cortes son instrucciones relativas
##   al fichero con la información CENSAL (ma.txt)
## + Los 3 últimos bloques de cortes son instrucciones relativas
##   al fichero de seguimientos  (sma.txt)
##---------------------------------------------------------------

#### --------------------------------------
#### Lectura de ma.txt (censo)
#### --------------------------------------
for (i in 1:4) {
  oo <- paste0('./datosandalucia/oo',i)
  assign(oo,sp[cortes[i,'inicio']:cortes[i,'fin']])
  # write_lines(get(oo), paste0('',oo,'.sps'))
  write.table(get(oo), paste0('',oo,'.sps'), 
              quote = F, row.names = F, col.names = F)
}
rm(oo)

ma  <- spss.fixed.file( "./datosandalucia/ma.txt",
                        columns.file="./datosandalucia/oo1.sps",
                        varlab.file="./datosandalucia/oo2.sps",
                        codes.file="./datosandalucia/oo3.sps",
                        missval.file="./datosandalucia/oo4.sps")


subset(ma) -> ma
show(ma)  
summary(ma)

length(ma$viv) #  740407

# dsView(ma)
# codebook(ma)  # informe algo largo


# El factor de elevación esta multiplicado por 10^6
# y la fecha de nacimiento por 10
# (se puede usar felev=10, sin modificaciones significativas en resultados)
save(ma,file='./datosandalucia/010_ma.RData')
file.remove('./datosandalucia/oo1.sps','./datosandalucia/oo2.sps','./datosandalucia/oo3.sps','./datosandalucia/oo4.sps')
##---------------------------------------------------------------------

#### --------------------------------------
#### Lectura de sma.txt (seguimientos de cohorte censal)
#### --------------------------------------

cortes2 <- cortes[5:7,]
rbind(cortes2,data.frame(inicio=0,fin=0))  -> cortes2
cortes2

i <- 4
for (i in 1:4) {
  oo <- paste0('oo',i)
  assign(oo,sp[cortes2[i,'inicio']:cortes2[i,'fin']])
  # write_lines(get(oo), paste0('',oo,'.sps'))
  write.table(get(oo), paste0('./datosandalucia/',oo,'.sps'), 
              quote = F, row.names = F, col.names = F)
}
rm(oo)



sma  <- spss.fixed.file("./datosandalucia/sma.txt",
                        columns.file="./datosandalucia/oo1.sps",
                        varlab.file="./datosandalucia/oo2.sps",
                        codes.file="./datosandalucia/oo3.sps",
                        missval.file="./datosandalucia/oo4.sps")


show(sma)    # 742889 observaciones

subset(sma) -> sma
show(sma)  

summary(sma)


## Recuerda que los años entran en año con un decimal (x 10)
save(sma,file='./datosandalucia/010_sma.RData')
file.remove('./datosandalucia/oo1.sps','./datosandalucia/oo2.sps','./datosandalucia/oo3.sps','./datosandalucia/oo4.sps')


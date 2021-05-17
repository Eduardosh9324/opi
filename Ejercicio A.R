library(sqldf)

setwd("C:/Users/lenovo/Desktop/Opi analytics/scripts")
crimes<-read.csv(
  "carpetas-de-investigacion-pgj-cdmx.csv",
  encoding = "UTF-8",
  as.is = c("character","integer","factor","character",rep("factor",7),"character","character","integer","character","character","numeric","numeric","character"))
#crimes <-na.omit(crimes)

crimes$mes_hechos<-factor(
  as.character(crimes$mes_hechos),
  levels = c("Enero",  "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre", ""),
            ordered = TRUE
  )

crimes$ao_hechos <- factor(
  as.character(crimes$ao_hechos),
  levels = as.character(sort(unique(crimes$ao_hechos))),
  ordered = TRUE
  )
levels(crimes$ao_hechos)

#1


#2
dim(crimes)#808871 registros
range(crimes$ao_hechos)
tapply(crimes$mes_hechos, crimes$ao_hechos, min)[1]#1906_6
tapply(crimes$mes_hechos, crimes$ao_hechos, max)#2019_6

#3
sortTable<-sort(table(crimes$delito),decreasing = T)[1:5]
sortTable

plot(x=sortTable,)
barplot(sortTable)
# VIOLENCIA FAMILIAR 
# 69517 
# ROBO DE OBJETOS 
# 52214 
# ROBO A NEGOCIO SIN VIOLENCIA 
# 51426 
# FRAUDE 
# 45349 
# DENUNCIA DE HECHOS 
# 44433 
# AMENAZAS 
# 37415 




#3
delitos<-as.factor(crimes$delito)
levels(delitos)
sort(table(delitos),decreasing = T)[1:5]
# VIOLENCIA FAMILIAR 
# 69517 
# ROBO DE OBJETOS 
# 52214 
# ROBO A NEGOCIO SIN VIOLENCIA 
# 51426 
# FRAUDE 
# 45349 
# DENUNCIA DE HECHOS 
# 44433

#4pendiente
  #filtramos solo del 2019
delitos_alza<-crimes[crimes$ao_hechos == 2019,]
  #definimos funcion auxiliar
library(dplyr)
on<-group_by(delitos_alza,ao_hechos,mes_hechos))

#5
sort(tapply(crimes$alcaldia_hechos, crimes$alcaldia_hechos, length))
#con mas CUAUHTEMOC 131397
#hay muchas alcaldias de las que se cunetan solo con un registro
# ABALA 
# 1 
# ACAMBARO 
# 1 
# ACAXOCHITLAN 
# 1 
# ACONCHI 
# 1 
# ACTOPAN 
# 1 
# ACULCO 
# 1 
# AGUA DULCE 
# 1 
# AGUA PRIETA 
# 1 
# AHUACUOTZINGO 
# 1 
# AHUATLAN 
# 1 
# AHUMADA 
# 1 
# ALMOLOYA DE ALQUISIRAS 
# 1 
# ALTO LUCERO DE GUTIERREZ BARRIOS 
# 1 
# ALVARADO 
# 1 
# AMANALCO 
# 1 
# AMAXAC DE GUERRERO 
# 1 
# AMECA 
# 1

#7
auxFilter1<-function(arreglo){
  aux<-sort(table(arreglo),decreasing = F)[1:3]
  return(aux)
}
L<-tapply(crimes$alcaldia_hechos, crimes$alcaldia_hechos, auxFilter1)

group_alcaldia_colonia<-sqldf('SELECT alcaldia_hechos, colonia_hechos, COUNT(*) AS numberDelitos FROM crimes GROUP BY alcaldia_hechos, colonia_hechos ORDER BY numberDelitos desc')
sqldf('select * from (
    select alcaldia_hechos, 
      colonia_hechos,
      numberDelitos, 
      row_number() over (partition by alcaldia_hechos order by numberDelitos desc) as delitos_rank 
      from group_alcaldia_colonia) ranks
      where delitos_rank <= 3;')

#7








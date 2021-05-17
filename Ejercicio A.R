library(sqldf)
#Cargamos la base de datos
setwd("C:/Users/lenovo/Desktop/Opi analytics/scripts")
crimes<-read.csv(
  "carpetas-de-investigacion-pgj-cdmx.csv",
  encoding = "UTF-8",
  as.is = c("character","integer","factor","character",rep("factor",7),"character","character","integer","character","character","numeric","numeric","character"))

#Ajustamos la clase de la variable mes_hechos
crimes$mes_hechos<-factor(
  as.character(crimes$mes_hechos),
  levels = c("Enero",  "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre", ""),
            ordered = TRUE
  )
#Ajustamos la clase de la variable ao_hechos
crimes$ao_hechos <- factor(
  as.character(crimes$ao_hechos),
  levels = as.character(sort(unique(crimes$ao_hechos))),
  ordered = TRUE
  )
levels(crimes$ao_hechos)

#Pregunta 1


#Pregunta 2: Numero de registros y rango de tiempo de los registros
dim(crimes)#808871 registros
range(crimes$ao_hechos)
tapply(crimes$mes_hechos, crimes$ao_hechos, min)[1]#1906_6
tapply(crimes$mes_hechos, crimes$ao_hechos, max)#2019_6

#Pregunta 3: Distribucion de los delitos y mostramos los 5 más frecuentes
sortTable<-sort(table(crimes$delito),decreasing = T)[1:5]
sortTable
plot(x=sortTable,)
barplot(sortTable)
  #Respuesta del 5
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

#Pregunta 4: Identificando los delitos que van al alza y a la baja en el ultimo año
  #filtramos solo del 2019
delitos_alza<-crimes[crimes$ao_hechos == 2019,]
  #definimos funcion auxiliar
library(dplyr)
on<-group_by(delitos_alza,ao_hechos,mes_hechos))

#Pregunta 5: Encontrando la alcaldia con mas delitos y las que no tienen tanto
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

#Pregunta 6: Mostrando las 3 colonias con mas delitos por cada alcaldia
group_alcaldia_colonia<-sqldf('SELECT alcaldia_hechos, colonia_hechos, COUNT(*) AS numberDelitos FROM crimes GROUP BY alcaldia_hechos, colonia_hechos ORDER BY numberDelitos desc')
sqldf('select * from (
    select alcaldia_hechos, 
      colonia_hechos,
      numberDelitos, 
      row_number() over (partition by alcaldia_hechos order by numberDelitos desc) as delitos_rank 
      from group_alcaldia_colonia) ranks
      where delitos_rank <= 3;')









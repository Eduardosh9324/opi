#Ejercicio Data Home and Kitchen

library(stringr)
setwd("C:/Users/lenovo/Desktop/Opi analytics")
bm<-read.csv("bops_bm.csv",header = TRUE)[,1:7]
online<-read.csv("bops_online.csv",header = TRUE)[,1:7]

#Elegimos los registros que son de ventas en EU o 
# ventas en linea más cercanos a tiendas de EU
us_bm<-bm[bm$usa==1,]
us_on<-online[online$close==1,]

#Ajustamos el tipo de variable de las ventas a "numerico"
us_bm$sales<-as.numeric(str_remove(us_bm$sales,","))
us_on$sales<-as.numeric(str_remove(us_on$sales,","))

#Añadimosuna nueva etiqueta a cada registro, esta etiqueta
#nos permite identificar el año, mes y semana al mismo tiempo de un registro de la tabla
us_bm$newId<-paste(paste(us_bm$year, us_bm$month),us_bm$week,sep = "|")
us_on$newId<-paste(paste(us_on$year, us_on$month),us_on$week,sep = "|")

#calculamos las ventas por semana, separando si fue en 
#fisico o en linea
sales_per_week<-c(
  tapply(us_bm$sales,as.factor(us_bm$newId),sum),
  tapply(us_on$sales,as.factor(us_on$newId),sum)
)

#creamos una funcion auxiliar que nos permite indgar
#si durante una semana fija, se lanzó la iniciativa
bops <- function(arreglo){
  aux = sum(arreglo,na.rm = T)
  if(aux == 0){
    aux2 = 0
  }else{
    aux2 = 1
  }
}
#Creamos una variable categórica para identificar cada registro
#si fue una semana antes o despues de la iniciativa
afterBops<-as.factor(c(
  tapply(us_bm$after, as.factor(us_bm$newId), bops),
  tapply(us_on$after, as.factor(us_on$newId), bops)
))

#creamos otra variable categórica que nos indica si la venta
#se realizó en liínea o en físico
length_on <- length(tapply(us_bm$sales, as.factor(us_bm$newId), bops))
length_bm <- length(tapply(us_on$sales, as.factor(us_on$newId), bops))
isSaleOnline<-as.factor(c(
  rep(1,length_on), rep(0,length_bm)
))

#ajustamos el modelo propuesto
modelo1<-lm(sales_per_week ~ afterBops * isSaleOnline)
summary(modelo1)


# 
# Call:
#   lm(formula = sales_per_week ~ afterBops * isSaleOnline)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2105010  -392746   -59945   213084  3332767 
# 
# Coefficients:
#   Estimate Std. Error
# (Intercept)               1388099     150950
# afterBops1                -214938     215600
# isSaleOnline1             3157482     211490
# afterBops1:isSaleOnline1  -239695     300612
# t value Pr(>|t|)    
# (Intercept)                9.196  5.3e-15 ***
#   afterBops1                -0.997    0.321    
# isSaleOnline1             14.930  < 2e-16 ***
#   afterBops1:isSaleOnline1  -0.797    0.427    
# ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 769700 on 101 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.8037,	Adjusted R-squared:  0.7979 
# F-statistic: 137.9 on 3 and 101 DF,  p-value: < 2.2e-16


#Explorando otras alternativas
modelo2<-lm(sales_per_week ~ isSaleOnline)
summary(modelo2)
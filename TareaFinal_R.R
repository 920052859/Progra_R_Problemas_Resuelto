#### Iniciando el analicis de los datos ####
rm(list = ls())
setwd(dir = "C:/Users/DANIPZA/Desktop/Tareas/tareaFinalR")
getwd()

####################################################################
####Analisis descriptivo del data frame iris (Libreria datasets)####
####################################################################
###   1 Pregunta 
##  por especie : histograma, boxplot , diagrama de dispersion [12 graficos]
##  y en conjunto para las 3 especies : histograma, boxplot , diagrama de dispersion [4 graficos]
library(datasets)
data(iris)
help(iris)
Data <- iris
View(Data)
class(Data)
str(Data) # tenemos 3 especies 
col_names <- c("Sep_longitud", "Sep_ancho", "Petalo_longitud", "Petalo_ancho", "Especie")
names(Data) <- col_names

                                 ###################
---------------------------------####HISTOGRAMAS####--------------------------------------
                                 ###################
-------------------------------------------------------------------------------------------
#### Histograma de las tres especies segun la longitud de la sepa ####
------------------------------------------------------------------------------------------
ggplot(data = Data,
       mapping = aes(x = Sep_longitud,
                     fill = factor(Especie))) +
  geom_histogram(bins = 9,
                 position = 'identity',
                 alpha = 0.7) +
  labs(title = 'Longitud de la sepa segun las especies',
       fill = 'Especies',
       x = 'Valores de la longitud de la sepa',
       y = 'Frecuencia',
       subtitle = 'Obserbacion de las maxima frecuencia se encuentra en la setosa',
       caption = 'Este modelo es de la data iris')

#### Histograma de las tres especies segun la longitud de la sepa ####
ggplot(data = Data,
       mapping = aes(x = Petalo_longitud,
                     fill = factor(Especie))) +
  geom_histogram(bins = 11,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Longitud del petalo segun las especies',
       fill = 'Especies',
       x = 'Valores de la longitud del petalo',
       y = 'Frecuencia',
       subtitle = 'Obserbacion de las maxima frecuencia se encuentra en la setosa',
       caption = 'Este modelo es de la data iris')

-------------------------------------------------------------------------------------
#ahora individualmente 
----------------------------------------------------------------------------------

  # editando histograma para obtener segun la longitud degun la especie 

c(Data$Especie == "setosa",Data$Sep_longitud)
View(Data[Data$Especie == "setosa",]$Sep_longitud)

hist(Data[Data$Especie == "setosa",]$Sep_longitud,
     col = "darkgray",
     border = "gray10",
     main = "Longitud de la sepa llamada Setosa",
     xlab = 'Longitud de la Sepa',
     ylab = 'Frecuencia de la Setosa')

class(Data[Data$Especie == "setosa",])

hist(Data[Data$Especie == "versicolor",]$Sep_longitud, col = "red",
     main = "Longitud de la sepa llamada versicolor",
     xlab = 'Longitud de la Sepa',
     ylab = 'Frecuencia de la versicolor')

hist(Data[Data$Especie == "virginica",]$Sep_longitud, col = "coral4",
     main = "Longitud de la sepa llamada 	virginica",
     xlab = 'Longitud de la Sepa',
     ylab = 'Frecuencia de la 	virginica')

# editando histograma para obtener  el ancho segun la especie  
# Pero sumaremos la tendencia de la campana 

# SETOSA
hist(Data[Data$Especie == "setosa",]$Sep_ancho,
     col = "cadetblue1",
     border = "gray10",
     main = "Ancho de la sepa llamada Setosa",
     xlab = 'Ancho de la Sepa',
     ylab = 'Frecuencia relativa de la Setosa' , freq = FALSE)
curve(dnorm(x, 
            mean=mean(Data[Data$Especie == "setosa",]$Sep_ancho), 
            sd=sd(Data[Data$Especie == "setosa",]$Sep_ancho)), 
      add=TRUE, col="red")

# VERSICOLOR
hist(Data[Data$Especie == "versicolor",]$Sep_ancho, col = "azure2",
     main = "Ancho de la sepa llamada versicolor",
     xlab = 'Ancho de la Sepa',
     ylab = 'Frecuencia relativa de la versicolor', freq = FALSE)
curve(dnorm(x, 
            mean=mean(Data[Data$Especie =="versicolor",]$Sep_ancho), 
            sd=sd(Data[Data$Especie == "versicolor",]$Sep_ancho)), 
      add=TRUE, col="red")

# VIRGINICA
hist(Data[Data$Especie == "virginica",]$Sep_ancho, col = "cornsilk",
     main = "Ancho de la sepa llamada 	virginica",
     xlab = 'Ancho de la Sepa',
     ylab = 'Frecuencia relativa de la 	virginica', freq = FALSE)
curve(dnorm(x, 
            mean=mean(Data[Data$Especie == "virginica",]$Sep_ancho), 
            sd=sd(Data[Data$Especie == "virginica",]$Sep_ancho)), 
      add=TRUE, col="red")

#### en estos vemos que la tendencia no es tan fuerte 
-----------------------------------------------------------------------------------
#####################################
#### Boxplot de las tres especies#### 
#####################################
---------------------------------------------------------------------------------

  #### GRAFICOS DE BOXPLOT PARA LAS TRES ESPECIES ####

#    boxplot de las tres especies segun su longitud  de la sepa
boxplot(Data$Sep_longitud ~ Data$Especie ,main="DIAGRAMA DE CAJAS DE LAS TRES ESPECIES",
        xlab="Especies", ylab="Valores de Dispercion de la longitud de la sepa")

#    boxplot de las tres especies segun su anchura  de la sepa
boxplot(Data$Sep_ancho ~ Data$Especie ,main="DIAGRAMA DE CAJAS DE LAS TRES ESPECIES",
        xlab="Especies", ylab="Valores de Dispercion del ancho de la sepa")

#    boxplot de las tres especies segun su longitud  del petalo
boxplot(Data$Petalo_longitud ~ Data$Especie ,main="DIAGRAMA DE CAJAS DE LAS TRES ESPECIES-LPETALO",
        xlab="Especies", ylab="Valores de Dispercion de la longitud del petalo")

#    boxplot de las tres especies segun su anchura  del petalo
boxplot(Data$Petalo_ancho ~ Data$Especie ,main="DIAGRAMA DE CAJAS DE LAS TRES ESPECIES-APETALO",
        xlab="Especies", ylab="Valores de Dispercion del ancho del petalo")

-------------------------------------------------------------------------------------
  #ahora individualmente 
----------------------------------------------------------------------------------

#### diagrama de cajas segun la especie y longitud del petalo ####  
  
boxplot(Data[Data$Especie == "setosa",]$Petalo_longitud, 
          main = "DIAGRAMA DE CAJAS SEGUN LA LONGITUD DEL PETALO",
          xlab = 'Especie = setosa',
          ylab = 'Valores de Dispercion de la long del petalo')

boxplot(Data[Data$Especie == "versicolor",]$Petalo_longitud, 
        main = "DIAGRAMA DE CAJAS SEGUN LA LONGITUD DEL PETALO",
        xlab = 'Especie = versicolor',
        ylab = 'Valores de Dispercion de la long del petalo')

boxplot(Data[Data$Especie == "virginica",]$Petalo_longitud, 
        main = "DIAGRAMA DE CAJAS SEGUN LA LONGITUD DEL PETALO",
        xlab = 'Especie = virginica',
        ylab = 'Valores de Dispercion de la long del petalo')


#### diagrama de cajas segun la especie y longitud del petalo ####  

boxplot(Data[Data$Especie == "setosa",]$Petalo_ancho, 
        main = "DIAGRAMA DE CAJAS SEGUN EL ANCHO DEL PETALO",
        xlab = 'Especie = setosa',
        ylab = 'Valores de Dispercion del ancho del petalo')

boxplot(Data[Data$Especie == "versicolor",]$Petalo_ancho, 
        main = "DIAGRAMA DE CAJAS SEGUN EL ANCHO DEL PETALO",
        xlab = 'Especie = versicolor',
        ylab = 'Valores de Dispercion del ancho del petalo')

boxplot(Data[Data$Especie == "virginica",]$Petalo_ancho, 
        main = "DIAGRAMA DE CAJAS SEGUN EL ANCHO DEL PETALO",
        xlab = 'Especie = virginica',
        ylab = 'Valores de Dispercion del ancho del petalo')


--------------------------------------------------------------------------------------
  #### Iniciando el segundo analicis de los datos ####
rm(list = ls())
setwd(dir = "C:/Users/DANIPZA/Desktop/Tareas/tareaFinalR")
getwd()

####################################################################
####Analisis descriptivo del data frame iris (Libreria datasets)####
####################################################################
###   2. Pregunta 
##  Contruya un arbol de regresion para el data frame iris (Libreria datasets)  y genere las predicciones 
##  para el siguiente data frame :(datasets)
# conjunto de datos de prueva 
NuevaEspecie <- data.frame(
  Sepal.Length = 6.5, Sepal.Width = 3.0,
  Petal.Length = 5.2, Petal.Width = 2.0
)

-----------------------------------------------------------------------------------
  
  #### CARGANDO PAQUETES ####
library(rpart)
library(rpart.plot)
library(datasets)
data(iris)

# Creamos el modelo 
ArbolregEspecie <- rpart(Species ~ ., data = iris)
class(ArbolregEspecie)
typeof(ArbolregEspecie)
ArbolregEspecie

print(ArbolregEspecie)
#root 150 100 setosa (0.33333333 0.33333333 0.33333333)
# los datos esta distribuidos en tres partes iguales 
# Visualizacion del arbol mod1 : prp [libreria rpart.plot]
prp(ArbolregEspecie)
rpart.plot(ArbolregEspecie, extra = 4)

printcp(ArbolregEspecie) # para la estadistica
plotcp(ArbolregEspecie) # Grafico del error 

#CP nsplit rel error xerror     xstd
#1 0.50      0      1.00   1.19 0.049592
#2 0.44      1      0.50   0.67 0.060888
#3 0.01      2      0.06   0.11 0.031927

## NO hay tanta necesidad de podar el arbol ya que el erro deciende constantemente 

predict(ArbolregEspecie, newdata = NuevaEspecie)

#setosa versicolor virginica
#      0 0.02173913 0.9782609

class(ArbolregEspecie)
class(iris)
#----------------------------------------------------------------------------------------------
############## La prediccion salio que su especie es virginica con seguridad de 97.8%##########
#----------------------------------------------------------------------------------------------
  
#********************************************************************************************************
--------------------------------------------------------------------------------------
  #### Iniciando el segundo analicis de los datos ####
rm(list = ls())
setwd(dir = "C:/Users/DANIPZA/Desktop/Tareas/tareaFinalR")
getwd()

####################################################################
####Analisis descriptivo del data (Libreria mlbench)####
####################################################################
#### Pregunta 3 ####
## Cree un arbol de clasificacion para predecir la variable "diabetes" en los siguientes dos escenarios
## Escenario 1 : no considere la variable "pedigree" 
## Escenario 2 : no considere la variable "glucose" 
## Para cada uno de los escenario, genere nuevos data frames para predecir sus nuevos resultados 
## (predecir la variable "diabetes" cuyos posibles resultados son pos-neg)
#### cargando la librerias####
  
  library(rpart)
  library(rpart.plot)
  library(mlbench)
data("PimaIndiansDiabetes2")
class(PimaIndiansDiabetes2) ##"data.frame"
#--------------------********************************************************************-------------
              #### entonces partire mi data para 65% -> entrenamiento -> prueva ####
---------------------------------------------------------------------------------------------------------
## Escenario 1
  -------------------------------------------------------------------------------------------------------
# Creamos el modelo 
Esenario1 <- PimaIndiansDiabetes2[,-7]

# Perticionamiento 
repartir <- sample(2,nrow(Esenario1), replace = TRUE,prob = c(0.6,0.4))
Entrenamiento <- Esenario1[ repartir==1 ,]
TestData <- Esenario1[ repartir ==2,]

Arbol1 <- rpart(diabetes ~ ., data = Entrenamiento)
class(Arbol1)
typeof(Arbol1)
Arbol1

print(Arbol1)
#root 457 159 neg (0.6520788 0.3479212) 
# los datos esta distribuidos tieneden un poco para el neg 

prp(Arbol1)
rpart.plot(Arbol1, extra = 4)

printcp(Arbol1) # para la estadistica
plotcp(Arbol1) # Grafico del error 

#CP nsplit rel error  xerror     xstd
#1 0.213836      0   1.00000 1.00000 0.064040
#2 0.125786      1   0.78616 0.80503 0.060374
#3 0.031447      2   0.66038 0.75472 0.059163
#4 0.015723      4   0.59748 0.76101 0.059321 <- desde aca 
#5 0.010000      8   0.53459 0.76101 0.059321

## Desde el punto 0.063 casi no desiende el error

#### PODAMOS NUESTRO ARBOL ####
# UTILIZAREMOS EL MODO AUTOMATICO

arbol1Podado <- prune(Arbol1, cp= Arbol1$cptable[ which.min(Arbol1$cptable[,"xerror"]),"CP"])

plotcp(arbol1Podado)
rpart.plot(arbol1Podado, extra = 4)

 resultado <- predict(Arbol1, newdata = TestData, type = "class")

##### Viendo los resultados finales ####
table(resultado,TestData$diabetes)

 #resultado neg pos
 #neg       167  49
 #pos       35  60

 sum(resultado==TestData$diabetes) / length(TestData$diabetes)*100
     
 #### Efectividad de 72.99035 ####
     
---------------------------------------------------------------------------------------------------------
   ## Escenario 2
-------------------------------------------------------------------------------------------------------
   # Creamos el modelo 
   Esenario2 <- PimaIndiansDiabetes2[,-2]
 
 # Perticionamiento 
 repartir <- sample(2,nrow(Esenario2), replace = TRUE,prob = c(0.6,0.4))
 Entrenamiento <- Esenario2[ repartir==1 ,]
 TestData <- Esenario2[ repartir ==2,]
 
 Arbol2 <- rpart(diabetes ~ ., data = Entrenamiento)
 class(Arbol2)
 typeof(Arbol2)
 Arbol1
 
 print(Arbol2)
 #root 492 170 neg (0.6544715 0.3455285) 
 # los datos esta distribuidos tieneden un poco para el neg como en el caso anterior
 
 prp(Arbol2)
 rpart.plot(Arbol2, extra = 4)
 
 printcp(Arbol2) # para la estadistica
 plotcp(Arbol2) # Grafico del error 
 
 #CP nsplit rel error  xerror     xstd
 #1 0.088235      0   1.00000 1.00000 0.062047
 #2 0.064706      2   0.82353 0.99412 0.061960
 #3 0.029412      3   0.75882 0.93529 0.061022
 #4 0.017647      5   0.70000 0.89412 0.060288
 #5 0.014706      6   0.68235 0.91765 0.060716 -> aproximadamente aqui
 #6 0.011765      9   0.62941 0.91765 0.060716
 #7 0.010000     13   0.58235 0.90000 0.060397
 
 ## Desde el punto 0.023 casi no desiende el error
 
 #### PODAMOS NUESTRO ARBOL ####
 # UTILIZAREMOS EL MODO AUTOMATICO
 
 arbol2Podado <- prune(Arbol2, cp= Arbol2$cptable[ which.min(Arbol2$cptable[,"xerror"]),"CP"])
 
 plotcp(arbol2Podado)
 rpart.plot(arbol2Podado, extra = 4)
 
 resultado <- predict(Arbol2, newdata = TestData, type = "class")
 
 ##### Viendo los resultados finales ####
 table(resultado,TestData$diabetes)
 
 #resultado neg pos
 #neg       139  50
 #pos       39  48
 
 sum(resultado==TestData$diabetes) / length(TestData$diabetes)*100
 
 #### Efectividad de 67.75362 ####     
     
#***************************************************************************************************************
 ####################################################################
 ####Analisis descriptivo del data (Libreria MASS)####
 ####################################################################
 #**************************************************************************************************************
      
 #### Iniciando el segundo analicis de los datos ####
 rm(list = ls())
 setwd(dir = "C:/Users/DANIPZA/Desktop/Tareas/tareaFinalR")
 getwd()
 
 #### Pregunta 4 ####
 ## Construta un arbol de regresion para el data frame Boston (libreria MASS) y genere las predicciones
 ## para los siguientes dos escenadios 
 Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,193, 194, 209,
             210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,366 ,367 ,371 ,378, 393 ,401 ,
             406 ,422, 423 ,453 ,455 ,485, 496, 505)
 Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
 Escenario1 <- Boston[Prueba1,] # dataframe de prueba 1
 Escenario2 <- Boston[Prueba2,-14] # dataframe de prueba 2
 #   Deacuerdo al arbol de regresion y a los resultados obtenidos usando los data frames de prueba :  Escenario1 y 
 #   Escenario2, diga usted con cual de estos dos dataframes de prueba le fue "mejor" a las predicciones objtenidas
 #   Sustente su respuesta a esta pregunta con un indicador cuantitativo y con un grafico.
  ## librerias
 
 library(rpart)
 library(rpart.plot)
 library(MASS)
 data(Boston)
 
 
 
 Arbol <- rpart(medv ~ ., data = Boston)
 class(Arbol)
 typeof(Arbol)
 Arbol
 
 print(Arbol)
 #root 506 42716.3000 22.53281 
 
 prp(Arbol)
 rpart.plot(Arbol, extra = 1)
 
 printcp(Arbol) # para la estadistica
 plotcp(Arbol) # Grafico del error 
 
 #CP nsplit rel error  xerror     xstd
 #1 0.452744      0   1.00000 1.00229 0.083012
 #2 0.171172      1   0.54726 0.64433 0.059766
 #3 0.071658      2   0.37608 0.45601 0.049900
 #4 0.036164      3   0.30443 0.36463 0.045104
 #5 0.033369      4   0.26826 0.34642 0.044344
 #6 0.026613      5   0.23489 0.34227 0.045240
 #7 0.015851      6   0.20828 0.30759 0.044600 -> aca un poco 
 #8 0.010000      7   0.19243 0.28456 0.039962
 
 ## Desde el punto 0.015851 casi no desiende el error
 
 #### PODAMOS NUESTRO ARBOL ####
 # UTILIZAREMOS EL MODO AUTOMATICO
 
 arbolPodado <- prune(Arbol, cp= Arbol$cptable[ which.min(Arbol$cptable[,"xerror"]),"CP"])
 
 resultado <- predict(Arbol, newdata = Escenario1)
 
 #9.90  	0	0.5440	6.113	  58.8	4.0019	4	304	  18.4	396.23	  12.73	  21.0
 #9.82349	0.0	18.10	0	0.671	6.794	98.8	  1.3580	24	  666	20.2	396.90	21.24
 resultado2 <- predict(Arbol, newdata = Escenario2)

plot(resultado, type = 'l')
 plot(resultado2,type = 'l')
 
    
 (sum(Boston[Prueba1,]$medv)-sum(resultado))/ sum(Boston[Prueba1,]$medv)*100
 sum(resultado) / length(Boston[Prueba1,]$medv)*100
 sum(resultado2) / length(Boston[Prueba2,]$medv)*100 
 (sum(Boston[Prueba2,]$medv)-sum(resultado2)) / length(Boston[Prueba2,]$medv)*100
 
 table(resultado,Boston[Prueba1,]$medv)
     class(resultado)
     str(resultado)
     
     Boston[Prueba1,]$medv  
     resultado

     Boston[Prueba1,]$medv  
     [1] 24.0 34.7 16.5 18.9 20.4 18.2 14.5 18.9 21.2 23.3 23.5 25.0 20.6 14.3 13.8 17.8 17.4
     [18] 32.0 36.4 31.1 24.4 20.0 28.7 37.6 25.1 22.0 21.9 33.2 33.1 28.2 16.2 21.0 22.2 24.5
     [35] 27.5 21.9 50.0 13.3  9.7  5.6  5.0 14.2 20.8 16.1 14.9 20.6 23.1 22.0
     >      resultado
     1        3        9       12       14       15       32       36       45       59 
     27.42727 33.73846 17.13762 21.65648 21.65648 21.65648 21.65648 21.65648 21.65648 21.65648 
     66       94       95      130      146      149      171      188      193      194 
     21.65648 21.65648 21.65648 17.13762 17.13762 17.13762 17.13762 27.42727 33.73846 27.42727 
     209      210      218      227      237      241      255      277      304      308 
     17.13762 17.13762 27.42727 45.09667 27.42727 27.42727 21.65648 33.73846 33.73846 27.42727 
     316      320      334      349      366      367      371      378      393      401 
     21.65648 21.65648 21.65648 27.42727 21.65648 21.65648 33.73846 11.97838 11.97838 11.97838 
     406      422      423      453      455      485      496      505 
     11.97838 11.97838 21.65648 17.13762 11.97838 21.65648 17.13762 27.42727 
     
     Boston[Prueba2,]$medv  
     resultado2
     
     Boston[Prueba2,]$medv  
     [1] 18.6 21.7 20.6  8.1 12.7 11.3 44.0 19.0 16.1 33.4 41.7 14.5  8.8 13.1 22.7 14.1 19.1
     [18] 48.8 14.6 17.8 23.7 17.2 24.5 19.5 27.5
     >      resultado2
     353      211      503      491      457      383      257      467      433      307 
     21.65648 17.13762 21.65648 17.13762 17.13762 11.97838 45.09667 17.13762 21.65648 33.73846 
     233      431      419      379      359      449      487      263      479      317 
     45.09667 11.97838 11.97838 11.97838 21.65648 11.97838 17.13762 45.09667 11.97838 17.13762 
     239      347      349      463      223 
     21.65648 21.65648 27.42727 21.65648 27.42727 
     
     #### hay ma
     resultado[1]
     for( i in 1:48)
resultado[1]+  Boston[Prueba2,]$medv[1]   


#### en conclusion se podria decir que en el arreglo 1 hay nemos % de error ya que son mas datos a manejar 
#### por que sale 1.05% de erro 

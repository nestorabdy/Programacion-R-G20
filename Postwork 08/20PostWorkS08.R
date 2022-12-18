"Un centro de salud nutricional está interesado en analizar estadísticamente y
probabilísticamente los patrones de gasto en alimentos saludables y no saludables
en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar
tiene recursos financieros extrar al ingreso y en si presenta o no inseguridad
alimentaria. Además, está interesado en un modelo que le permita identificar los
determinantes socioeconómicos de la inseguridad alimentaria.
La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición
(2012) levantada por el Instituto Nacional de Salud Pública en México. La mayoría
de las personas afirman que los hogares con menor nivel socioeconómico tienden a
gastar más en productos no saludables que las personas con mayores niveles
socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar
presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:
nse5f (Nivel socieconómico del hogar): 1 Bajo, 2 Medio bajo, 3 Medio, 4 Medio alto, 5 Alto
#area (Zona geográfica): 0 Zona urbana, 1 Zona rural
#numpeho (Número de persona en el hogar)
#refin (Recursos financieros distintos al ingreso laboral): 0 no, 1 sí
#edadjef (Edad del jefe/a de familia)
#sexoje (Sexo del jefe/a de familia): 0 Hombre, 1 Mujer
#añosedu (Años de educación del jefe de familia)
#ln_als (Logarítmo natural del gasto en alimentos saludables)
#ln_alns (Logarítmo natural del gasto en alimentos no saludables)
#IA (Inseguridad alimentaria en el hogar): 0 No presenta IA, 1 Presenta IA"

### Cargo de librerias
library(DescTools)
library(ggplot2)
library(moments)
library(dplyr)
library(tidyverse)


# Lectura de datos y filtrado de datos
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
sum(complete.cases(df))
sum(complete.cases(df))
sum(complete.cases(df$edadjef))
sum(complete.cases(df$sexojef))
sum(complete.cases(df$ln_als))
sum(complete.cases(df$ln_alns))

df.limpio <- df[complete.cases(df),]
str(df.limpio)
summary(df.limpio)

df.limpio$nse5f <- factor(df.limpio$nse5f, levels=c(1,2,3,4,5),labels=c("Bajo","Medio bajo","Medio","Medio alto", "Alto"))
df.limpio$IA <- factor(df.limpio$IA,levels=c(0,1),labels=c("No presenta IA", "Presenta IA") )
df.limpio$sexojef <- factor(df.limpio$sexojef,levels=c(0,1),labels=c("Hombre", "Mujer") )
df.limpio$area <- factor(df.limpio$area,labels =c("Urbana","Rural"))
df.limpio$refin <- factor(df.limpio$refin,labels =c("No","Si"))

attach(df.limpio)


######################### 2  #################################################
# Análisis de datos 

## Logaritmo natural de Gasto en alimentos saludables
"Histograma de densidad"
hist(df.limpio$ln_als, prob=T, main="Logaritmo natural de Gasto en alimentos saludables", xlab="ln de Gastos en alimentos saludables")
x <- seq(min(df.limpio$ln_als), max(df.limpio$ln_als), length = 80)
f <- dnorm(x, mean = mean(df.limpio$ln_als), sd = sd(df.limpio$ln_als))
lines(x, f, col = "red", lwd = 2)

"Medidad de tendencia central"
mean(ln_als)   #Media
median(ln_als) #Mediana
Mode(ln_als)   #Moda

"Medidas de dispersión"
var(ln_als)  #Varianza
sd(ln_als)   #Desviación estándar
IQR(ln_als)  #Dispersión alrededor de la media

"Medidas de forma"
s1 <- skewness(ln_als)  #Sesgo
s1 # Presenta sesgo a la izquierda
c1 <- kurtosis(ln_als)  #Curtosis 
c1  #Presenta forma letpocúrtica

####
"La distribución de los datos se asemeja a una distribución normal con sesgo a la izquierda.
Sus medidas de tendencia central se ordenan de la siguiente manera:
  Media < Mediana < Moda
Con la desviación estándar sd= 1.04 podemos observar que no hay mucha dispersión de los datos, y de manera gráfica a través del histograma observamos que tiene  sesgo a la izquierda forma leptocúrtica"



## Logaritmo natural de Gasto en alimentos saludables
hist(df.limpio$ln_alns, prob=T, main="Logaritmo natural de Gastos en alimentos no saludables",  xlab="ln de Gastos en alimentos saludables")
x <- seq(min(df.limpio$ln_alns), max(df.limpio$ln_alns), length = 80)
f <- dnorm(x, mean = mean(df.limpio$ln_alns), sd = sd(df.limpio$ln_alns))
lines(x, f, col = "red", lwd = 2)

"Medidad de tendencia central"
mean(ln_alns)   #Media
median(ln_alns) #Mediana
Mode(ln_alns)   #Moda

"Medidas de dispersión"
var(ln_alns)  #Varianza
sd(ln_alns)   #Desviación estándar
IQR(ln_alns)  #Dispersión alrededor de la media

"Medidas de forma"
s1 <- skewness(ln_alns)  #Sesgo
s1 # Presenta un ligero sesgo a la derecha
c1 <- kurtosis(ln_alns)  #Curtosis 
c1  #Presenta forma aproximadamente mesocúrtica

###
"La distribución de los datos se asemeja a una distribución normal con sesgo a la derecha.
Sus medidas de tendencia central se ordenan de la siguiente manera:
Media  =  Mediana  = Moda
Con la desviación estándar sd= 1.04 podemos observar que no hay mucha dispersión de los datos, y de manera gráfica a través del histograma observamos que tiene  un pequeño sesgo a la derecha y forma mesocúrtica" 


# Boxplots
## Boxplot por nivel socioeconómico e Inseguridad alimentaria
ggplot(df.limpio, aes(x=nse5f, y=ln_als, fill=IA))+
  geom_boxplot()+
  labs( x = "Nivel socioeconómico", y = "Ln de gasto de alimentos saludables")+
  theme_classic()

###
"Observamos en esta gráfica que la IA varia muy poco entre su mismo nivel socioeconómico"


ggplot(df.limpio, aes(x=nse5f, y=ln_alns, fill=IA))+
  geom_boxplot()+
  labs( x = "Nivel socioeconómico", y = "Ln de gasto de alimentos no saludables")+
  theme_classic()

"Tablas resumen"
resumen.df.1 <- df.limpio %>%
  group_by(nse5f,IA) %>%
  summarize(mean_m = mean(ln_als),
            mediana_m=median(ln_als),
            sd_m = sd(ln_als),
            mean_m = mean(ln_alns),
            mediana_m=median(ln_alns),
            sd_m = sd(ln_alns),
            n = n())
resumen.df.1

"Observamos en esta gráfica que la IA varia muy poco entre su mismo nivel socioeconómico, además a diferencia del 
gasto en alimentos saludables esta clasificacion presnta un IQR mas amplio"

## Boxplot por nivel socioeconómico y sexo del jefe de familia
ggplot(df.limpio, aes(x=nse5f, y=ln_als, fill=sexojef))+
  geom_boxplot()+
  labs( x = "Nivel socioeconómico", y = "Ln de gasto de alimentos saludables")+
  theme_classic()


ggplot(df.limpio, aes(x=nse5f, y=ln_alns, fill=sexojef))+
  geom_boxplot()+
  labs( x = "Nivel socioeconómico", y = "Ln de gasto de alimentos no saludables")+
  theme_classic()

"Tablas resumen"
resumen.df.2 <- df.limpio %>%
  group_by(nse5f,sexojef) %>%
  summarize(mean_m = mean(ln_als),
            mediana_m=median(ln_als),
            sd_m = sd(ln_als),
            mean_m = mean(ln_alns),
            mediana_m=median(ln_alns),
            sd_m = sd(ln_alns),
            n = n())
resumen.df.2

### 
"En estos boxplots no existe mucha diferencia a la clasificacion por IA, el comportamiento es muy similar. "

## Boxplot por Inseguridad alimentaria y sexo del jefe de familia
ggplot(df.limpio, aes(x=IA, y=ln_als, fill=sexojef))+
  geom_boxplot()+
  labs( x = "Insuficiencia alimentaria", y = "Ln de gasto de alimentos saludables")+
  theme_classic()

"No hay muchas diferencias entre las diferentes categorias"

ggplot(df.limpio, aes(x=IA, y=ln_alns, fill=sexojef))+
  geom_boxplot()+
  labs( x = "Insuficiencia alimentaria", y = "Ln de gasto de alimentos no saludables")+
  theme_classic()

### 
"Podemos observar que las mujeres jefas de familia gastan menos en alimentos no saludables, mienstras que el gasto en alimentos saludables es muy similar al de los hombres"

"Tablas resumen"
resumen.df.3 <- df.limpio %>%
  group_by(IA,sexojef) %>%
  summarize(mean_m = mean(ln_als),
            mediana_m=median(ln_als),
            sd_m = sd(ln_als),
            mean_m = mean(ln_alns),
            mediana_m=median(ln_alns),
            sd_m = sd(ln_alns),
            n = n())
resumen.df.3

######################### 3  #################################################

#3Probabilidad de presentar IA
t<-table(IA)
t

transform(t,
          rel_freq=prop.table(Freq))

barplot(table(IA)/length(IA), 
        main = "Relacion Inseguridad Alimentaria de la muestra", 
        xlab = "Resultado",
        names = c("NO Presenta IA", "Presenta IA"))

######################### 4 #################################################

"Pruebas de hipotesis"
"1. Se empleo un nivel de confianza del 99%"
"En promedio el ln de gasto en alimentos saludables es en hogares con recursos financieros extras es mayor que en hogares que no lo tienen"
# Ho: promedio_ln_als (refin==Si) <= promedio_ln_als (refin==No)
# Ha: promedio_ln_als (refin==Si) > promedio_ln_als (refin==No)

var.test(df.limpio[df.limpio$refin == "Si", "ln_als"], 
         df.limpio[df.limpio$refin == "No", "ln_als"], 
         ratio = 1, alternative = "two.sided",conf.level = 0.99)

#p-value = 0.01147 > 0.01 (99% NC)

t.test(df.limpio[df.limpio$refin == "Si", "ln_als"],
       df.limpio[df.limpio$refin == "No", "ln_als"],
       alternative = "greater",conf.level = 0.99, mu = 0, var.equal = TRUE,)

# Con un p-value =3.119e-08 < 0.01 (99% NC). EEE para rechazar Ho
# Es decir el promedio del gastos en alimentos saludables por hogares con recursos financieros extras no es mayor a los que si lo tienen




"2. En promedio el gasto en alimentos no saludables es en hogares con una mujer como jefe de familia es menor que en hogares cuya jefe de familia es hombre con un 90%"
# Ho: promedio_ln_alns (sexojef==mujer) >= promedio_ln_als (sexojef==Hombre)
# Ha: promedio_ln_alns (sexojef==mujer) < promedio_ln_als (sexojef==Hombre)

var.test(df.limpio[df.limpio$sexojef == "Mujer", "ln_alns"], 
         df.limpio[df.limpio$sexojef == "Hombre", "ln_alns"], 
         ratio = 1, alternative = "two.sided",conf.level = 0.90)

#p-value = 0.377 > 0.1 (90% NC)

t.test(df.limpio[df.limpio$sexojef == "Mujer", "ln_alns"],
       df.limpio[df.limpio$sexojef == "Hombre", "ln_alns"],
       alternative = "less",conf.level = 0.90, mu = 0, var.equal = TRUE,)
# Con un p-value ~ 2.2e-16 < 0.1 (90% NC). EEE para rechazar Ho
#  Es decir el promedio del gastos en alimentos no saludables por hogares con jefe de familia mujer no es menor a los hogares que tienen jefe de familia mujer


"3. En promedio La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a 
gastar más en productos no saludables que las personas con mayores niveles socioeconómicos"
# Ho: promedio_ln_alns (nse5f==("Bajo" | "Medio bajo" ) >= promedio_ln_als (nse5f==("Medio alto" | "Alto" )
# Ho: promedio_ln_alns (nse5f==("Bajo" | "Medio bajo" ) < promedio_ln_als (nse5f==("Medio alto" | "Alto" )
var.test(df.limpio[(df.limpio$nse5f == "Bajo" | df.limpio$nse5f == "Medio Bajo"), "ln_alns"], 
         df.limpio[(df.limpio$nse5f == "Medio Alto" | df.limpio$nse5f == "Alto"), "ln_alns"], 
         ratio = 1, alternative = "two.sided",conf.level = 0.90)

#p-value = 6.199e-13 < 0.1 (90% NC). Las varianzas no son similares
# No EEE suficiente para evaluar las hipótesis

"4. En promedio el gasto en alimentos saludables en hogares con IA es igual a los que no presentan IA  con un 90%"
# Ho: promedio_ln_als (IA==sI) == promedio_ln_als (IA==No)
# Ha: promedio_ln_als (IA==sI) != promedio_ln_als (IA==No)

var.test(df.limpio[df.limpio$IA == "No presenta IA", "ln_als"], 
         df.limpio[df.limpio$IA == "Presenta IA", "ln_als"], 
         ratio = 1, alternative = "two.sided",conf.level = 0.90)

#p-value =0.3532 > 0.1 (90% NC)

t.test(df.limpio[df.limpio$IA == "No presenta IA", "ln_als"],
       df.limpio[df.limpio$IA == "Presenta IA", "ln_als"],
       alternative = "two.sided",conf.level = 0.90, mu = 0, var.equal = TRUE,)
# Con un p-value < 2.2e-16 < 0.1 (90% NC). EEE para rechazar Ho
#  Es decir el promedio del gastos en alimentos saludables por hogares con IA no es igual a los hogares que no presnetan IA


"5. El promedio para el gasto de alimentos saludables es el mismo para todos los niveles socioeconómicos. Con un nivel de confianza del 90%"
# Ho: promedio_ln_alns (nse5f==Bajo) == promedio_ln_alns (nse5f==Medio Bajo) == promedio_ln_alns (nse5f==Medio) ...
# Ha: Al menos un par de promedios del gasto en alimentos saludables en dos niveles socioeconómicos son diferentes
boxplot(ln_als ~ nse5f, data = df.limpio)

anova <- aov(ln_als ~ nse5f, data = df.limpio)
summary(anova)

TukeyHSD(anova)
plot(TukeyHSD(anova))

#p-value <2e-16
#p-value < 0.1 (90% NC) EEE para rechazar Ho
"Hemos obtenido un p-valor del orden de 2e-16. Esto nos permite rechazar la hipótesis nula en favor de la hipotesis alternativa 
y concluir que al menos el promedio del gasto en alimentos saludables de dos niveles socioeconómicos es distinto. 
La gráfica de diferencias de promedios y el diagrama boxplot apoyan visualmente el rechazo de la hipótesis nula"

######################### 5  #################################################

#Se genra una matriz dxe correlación que incluye a IA
df.select <- select(df, IA,nse5f, numpeho, edadjef, añosedu, ln_als, ln_alns)
round(cor(df.select),4)

"        IA      nse5f    numpeho  dadjef añosedu ln_als  ln_alns
IA       1.0000 -0.2811  0.0967      NA -0.2114     NA      NA
nse5f   -0.2811  1.0000  0.0430      NA  0.4262     NA      NA
numpeho  0.0967  0.0430  1.0000      NA  0.0080     NA      NA
edadjef      NA      NA      NA       1      NA     NA      NA
añosedu -0.2114  0.4262  0.0080      NA  1.0000     NA      NA
ln_als       NA      NA      NA      NA      NA      1      NA
ln_alns      NA      NA      NA      NA      NA     NA       1"


#Partiendo del DataFrame "limpio" o sin datos de "NA" se procede a la generación de una matriz de correlación
" sin incluir a IA y así comparando con la tabla anterior"
#adv.select <- select(adv, Sales,TV, Radio, Newspaper)
#round(cor(adv.select),4)

# La primera matriz va a permitir contrastar 4 variables (numpeho,edadjef, añoseduln_als,ln_alns) para ver si existe correlación
# tomando como base el nivel socioeconómico del hogar nse5f
dfLimpio <- df[complete.cases(df),]
dfLimpio.select <- select(dfLimpio, nse5f, numpeho, edadjef, añosedu, ln_als, ln_alns)
round(cor(dfLimpio.select),4)

#Obteniendo la siguiente tabla de correlación

"        nse5f numpeho edadjef añosedu  ln_als ln_alns
nse5f   1.0000  0.0495  0.0451  0.4335  0.3566  0.2922
numpeho 0.0495  1.0000 -0.1140 -0.0435  0.3109  0.0809
edadjef 0.0451 -0.1140  1.0000 -0.3566 -0.1127 -0.0984
añosedu 0.4335 -0.0435 -0.3566  1.0000  0.2421  0.2184
ln_als  0.3566  0.3109 -0.1127  0.2421  1.0000  0.3285
ln_alns 0.2922  0.0809 -0.0984  0.2184  0.3285  1.0000"

# Teniendo las siguientes observaciones:

#1. Hay una correlación débil entre El nivel socioeconómico del hogar y los gastos en alimentos
# saludables y no saludables (0.3566 y 0.2922, respectivamente)
#2. Hay una correlación prácticamente positiva moderada entre el Nivel Socioeconómico y los años de educación
# del jefe de familia

# Recordando que

##nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto""
#ln_als (Logarítmo natural del gasto en alimentos saludables)
#ln_alns (Logarítmo natural del gasto en alimentos no saludables)
# A pesar de que no se incluye IA se puede observar que existe una correlación positiva débil entre el Nivel 
# Socioeconómico y de añosedu de 0.433, también con gasto de alimentos no slaudables que es de 0.2922

# A continuación se genera una matriz de dispersión relacionando las variables a través de ~
# Como son muchos datos se divide en dos presentaciones gráficas

pairs(~ IA+nse5f +ln_als+ln_alns,
      data = dfLimpio, gap = 0.4, cex.labels = 1.5)

pairs(~ IA+nse5f+area+numpeho+refin +ln_als+ln_alns,
      data = dfLimpio, gap = 0.4, cex.labels = 1.5)




# Estimación del modelo de regresión, lineal o logístico, para identificiar los determinanres de la 
#inseguridad alimentaria en México

#Se inician con modelos de Regresión Lineal
attach(dfLimpio) 
modelo1 <- lm(IA ~ nse5f +numpeho+edadjef+añosedu+ln_als+ln_alns)
summary(modelo1)

"Residuals:
    Min      1Q  Median      3Q     Max 
-1.1896 -0.4883  0.1806  0.3145  0.6616 
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.0816979  0.0317467  34.073  < 2e-16 ***
nse5f       -0.0673001  0.0026281 -25.608  < 2e-16 ***
numpeho      0.0283756  0.0017439  16.271  < 2e-16 ***
edadjef     -0.0001502  0.0002240  -0.670    0.503    
añosedu     -0.0103401  0.0007980 -12.958  < 2e-16 ***
ln_als      -0.0105453  0.0051347  -2.054    0.040 *  
ln_alns     -0.0218575  0.0031499  -6.939 4.07e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.4303 on 20273 degrees of freedom
Multiple R-squared:  0.09867,	Adjusted R-squared:  0.0984 
F-statistic: 369.9 on 6 and 20273 DF,  p-value: < 2.2e-16"

# Regresión logística


logistic.prueba1<-glm(IA ~ nse5f+area+numpeho+refin+edadjef+sexojef+añosedu+ln_als+ln_alns, data=dfLimpio,family = binomial())
summary(logistic.prueba1)
exp(coef(logistic.prueba1))

"Mostrando la siguiente tabla"

"(Intercept)       nse5f        area     numpeho       refin     edadjef     sexojef     añosedu      ln_als     ln_alns 
 19.3437901   0.6896405   0.9021626   1.1907381   1.4887174   1.0003946   1.1678150   0.9472396   0.9131167   0.9021909 "

# Se puede interpretar que los recursos financieros distintos al ingreso laboral (refin) tienen más alta probabilidad
# sobre la Insuficiencia Laboral y el segundo factor es el número de personas en el hogar
# Se analiza específicamente con el Número de personas
y<-df$IA
x<-df$numpeho
logistic.prueba1<-glm(y ~x,data=dfLimpio, family = binomial())
summary(logistic.prueba1)
exp(coef(logistic.prueba1))

# Dando los siguiente resultados:

"(Intercept)           x 
1.728377    1.132376 "

" y se genra la gráfica para representar la relación"
plot(IA ~ numpeho, data=dfLimpio, xlim = c(0,10))
curve(predict(logistic.prueba1, newdata = data.frame(x), type = "response"),
      add = TRUE)


# Se concluye que la Insuficiencia Alimentaria le afecta el Número de personas del hogar


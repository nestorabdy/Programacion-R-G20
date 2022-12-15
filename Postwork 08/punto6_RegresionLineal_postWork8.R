"Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente 
los patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en su
nivel socioeconómico, en si el hogar tiene recursos financieros extrar al ingreso y en si presenta o no
inseguridad alimentaria. Además, está interesado en un modelo que le permita identificar los determinantes
socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por
el Instituto Nacional de Salud Pública en México. La mayoría de las personas afirman que los hogares 
con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas con
mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente
cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:"

#nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto""
#area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
#numpeho (Número de persona en el hogar)
#refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
#edadjef (Edad del jefe/a de familia)
#sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
#añosedu (Años de educación del jefe de familia)
#ln_als (Logarítmo natural del gasto en alimentos saludables)
#ln_alns (Logarítmo natural del gasto en alimentos no saludables)
#IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA""
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
"Plantea el problema del caso


5. Estima un modelo de regresión, lineal o logístico, para identificiar los determinanres de la 
inseguridad alimentaria en México


NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás analizar e 
interpretar todos tus resultados para poder dar una conclusión final al problema planteado."

library(dplyr)
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")


#Sugerimos que el campo nse5f quede con un valor numérico para fines de cálculo de parámetros estadísticos
# df$nse5f<- factor(df$nse5f, levels = c("Bajo", "Medio_bajo", "Medio", "Medio_alto", "Alto"), ordered = TRUE)

# conversión de las variables categóricas

df$area <- factor(df$area,labels =c("Urbana","Rural"))
df$refin <- factor(df$refin,labels =c("No","Si"))
df$sexojef <- factor(df$sexojef,labels =c("Hombre","Mujer"))
df$IA <- factor(df$IA,labels =c("No","Si"))

# consideramos que el campo nse5f se mantenga con los valores numéricos para fines de cálculos estadísticos

 # df$nse5f<- factor(df$nse5f, levels = c("Bajo", "Medio_bajo", "Medio", "Medio_alto", "Alto"), ordered = TRUE)
   

sum(complete.cases(df))
sum(complete.cases(df$edadjef))
sum(complete.cases(df$sexojef))
sum(complete.cases(df$ln_als))
sum(complete.cases(df$ln_alns))

dfLimpio <- df[complete.cases(df),]
str(dfLimpio)
summary(dfLimpio)

#La primera parte es obtener una matriz de correlación manteniendo IA como
#variable a comparar
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
sum(complete.cases(df))
sum(complete.cases(df$edadjef))
sum(complete.cases(df$sexojef))
sum(complete.cases(df$ln_als))
sum(complete.cases(df$ln_alns))

dfLimpio <- df[complete.cases(df),]
str(dfLimpio)
summary(dfLimpio)

library(dplyr)
library(ggplot2)

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

#adv.select <- select(adv, Sales,TV, Radio, Newspaper)
#round(cor(adv.select),4)

# La primera matriz va a permitir contrastar 4 variables (numpeho,edadjef, añoseduln_als,ln_alns) para ver si existe correlación
# tomando como base el nivel socioeconómico del hogar nse5f

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
# Recordando que

##nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto""
#ln_als (Logarítmo natural del gasto en alimentos saludables)
#ln_alns (Logarítmo natural del gasto en alimentos no saludables)

##
# A continuación se genera una matriz de dispersión relacionando las variables a través de ~

pairs(~ nse5f +numpeho+edadjef+añosedu+ln_als+ln_alns, 
      data = dfLimpio, gap = 0.4, cex.labels = 1.5)

# Estimación del modelo de regresión, lineal o logístico, para identificiar los determinanres de la 
#inseguridad alimentaria en México

#Se inician con modelos de Regresión Lineal
attach(dfLimpio) 
modelo1 <- lm(IA ~ nse5f +numpeho+edadjef+añosedu+ln_als+ln_alns)

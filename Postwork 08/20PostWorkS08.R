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



# Análisis de datos 

## Logaritmo natural de Gasto en alimentos saludables
"Histograma de densidad"
hist(df.limpio$ln_als, prob=T, main="Logaritmo natural de Gasto en alimentos saludables", xlab="ln de Gastos en alimentos saludables")
x <- seq(min(df.limpio$ln_als), max(df.limpio$ln_als), length = 80)
f <- dnorm(x, mean = mean(df$ln_als), sd = sd(df$ln_als))
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

## Logaritmo natural de Gasto en alimentos No saludables
hist(df.limpio$ln_alns, prob=T, main="Logaritmo natural de Gastos en alimentos no saludables", , xlab="ln de Gastos en alimentos saludables")
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

# Boxplots
## Boxplot por nivel socioeconómico e Inseguridad alimentaria
ggplot(df.limpio, aes(x=nse5f, y=ln_als, fill=IA))+
  geom_boxplot()+
  labs( x = "Nivel socioeconómico", y = "Ln de gasto de alimentos saludables")+
  theme_classic()


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

## Boxplot por Inseguridad alimentaria y sexo del jefe de familia
ggplot(df.limpio, aes(x=IA, y=ln_als, fill=sexojef))+
  geom_boxplot()+
  labs( x = "Insuficiencia alimentaria", y = "Ln de gasto de alimentos saludables")+
  theme_classic()


ggplot(df.limpio, aes(x=IA, y=ln_alns, fill=sexojef))+
  geom_boxplot()+
  labs( x = "Insuficiencia alimentaria", y = "Ln de gasto de alimentos no saludables")+
  theme_classic()

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

#3Probabilidad de presentar IA
t<-table(IA)
t

transform(t,
          rel_freq=prop.table(Freq))

barplot(table(IA)/length(IA), 
        main = "Relacion Inseguridad Alimentaria de la muestra", 
        xlab = "Resultado",
        names = c("NO Presenta IA", "Presenta IA"))



#5Modelo Regresion Logistica
logistic.1 <- glm(IA ~ nse5f+ area + numpeho + refin + edadjef + sexojef + 
                    añosedu + ln_als + ln_alns, family = binomial)

summary(logistic.1)




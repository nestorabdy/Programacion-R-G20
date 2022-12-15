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

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")


str(df)
sum(complete.cases(df))

# df$nse5f<- factor(df$nse5f, levels = c("Bajo", "Medio_bajo", "Medio", "Medio_alto", "Alto"), ordered = TRUE)
# df$sexojef <- factor(var$sexojef)
df$area <- factor(df$area,labels =c("Urbana","Rural"))
df$refin <- factor(df$refin,labels =c("No","Si"))
df$sexojef <- factor(df$sexojef,labels =c("Hombre","Mujer"))
df$IA <- factor(df$IA,labels =c("No","Si"))

library(tidyverse)

#no son factor
#df.select <- select(df,IA,numpeho,edadjef, añosedu, ln_alns, ln_als)

#round(cor(df.select),4)  

#attach(df)

#pairs"(~IA+nse5f+sexojef,
#      data=df, gap=0.4,cex.labels=1.5)
"
## NAs
sum(complete.cases(df))
sum(complete.cases(df$edadjef))
sum(complete.cases(df$sexojef))
sum(complete.cases(df$ln_als))
sum(complete.cases(df$ln_alns))

df.clean <- df[complete.cases(df),]
str(df.clean)
summary(df.clean)"

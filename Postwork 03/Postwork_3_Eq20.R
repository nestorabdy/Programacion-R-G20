# Postwork Sesión 3
" Objetivo
    - Realizar un análisis descriptivo de las variables de un dataframe
  Requisitos
    - R, RStudio
    - Haber realizado el prework y seguir el curso de los ejemplos de la sesión
    - Curiosidad por investigar nuevos tópicos y funciones de R"
"Desarrollo
Utilizando el dataframe boxp.csv realiza el siguiente análisis descriptivo. 
No olvides excluir los missing values y transformar las variables a su tipo y 
escala correspondiente."

library(DescTools)
library(ggplot2)
library(moments)
library(dplyr)

d <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")
str(d)
d$Categoria <- factor(d$Categoria)
d$Grupo <- factor(d$Grupo)
sum(complete.cases(d))
d <- d[complete.cases(d),]
summary(d)


# 1) Calcula e interpreta las medidas de tendencia central de la variable Mediciones
mean(d$Mediciones);median(d$Mediciones);Mode(d$Mediciones)


# 1.a) Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de Mediciones?
# Moda < Mediana < Media 
# Por los tanto se concluye que tiene sesgo a la derecha

# 1.b) Calcula e interpreta la desviación estándar y los cuartiles de la distribución de Mediciones
desv_s.d<-sd(d$Mediciones)
cuartiles.d <- quantile(d$Mediciones, probs = c(0.25, 0.50, 0.75))
iqr.d <-IQR(d$Mediciones)
desv_s.d;cuartiles.d;iqr.d


" Con ggplot, realiza un histograma separando la distribución de Mediciones por Categoría 
¿Consideras que sólo una categoría está generando el sesgo? No "

d.table <-table(cut(d$Mediciones, breaks = seq(0,300 , by = 50)),d$Categoria)
d.hist <- as.data.frame(d.table)


ggplot(d.hist, aes(Var1, fill=Var2)) + geom_bar(aes(weight=Freq), position="dodge") + xlab("Categoría") +
  labs(x= "Mediciones",  y="Frecuencias", fill="Categoría") + 
  labs(title = "Histograma por categoria")+
  theme_classic()



" Con ggplot, realiza un boxplot separando la distribución de Mediciones por Categoría y por Grupo dentro de cada categoría. 
¿Consideras que hay diferencias entre categorías? Si hay diferencias, de manera general el grupo '0' de cada categorai
contiene mas elenetos que los grupos '1', las medias de cada categoria son ditintas, al igual que sus IQR
¿Los grupos al interior de cada categoría podrían estar generando el sesgo? Si  
de elementos "

ggplot(d, aes(x=Categoria, y=Mediciones, fill=Grupo))+
  geom_boxplot()+
  labs(title="Boxplot de mediciones las por categoría y Grupo")+
  theme_classic()

resumen.mediciones <- d %>%
  group_by(Categoria, Grupo) %>%
  summarize(mean_m = mean(Mediciones),
            mediana_m=median(Mediciones),
            sd_m = sd(Mediciones),
   sd_m = sd(Mediciones),
   n = n())
resumen.mediciones

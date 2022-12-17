# Postwork sesión 8.
## Análisis de la Inseguridad Alimentaria en México
## OBJETIVO
- Realizar un análisis estadístico completo de un caso
- Publicar en un repositorio de Github el análisis y el código empleado
## REQUISITOS
- Haber realizado los works y postworks previos
- Tener una cuenta en Github o en RStudioCloud
## DESARROLLO
Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente los patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar tiene recursos financieros extrar al ingreso y en si presenta o no inseguridad alimentaria. Además, está interesado en un modelo que le permita identificar los determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por el Instituto Nacional de Salud Pública en México. La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:

- nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
- area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
- numpeho (Número de persona en el hogar)
- refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
- edadjef (Edad del jefe/a de familia)
- sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
- añosedu (Años de educación del jefe de familia)
- ln_als (Logarítmo natural del gasto en alimentos saludables)
- ln_alns (Logarítmo natural del gasto en alimentos no saludables)
- IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"
```
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
```
1. Plantea el problema del caso
2. Realiza un análisis descriptivo de la información
3. Calcula probabilidades que nos permitan entender el problema en México
4. Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México
5. Estima un modelo de regresión, lineal o logístico, para identificiar los determinanres de la inseguridad alimentaria en México
6. Escribe tu análisis en un archivo README.MD y tu código en un script de R y publica ambos en un repositorio de Github.

NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás analizar e interpretar todos tus resultados para poder dar una conclusión final al problema planteado.

# DESARROLLO DEL POSTWORK

## 1. PLANTEAMIENTO DEL PROBLEMA

A pesar de tener toda la descripción de lo que podiamos encontrar en el dataframe nos era muy necesario analizar los datos para ello en primer lugar se cargaron los datos usando el comando read.csv, y se almacenó en una variable de nombre DF (DataFrame). Al analizar el dataframe nos percatamos de que había datos faltantes (con valores NA) en varias filas y en diferentes columnas, teníamos dos opciones la primera de ellas era tomar la media o mediana del resto de los valores y sustituir los NA por el valor obtenido (media o mediana) dado que varias de las columnas contenían datos categóricos no resultaba viable sustituir de este modo los NA, la segunda opción era usar na.omit o complete.cases y dado que complete.cases devuelve un vector lógico que indica que casos están completos y na.omit devuelve el objeto con eliminación en lista de los valores faltantes decidimos utilizar la función complete.cases para obtener el vector indicando cuántos casos estaban completos. De las 40,809 observaciones solamente 20,280 estaban completas, de este modo obtuvimos un dataset de datos completos para realizar el análisis el cual corresponde a una muestra representativa del 49.69% de los casos iniciales.
Este nuevo dataframe filtrado, se le llamó df.limpio. Con esto se completó el primer paso de limpieza de datos.
```
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
sum(complete.cases(df))
df.limpio <- df[complete.cases(df),]
```
Posteriormente se verificó el tipo de dato de las 10 variables que contenía el dataframe mediante la función str. Del total de variables, 8 eran de tipo int, y las 2
restantes de tipo num. Sin embargo, al analizar el tipo de datos, se procedió a realizar la reclasificación de las siguientes variables: nivel socioeconómico 
(nse5f) sustituyendo los números por las categorías "Bajo", "Medio Bajo", "Medio", "Medio Alto", y "Alto"; zona geográfica (area) sustituyendo los números
por las categorías "Zona urbana" y "Zona Rural"; recursos financieros distintos al ingreso laboral (refin), sustiyendo según los valores boleanos "No" y "Si"; sexo
del jefe/jefa de familia (sexoje), sustiyendo con "Hombre" o "Mujer"; y la inseguridad alimentaria (IA) con los valores de "No presenta" o "Si presenta". 
```
str(df.limpio)
summary(df.limpio)
df.limpio$nse5f <- factor(df.limpio$nse5f, levels=c(1,2,3,4,5),labels=c("Bajo","Medio bajo","Medio","Medio alto", "Alto"))
df.limpio$IA <- factor(df.limpio$IA,levels=c(0,1),labels=c("No presenta IA", "Presenta IA") )
df.limpio$sexojef <- factor(df.limpio$sexojef,levels=c(0,1),labels=c("Hombre", "Mujer") )
df.limpio$area <- factor(df.limpio$area,labels =c("Urbana","Rural"))
df.limpio$refin <- factor(df.limpio$refin,labels =c("No","Si"))
str(df.limpio)
```
Con ello tenemos un dataframe completamente limpio y suficientemente legible para poder hacer los siguientes análisis y que los resultados obtenidos sean mucho más fáciles de interpretar.

## 2. ANÁLISIS DESCRIPTIVO DE LA INFORMACIÓN

Para la variable de "Logaritmo natural de Gasto en alimentos saludables" se realizó en primer lugar un Histograma de densidad, para ver la distribución de 
los datos.
```
hist(df.limpio$ln_als, prob=T, main="Logaritmo natural de Gasto en alimentos saludables", xlab="ln de Gastos en alimentos saludables")
```
Después se procedió a sacar las medidas de tendencia central (media, moda y mediana) de dicha variable, así como las medidas de dispersión (varianza, desviación
estándar y dispersión alrededor de la media:
```
"Medidad de tendencia central"
mean(ln_als)   #Media
median(ln_als) #Mediana
Mode(ln_als)   #Moda
```
```
"Medidas de dispersión"
var(ln_als)  #Varianza
sd(ln_als)   #Desviación estándar
IQR(ln_als)  #Dispersión alrededor de la media
```
Arrojando los siguientes resultados:
- Media: 6.191992
- Mediana: 6.27382
- Moda: 6.309918
- Varianza: 0.4741052
- Desviación estándar: 0.688553
- IQR: 0.789774

Se realizaron también las medidas de forma, mediante la función skewness y kurtosis. 
La distribución de los datos se asemeja a una distribución normal con sesgo a la izquierda. Sus medidas de tendencia central se ordenan de la siguiente manera:
  Media < Mediana < Moda
DUDA:Con la desviación estándar sd= 0.688 podemos observar que no hay mucha dispersión de los datos, y de manera gráfica a través del histograma observamos que tiene  sesgo a la izquierda forma leptocúrtica"

```
s1 <- skewness(ln_als)  #Sesgo
s1 # Presenta sesgo a la izquierda
c1 <- kurtosis(ln_als)  #Curtosis 
c1  #Presenta forma letpocúrtica
```

Se realizó el mismo análisis para la variable de "Logaritmo natural de gasto en alimentos no saludables". Haciendo primero el histograma y calculando las 
medidas de tendencia central y de dispersión:

```
hist(df.limpio$ln_alns, prob=T, main="Logaritmo natural de Gastos en alimentos no saludables",  xlab="ln de Gastos en alimentos saludables")
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
```
Arrojando los siguientes resultados:
- Media: 4.118845
- Mediana: 4.007333
- Moda: 3.401197
- Varianza: 1.084671
- Desviación estándar: 1.041476
- IQR: 1.466337

Se observa que para dicha variable, la distribución de los datos se asemeja a una distribución normal con sesgo a la derecha.Sus medidas de tendencia central se ordenan de la siguiente manera:
Media  =  Mediana  = Moda
Con la desviación estándar sd= 1.04 podemos observar que no hay mucha dispersión de los datos, y de manera gráfica a través del histograma observamos que tiene  un pequeño sesgo a la derecha y forma mesocúrtica" 

Para ahondar en el análisis de los datos, se realizó una gráfica Boxplot, por nivel socioeconómico y seguridad alimentaria, tomando como eje Y el gasto en alimentos
saludables.

```
ggplot(df.limpio, aes(x=nse5f, y=ln_als, fill=IA))+
  geom_boxplot()+
  labs( x = "Nivel socioeconómico", y = "Ln de gasto de alimentos saludables")+
  theme_classic()
  
```
Se realizó lo mismo tomando como eje Y el gasto en alimentos no saludables.
```
ggplot(df.limpio, aes(x=nse5f, y=ln_alns, fill=IA))+
  geom_boxplot()+
  labs( x = "Nivel socioeconómico", y = "Ln de gasto de alimentos no saludables")+
  theme_classic()
```
Observamos en esta gráfica que la IA varía muy poco entre su mismo nivel socioeconómico.
Observamos en esta gráfica que la IA varia muy poco entre su mismo nivel socioeconómico, además a diferencia del gasto en alimentos saludables esta clasificació presenta un IQR mas amplio.

Se realizó también una tabla resumen para observar la media, la mediana, la desviación estándar y el número de observaciones, según el nivel socioeconómico y
si presenta o no inseguridad alimentaria:
```
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
```

Se realizó también una gráfica Boxplot, por nivel socioeconómico y sexo del jefe de familia, tomando como eje Y el gasto en alimentos saludables, y una segunda
tomando como eje Y el gasto en alimentos no saludables. Se desarrolló también una tabla resumen sobre este análisis. 

```
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
```
Concluyendo que en estos boxplots no existe mucha diferencia a la clasificación por IA, el comportamiento es muy similar.

Después se realizó un análisis gráfico de Inseguridad alimentaria y Sexo del Jefe de Familia.

```
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
```
En la primera se puede observar que no hay muchas diferencias entre las diferentes categorias
Mientras que en la segunda podemos observar que las mujeres jefas de familia gastan menos en alimentos no saludables, mienstras que el gasto en alimentos saludables
es muy similar al de los hombres.

## 3. CÁLCULO DE PROBABILIDADES PARA ENTENDER EL PROBLEMA EN MÉXICO


## 4. HIPÓTESIS ESTADÍSTICAS PARA ENTENDER EL PROBLEMA EN MÉXICO

Para entender el problema en México se plantearon las siguientes hipótesis:

- 1. "En promedio el ln de gasto en alimentos saludables es en hogares con recursos financieros extras es mayor que en hogares que no lo tienen". Utilizando 
confianza del 99%, las hipótesis son:

Ho: promedio_ln_als (refin==Si) <= promedio_ln_als (refin==No)
Ha: promedio_ln_als (refin==Si) > promedio_ln_als (refin==No)

```
var.test(df.limpio[df.limpio$refin == "Si", "ln_als"], 
         df.limpio[df.limpio$refin == "No", "ln_als"], 
         ratio = 1, alternative = "two.sided",conf.level = 0.99)
```
Con este análisis se tiene que un p-value=0.01147 > 0.01 (99% NIVEL DE CONFIANZA)
```
t.test(df.limpio[df.limpio$refin == "Si", "ln_als"],
       df.limpio[df.limpio$refin == "No", "ln_als"],
       alternative = "greater",conf.level = 0.99, mu = 0, var.equal = TRUE,)
```
Con un p-value =3.119e-08 < 0.01 (99% NC). EEE para rechazar Ho. Es decir el promedio del gastos en alimentos saludables por hogares con recursos financieros extras
no es mayor a los que si lo tienen.

- 2. En promedio el gasto en alimentos no saludables es en hogares con una mujer como jefe de familia es menor que en hogares cuya jefe de familia es hombre".
Utilizando un nivel de confianza de 90%, las hipótesis son:

Ho: promedio_ln_alns (sexojef==mujer) >= promedio_ln_als (sexojef==Hombre)
Ha: promedio_ln_alns (sexojef==mujer) < promedio_ln_als (sexojef==Hombre)
```
var.test(df.limpio[df.limpio$sexojef == "Mujer", "ln_alns"], 
         df.limpio[df.limpio$sexojef == "Hombre", "ln_alns"], 
         ratio = 1, alternative = "two.sided",conf.level = 0.90)
```
Obteniendo un p-value = 0.377 > 0.1 (90% NC)
```
t.test(df.limpio[df.limpio$sexojef == "Mujer", "ln_alns"],
       df.limpio[df.limpio$sexojef == "Hombre", "ln_alns"],
       alternative = "less",conf.level = 0.90, mu = 0, var.equal = TRUE,)
 ```
Con un p-value ~ 2.2e-16 < 0.1 (90% NC). EEE para rechazar Ho. Es decir el promedio del gastos en alimentos no saludables por hogares con jefe de familia mujer no
es menor a los hogares que tienen jefe de familia mujer.

- 3. En promedio La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas
con mayores niveles socioeconómicos", utilizando un nivel de confianza de 90%, las hipótesis son:

Ho: promedio_ln_alns (nse5f==("Bajo" | "Medio bajo" ) >= promedio_ln_als (nse5f==("Medio alto" | "Alto" )
Ho: promedio_ln_alns (nse5f==("Bajo" | "Medio bajo" ) < promedio_ln_als (nse5f==("Medio alto" | "Alto" )
```
var.test(df.limpio[(df.limpio$nse5f == "Bajo" | df.limpio$nse5f == "Medio Bajo"), "ln_alns"], 
         df.limpio[(df.limpio$nse5f == "Medio Alto" | df.limpio$nse5f == "Alto"), "ln_alns"], 
         ratio = 1, alternative = "two.sided",conf.level = 0.90)
```
Obteniendo un p-value = 6.199e-13 < 0.1 (90% NC). Las varianzas no son similares. No EEE suficiente para evaluar las hipótesis.

- 4. En promedio el gasto en alimentos saludables en hogares con IA es igual a los que no presentan IA. Con un nivel de confianza de 90% las hipótesis son:

Ho: promedio_ln_als (IA==sI) == promedio_ln_als (IA==No)
Ha: promedio_ln_als (IA==sI) != promedio_ln_als (IA==No)

```
var.test(df.limpio[df.limpio$IA == "No presenta IA", "ln_als"], 
         df.limpio[df.limpio$IA == "Presenta IA", "ln_als"], 
         ratio = 1, alternative = "two.sided",conf.level = 0.90)
```
Obteniendo un p-value =0.3532 > 0.1 (90% NC)
```
t.test(df.limpio[df.limpio$IA == "No presenta IA", "ln_als"],
       df.limpio[df.limpio$IA == "Presenta IA", "ln_als"],
       alternative = "two.sided",conf.level = 0.90, mu = 0, var.equal = TRUE,)
```
Con un p-value < 2.2e-16 < 0.1 (90% NC). EEE para rechazar Ho. Es decir el promedio del gastos en alimentos saludables por hogares con IA no es igual a los hogares
que no presentan IA.

- 5. El promedio para el gasto de alimentos saludables es el mismo para todos los niveles socioeconómicos. Con un nivel de confianza del 90% se tiene:

Ho: promedio_ln_alns (nse5f==Bajo) == promedio_ln_alns (nse5f==Medio Bajo) == promedio_ln_alns (nse5f==Medio) ...
Ha: Al menos un par de promedios del gasto en alimentos saludables en dos niveles socioeconómicos son diferentes.

```
boxplot(ln_als ~ nse5f, data = df.limpio)

anova <- aov(ln_als ~ nse5f, data = df.limpio)
summary(anova)

TukeyHSD(anova)
plot(TukeyHSD(anova))
```
Con p-value <2e-16
p-value < 0.1 (90% NC) EEE para rechazar Ho. Hemos obtenido un p-valor del orden de 2e-16. Esto nos permite rechazar la hipótesis nula en favor de la hipotesis 
alternativa y concluir que al menos el promedio del gasto en alimentos saludables de dos niveles socioeconómicos es distinto. La gráfica de diferencias de promedios
y el diagrama boxplot apoyan visualmente el rechazo de la hipótesis nula.

```
summary(logistic.1)
```

## 5. MODELO DE REGRESIÓN PARA IDENTIFICAR LAS DETERMINANTES DE LA SITUACIÓN ALIMENTARIA EN MÉXICO


La primera parte es obtener una matriz de correlación manteniendo IA como variable a comparar:

Se genra una matriz dxe correlación que incluye a IA:

```
df.select <- select(df, IA,nse5f, numpeho, edadjef, añosedu, ln_als, ln_alns)
round(cor(df.select),4)
```

"        IA      nse5f    numpeho  dadjef añosedu ln_als  ln_alns
IA       1.0000 -0.2811  0.0967      NA -0.2114     NA      NA
nse5f   -0.2811  1.0000  0.0430      NA  0.4262     NA      NA
numpeho  0.0967  0.0430  1.0000      NA  0.0080     NA      NA
edadjef      NA      NA      NA       1      NA     NA      NA
añosedu -0.2114  0.4262  0.0080      NA  1.0000     NA      NA
ln_als       NA      NA      NA      NA      NA      1      NA
ln_alns      NA      NA      NA      NA      NA     NA       1"


Partiendo del DataFrame "limpio" o sin datos de "NA" se procede a la generación de una matriz de correlación sin incluir a IA y así comparando con la tabla
anterior.

```
adv.select <- select(adv, Sales,TV, Radio, Newspaper)
round(cor(adv.select),4)
```
La primera matriz va a permitir contrastar 4 variables (numpeho,edadjef, añoseduln_als,ln_alns) para ver si existe correlación tomando como base el nivel 
socioeconómico del hogar nse5f

```
dfLimpio.select <- select(dfLimpio, nse5f, numpeho, edadjef, añosedu, ln_als, ln_alns)
round(cor(dfLimpio.select),4)
```

Obteniendo la siguiente tabla de correlación

"        nse5f numpeho edadjef añosedu  ln_als ln_alns
nse5f   1.0000  0.0495  0.0451  0.4335  0.3566  0.2922
numpeho 0.0495  1.0000 -0.1140 -0.0435  0.3109  0.0809
edadjef 0.0451 -0.1140  1.0000 -0.3566 -0.1127 -0.0984
añosedu 0.4335 -0.0435 -0.3566  1.0000  0.2421  0.2184
ln_als  0.3566  0.3109 -0.1127  0.2421  1.0000  0.3285
ln_alns 0.2922  0.0809 -0.0984  0.2184  0.3285  1.0000"

Teniendo las siguientes observaciones:

1. Hay una correlación débil entre El nivel socioeconómico del hogar y los gastos en alimentos saludables y no saludables (0.3566 y 0.2922, respectivamente)
2. Hay una correlación prácticamente positiva moderada entre el Nivel Socioeconómico y los años de educación del jefe de familia

Recordando que: 

- nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto""
- ln_als (Logarítmo natural del gasto en alimentos saludables)
- ln_alns (Logarítmo natural del gasto en alimentos no saludables)

A pesar de que no se incluye IA se puede observar que existe una correlación positiva débil entre el Nivel Socioeconómico y de añosedu de 0.433, también con gasto
de alimentos no slaudables que es de 0.2922.

A continuación se genera una matriz de dispersión relacionando las variables a través de ~
Como son muchos datos se divide en dos presentaciones gráficas:

```
pairs(~ IA+nse5f +ln_als+ln_alns,
      data = dfLimpio, gap = 0.4, cex.labels = 1.5)

pairs(~ IA+nse5f+area+numpeho+refin +ln_als+ln_alns,
      data = dfLimpio, gap = 0.4, cex.labels = 1.5)
```
Estimación del modelo de regresión, lineal o logístico, para identificiar los determinanres de la inseguridad alimentaria en México.

Se inician con modelos de Regresión Lineal:
```
attach(dfLimpio) 
modelo1 <- lm(IA ~ nse5f +numpeho+edadjef+añosedu+ln_als+ln_alns)
summary(modelo1)
```
Obteniendo: 

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

- Regresión logística

```
logistic.prueba1<-glm(IA ~ nse5f+area+numpeho+refin+edadjef+sexojef+añosedu+ln_als+ln_alns, data=dfLimpio,family = binomial())
summary(logistic.prueba1)
exp(coef(logistic.prueba1))
```
Mostrando la siguiente tabla"

"(Intercept)       nse5f        area     numpeho       refin     edadjef     sexojef     añosedu      ln_als     ln_alns 
 19.3437901   0.6896405   0.9021626   1.1907381   1.4887174   1.0003946   1.1678150   0.9472396   0.9131167   0.9021909 "

Se puede interpretar que los recursos financieros distintos al ingreso laboral (refin) tienen más alta probabilidad sobre la Insuficiencia Laboral y el segundo
factor es el número de personas en el hogar. Se analiza específicamente con el Número de personas:
```
y<-df$IA
x<-df$numpeho
logistic.prueba1<-glm(y ~x,data=dfLimpio, family = binomial())
summary(logistic.prueba1)
exp(coef(logistic.prueba1))
```

Dando los siguiente resultados:

"(Intercept)           x 
1.728377    1.132376 "

y se genra la gráfica para representar la relación:

```
plot(IA ~ numpeho, data=dfLimpio, xlim = c(0,10))
curve(predict(logistic.prueba1, newdata = data.frame(x), type = "response"),
      add = TRUE)
```

Considerando lo anterior: Se concluye que la Insuficiencia Alimentaria le afecta el Número de personas del hogar.










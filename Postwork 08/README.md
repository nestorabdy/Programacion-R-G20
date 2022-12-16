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
Esta nueva base de datos filtrada, se le llamó df.limpio. Con esto se completó el primer paso de limpieza de datos.
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

## 5. MODELO DE REGRESIÓN PARA IDENTIFICAR LAS DETERMINANTES DE LA SITUACIÓN ALIMENTARIA EN MÉXICO













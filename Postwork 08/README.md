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

DESARROLLO DEL POSTWORK

1. Planteamiento del problema del caso

En primer lugar se cargó la base de datos usando el comando read.csv, y se le llamó DF. Al observar la base de datos nos percatamos de que había datos 
faltantes o NA en varias filas, por lo que se tomó la decisión de utilizar la función complete.cases para que regresara el vector indicando cuántos estaban completos. De las 40,809 observaciones solamente 20,280 estaban completas, y dado que varias de las columnas contenían datos categóricos, por lo que no resultaba
viable sustituir los valores NA con la media o la mediana, se tomó la decisión de utilizar como base de datos para realizar el análisis los datos completos.
Esta nueva base de datos filtrada, se llama df.limpio. Con esto se completó el primer paso de limpieza de datos.
```
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
sum(complete.cases(df))
df.limpio <- df[complete.cases(df),]
```
Después se verificó el tipo de las 10 variables que contenía la base de datos mediante la función str. Del total de variables, 8 eran de tipo int, y las 2
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

2. Análisis descriptivo de la información

Para la variable de "Logaritmo natural de Gasto en alimentos saludables" se realizó en primer lugar un Histograma de densidad, para ver la distribución de 
los datos.
```
hist(df$ln_als, prob=T, main="Logaritmo natural de Gasto en alimentos saludables", xlab="ln de Gastos en alimentos saludables")
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
Media: 6.191992
Mediana: 6.27382
Moda: 6.309918
Varianza: 0.4741052
Desviación estándar: 0.688553
IQR: 0.789774


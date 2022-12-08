# Postwork Sesión 2.

#### Objetivo

- Conocer algunas de las bases de datos disponibles en `R`
- Observar algunas características y manipular los DataFrames con `dplyr`
- Realizar visualizaciones con `ggplot`
#### Requisitos

1. Tener instalado R y RStudio
2. Haber realizado el prework y estudiado los ejemplos de la sesión.

#### Desarrollo

1) Inspecciona el DataSet iris_meaniris` disponible directamente en R. Identifica las variables que contiene y su tipo, asegúrate de que no hayan datos faltantes y 
que los datos se encuentran listos para usarse.

- Nombres del dataframe *iris*
 ```R
names(iris)
 ```
 Resultado
  ```R
[1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species" 
 ```
 - Estructura del dataframe *iris*
  ```R
str(iris)
 ```
 Resultado
  ```R
'data.frame':	150 obs. of  5 variables:
 $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
 $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
 $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
 $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
 $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
 ```
 - Dimensión del dataframe *iris* 
  ```R
dim(iris)
 ```
 Resultado
  ```R
[1] 150   5
> 
 ```
  ```R
sum(complete.cases(iris))
 ```
 Resultado
  ```R
[1] 150
 ```
 > El data frame esta completo, la dimension es igual al numero de casos completos.

2) Crea una gráfica de puntos que contenga `Sepal.Lenght` en el eje horizontal, `Sepal.Width` en el eje vertical, que identifique `Species` por color y que el tamaño de la figura está representado por `Petal.Width`. Asegúrate de que la geometría contenga `shape = 10` y `alpha = 0.5`.
  ```R
g.iris <- ggplot(iris, aes(x = Sepal.Length, y=Sepal.Width, color = Species, size = Petal.Width)) + 
  geom_point(shape=10, alpha=0.5) 
 ```
(https://myoctocat.com/assets/images/base-octocat.svg)

3) Crea una tabla llamada `iris_mean` que contenga el promedio de todas las variables agrupadas por `Species`.

4) Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos para agregar los promedios en la visualización. Asegúrate que el primer argumento de la geometría sea el nombre de tu tabla y que los parámetros sean `shape = 23`, `size = 4`, `fill = "black"` y `stroke = 2`. También agrega etiquetas, temas y los cambios necesarios para mejorar tu visualización.

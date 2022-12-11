# Postwork Sesión 1.
## Objetivo
El Postwork tiene como objetivo que practiques los comandos básicos aprendidos durante la sesión, de tal modo que sirvan para reafirmar el conocimiento. Recuerda que la programación es como un deporte en el que se debe practicar, habrá caídas, pero lo importante es levantarse y seguir adelante. Éxito

## Requisitos
- Concluir los retos
- Haber estudiado los ejemplos durante la sesión
## Descripción
El siguiente postwork, te servirá para ir desarrollando habilidades como si se tratara de un proyecto que evidencie el progreso del aprendizaje durante el módulo, sesión a sesión se irá desarrollando.

A continuación aparecen una serie de objetivos que deberás cumplir, es un ejemplo real de aplicación y tiene que ver con datos referentes a equipos de la liga española de fútbol (recuerda que los datos provienen siempre de diversas naturalezas), en este caso se cuenta con muchos datos que se pueden aprovechar, explotarlos y generar análisis interesantes que se pueden aplicar a otras áreas. Siendo así damos paso a las instrucciones:
 ## Desarrollo

1. Del siguiente enlace, descarga los datos de soccer de la temporada 2019/2020 de la primera división de la liga española: https://www.football-data.co.uk/spainm.php

2. Importa los datos a R como un Dataframe
```R
sp1 <- read.csv("SP1.csv")
```

3. Del dataframe que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG); guárdalos en vectores separados
```R
FTHG <- sp1$FTHG
FTAG <- sp1$FTAG
```
4. Consulta cómo funciona la función table en R. Para ello, puedes ingresar los comandos help("table") o ?table para leer la documentación.
```R
?table
goles <- table(FTHG,FTAG)
goles
dim(goles)
```
 
|FTHG\FTAG| 0 | 1 | 2 | 3 | 4 | 5 |
|----|---|---|---|---|---|---| 
| **0** | 33| 28| 15|  8|  2|  2|
| **1** | 43| 49| 32|  5|  3|  0|
| **2** | 39| 35| 20|  3|  2|  0|
| **3** | 14| 14|  7|  2|  1|  0|
| **4** |  4|  5|  4|  0|  1|  0|
| **5** |  2|  3|  3|  0|  0|  0|
| **6** |  1|  0|  0|  0|  0|  0|
 
5. Responde a las siguientes preguntas: 
 - ¿Cuántos goles tuvo el partido con mayor empate? 
```R
#Para obtener los empates obtenemos los valores de la diagonal principal de la tabla. Descartamos aquellos conteos que sean iguales a 0
empates <- diag(goles)
empates
```
Resultado
```R
0   1  2  3  4  5  
33 49 20  2  1  0 
# Del resultado vemos que el empate con la mayor cantidad de goles fue un partido con marcador 4-4
```
> 8 goles en total, el empate fue 4 - 4
```
#Desarrollo comprobacionde resultado visual: convertir 0 a NA y omitir NA
(empates[empates==0] <- NA)
(empates<-na.omit(empates))
#Extraer el de mayor empate
(mayor_empate<-tail(empates,1))

#Cuantos goles tuvo el mayor empate ?
(totgoles<-(as.numeric(names(mayor_empate))*2))
```
```
>Resultado 8
```
 - ¿En cuántos partidos ambos equipos empataron 0 a 0? 
```R
#Para obtener los empates obtenemos los valores de la diagonal principal de la tabla. Descartamos aquellos conteos que sean iguales a 0
partidos.empatados0.0 <- goles[1,1]
partidos.empatados0.0
```
Resultado
```
> 33
```
> En 33 partidos los equipos empataron 0-0
 - ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?
```R
partidos.mayor.goleada <- goles[length(goles[,1]),1]
partidos.mayor.goleada
```
Resultado
```
> 1
```
> 1 partido con el marcador 6-0

Notas para los datos de soccer: https://www.football-data.co.uk/notes.txt

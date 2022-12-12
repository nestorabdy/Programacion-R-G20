"El data frame iris contiene información recolectada por Anderson sobre 50 flores 
de 3 especies distintas (setosa, versicolor y virginca), incluyendo medidas en 
centímetros del largo y ancho del sépalo así como de los pétalos.

Estudios recientes sobre las mismas especies muestran que:

En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm
En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm
En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que
el promedio del largo del pétalo de la especie versicolor.
En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.
Utilizando pruebas de inferencia estadística, concluye si existe evidencia suficiente 
para concluir que los datos recolectados por Anderson están en línea con los nuevos estudios.

Utiliza 99% de confianza para toda las pruebas, en cada caso realiza 
el planteamiento de hipótesis adecuado y concluye."

str(iris)
?t.test

#1.-En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm
#Ho : prom_slenght_setosa == 5.7
#Ha : prom_slenght_setosa =! 5.7

t.test(iris[iris$Species == 'setosa', "Sepal.Length"],conf.level=0.99,
       alternative = 'two.sided', mu=5.7)
"pvalue = 0.06527 > 0.01 (99% NC) 
EEE para rechazar Ho a favor de Ha
es decir el promedio es diferente a 5.7"


#2.-En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm
#Ho : prom_ancho_virginica >= 2.1
#Ha : prom_ancho_virginica <2.1

t.test(iris[iris$Species=="virginica", "Petal.Width"],conf.level=0.99,
       alternative = "less", mu=2.1)
"pvalue = 0.03132 > 0.01  (99% NC) No EEE para rechazar (NO rechazo Ho)
es decir el promedio del ancho es mayor o igual que 2.1"


"3.-En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que
el promedio del largo del pétalo de la especie versicolor"
#Ho: prom_petal_width_virginica-prom_petal_width_versicolor <= 1.1
#Ha: prom_petal_width_virginica-prom_petal_width_versicolor > 1.1

?var.test

var.test(iris[iris$Species == "virginica", "Petal.Length"], 
         iris[iris$Species == "versicolor", "Petal.Length"], 
         ratio = 1, alternative = "two.sided",conf.level = 0.99)
#pvalue= 0.2637> 0.01 (99% NC) No EEE para rechazar Ho
#es decir las varianzas son iguales (se aproximan)


t.test(iris[iris$Species == "virginica", "Petal.Length"],
       iris[iris$Species == "versicolor", "Petal.Length"],
       alternative = "greater",conf.level = 0.99, mu = 1.1, var.equal = TRUE,)

#pvalue =0.03203 > 0.01 (99% NC) No EEE para rechazar Ho
#Es decir el promedio de largo del petalo virginica no es mas grande que el de la versicolor


#4.-En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.

?anova
#Ho: El promedio del ancho del sépalo de las tres especies es igual
#Ha: Existe al menos un par especies cuyos promedios de ancho de sépalo es distinto "

tapply(iris$Sepal.Width, iris$Species, mean)

boxplot(Sepal.Width ~ Species, data = iris)

anova <- aov(Sepal.Width ~ Species,
             data = iris)

summary(anova)

##Observamos la tabla de resumen para los diferentes promedios del ancho de sépalo de las especies de flor iris


#Graficamos la diferencia de promedios para ver gráficamente que las diferencias de dos promedios son disitintos
TukeyHSD(anova)
plot(TukeyHSD(anova))

#Hemos obtenido un p-valor del orden de 2e-16 Esto nos permite rechazar la hipótesis nula y concluir que el ancho se sepalo para las diferentes especies  de flor iris consideradas NO son iguales

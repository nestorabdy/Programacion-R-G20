####### SESIÓN 5: TEOREMA DEL LÍMITE CENTRAL E INFERENCIA ESTADÍSTICA  ########
###############################################################################
set.seed(2022)

## EJEMPLO 01: TEOREMA DEL LÍMITE CENTRAL
"Todos los estadísticos son en sí mismos variables aleatorias, ya que su valor 
depende de la muestra. Para conocer la distribución muestral (de un estadístico) 
es necesario extrear muestras aleatorias repetidas de tamaño n.

Veamos un ejemplo para la media de una distribución exponencial:"

par(mfrow=c(2,2))

{sample5 <- c()
  n <- 5
  for (i in 1:10000) {
    sample5[i] <- mean(rexp(n = n, rate = 0.1))
  }
  hist(sample5, main = "n = 5", xlab = "")}

{sample10 <- c()
  n <- 10
  for (i in 1:10000) {
    sample10[i] <- mean(rexp(n = n, rate = 0.1))
  }
  hist(sample10, main = "n = 10", xlab = "")}

{sample100 <- c()
  n <- 100
  for (i in 1:10000) {
    sample100[i] <- mean(rexp(n = n, rate = 0.1))
  }
  hist(sample100, main = "n = 100", xlab = "")}

{sample1000<- c()
  n <- 1000
  for (i in 1:10000) {
    sample1000[i] <- mean(rexp(n = n, rate = 0.1))
  }
  hist(sample1000, main = "n = 1000", xlab = "")}
dev.off()

"El Teorema del Límite Central establece que, dada una muestra aleatoria suficientemente 
grande de la población, la distribución muestral de la media seguirá una distribución normal 
con un promedio igual al de la población y un error estándar igual a desv. est/sqrt(n)"

mean(sample1000); 1/0.1
sd(sample1000); sqrt(1/0.1**2)/sqrt(1000)

## EJEMPLO 02.a: INFERENCIA A LA MEDIA DE UNA POBLACIÓN

# Muestra grande (n >= 30) o desv. estándar poblacional conocida
"Cuando la muestra es grande o conocemos la desv. estandar de la población, el estadístico 
de prueba que utilizaremos para tomar una decisión sobre la hipótesis nula tendrá 
una distribución normal estándar, con la cual se calcularán los valores p del 
estadístico de prueba."

# Muestra pequeña (n < 30) y desv. estándar poblacional desconocida
"Cuando la muestra es pequeña y no conocemos la desviación estándar de la población, 
podemos utilizar un estimador, conocido como desviación estándar de la muestra. 
Esto hace que el estadístico de prueba cambie su distribución a una t de Student.

Esta distribución tiene características similares a la distribución normal estándar, 
salvo que tiene un único parámetro (grados de libertad) y es utilizada 
preferentemente en lugar de la distribución Z, ya que a medida que el tamaño de 
la muestra es más grande, su densidad se acerca a la de la distribución normal estándar"

{curve(dnorm(x), from = -4, to = 4, xlab = "X", ylab = "f(x)", main = "Distribución t - Student")
  legend(x = 2, y = 0.4, legend=c("N(0,1)", "df=1", "df=5", "df=10", "df=100"),
         col=c("black","blue", "green", "orange", "red"), lty = 1, bty = "n", cex=0.8)}
curve(dt(x, 1), from = -4, to = 4, col = "blue", add = TRUE)
curve(dt(x, 5), from = -4, to = 4, col = "green", add = TRUE)
curve(dt(x, 10), from = -4, to = 4, col = "orange", add = TRUE)
curve(dt(x, 100), from = -4, to = 4, col = "red", add = TRUE)

"Ejemplo: Un estudio anterior de telecomunicaciones señala que, en promedio, el 
total de llamadas internacionales es menor a 4.54. ¿A qué NC EEE para concluir 
que lo mismo sucede en nuestro mercado?"
#NC NIvel de confianza
#EEE existe evidencia estadistica

# Ho: <=, =, >=
# Ha: >, =!, <

# Nivel de confianza: 90%, 95%, 99%
# Nivel de significancia (1 - NC): 0.1, 0.05, 0.01


df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")

"Planteamiento de hipótesis:"
#H0 : mu >=4.54
#Ha : mu < 4.54 cola inferior

t.test(x=df$total_intl_calls, altermative ="less", mu =4.54)

#Toma decision
# Toma de decisión: Si pvalue >= significancia -> No rechazo Ho
#                   Si pvalue < significancia -> Rechazo Ho

# A nivel de confianza del 90%, existe evidencia estadística para rechazar Ho, es decir,
# El promedio es menor a 4.54.



"Ejemplo: El mismo estudio, señala que, en promedio, el número de mensajes de voz 
es mayor a 7.79 A un NC del 95%, ¿EEE para concluir que lo mismo sucede en nuestro mercado?"

"Planteamiento de hipótesis:"
#Ho :mu <= 7.79
#Ha : mu > 7.79 cola superior

t.test(x=df$number_vmail_messages, alternative="greater", mu =7.79)

# A nivel de confianza del 95%, no existe evidencia estadística para rechazar Ho, es decir,
# El promedio no es mayor a 7.79.



"Ejemplo: El mismo estudio, señala que, en promedio, el número de llamadas de servicio 
es igual a 1.59 A un NC del 95%, ¿EEE para concluir que lo mismo sucede en nuestro mercado?"

"Planteamiento de hipótesis:"
##Ho :mu = 1.59
#Ha : mu =! 1.59 

t.test(x=df$customer_service_calls, alternative="two.sided", ,mu=1.59)

# Para prueba de dos colas: Nivel de signifancia/2

#Nivel confianza = 0.05 -> 0.05 /2 = 0.025

# A niveles de confianza estándar (90%, 95% y 99%), no existe evidencia estadística 
# para rechazar Ho, es decir El promedio no es distinto o es igual a 1.59.

#Toma de desición:
#Si el p-value es >= a la significancia entonces no rechazo la hipotesis nula
#Si el p-value es < a la significancia entonces rechazo la hipotesis nula



## EJEMPLO 02.b: INFERENCIA A LA MEDIA DE DOS POBLACIONES
"En inferencia estadística también podemos hacer comparaciones entre la media de 
una variable para diferentes grupos.

En el caso de comparación de dos medias, el estadístico a utilizarse puede distribuir 
como una normal estándar o una t - Student, dependiende si se cumplen las condiciones 
antes mencionadas. Para el caso particular de la distribución t - Student, es necesario 
verificar si las desviaciones estándar de cada grupo son iguales o diferentes en 
la población, por lo que realizaremos una prueba estadística para verificar esto:"

"Ejemplo: El mismo estudio, señala que, el promedio de llamadas de atención a clientes
en los usuarios que cancelaron el servicio es mayor que los que no cancelaron
A un NC del 90%, ¿EEE para concluir que lo mismo sucede en nuestro mercado?"

"Planteamiento de hipótesis:
Ho: prom_customer_service_calls_churn1 <= prom_customer_service_calls_churn2 
Ha: prom_customer_service_calls_churn1 > prom_customer_service_calls_churn2"

#checar si las varianzas son iguales o dif


#Ho : razon =1
#Ha : razon !=1

var.test(df[df$churn == 1, "customer_service_calls"],
         df[df$churn == 0, "customer_service_calls"],
         ratio = 1, alternative = "two.sided")

t.test(x = df[df$churn == 1, "customer_service_calls"], 
       y = df[df$churn == 0, "customer_service_calls"],
       alternative = "greater", mu = 0, var.equal = FALSE)


#A niveles de confianza std EEE para rechazar Ho
#el promedio de llamadas de atención a clientes
#en los usuarios que cancelaron el servicio es mayor que los que no cancelaron


"Ejemplo: Prueba que, en promedio, el número de llamadas internacionales realizadas 
por los usuarios que cancelaron es igual a las realizadas por quienes no cancelaron"

"Planteamiento de hipótesis:
Ho: prom_total_intl_calls_churn1 == prom_total_intl_calls_churn2 
Ha: prom_total_intl_calls_churn1 =! prom_total_intl_calls_churn2"
var.test(df[df$churn == 1, "total_intl_calls"], 
         df[df$churn == 0, "total_intl_calls"], 
         ratio = 1, alternative = "two.sided")

#no rechazo p value >= significancia

## EJEMPLO 03: ANÁLISIS DE VARIANZA (ANOVA)
"El análisis de varianza (de un factor) nos permite comparar la media de una variable 
considerando dos o más niveles/grupos de factor. Entre muchas otras aplicaciones 
del ANOVA, esta técnica puede emplearse como una extensión de la prueba t de Student."

"Ejemplo: ¿Existe evidencia estadística para concluir que, en promedio, el tipo de 
corte tiene efectos sobre el quilate del diamante?"

"Planteamiento de hipótesis:
Ho: prom_carat_cut_fair = prom_carat_cut_good = prom_carat_cut_very_good = prom_carat_cut_premium = prom_carat_cut_ideal
Ha: Al menos uno es diferente."
library(ggplot2)
str(diamonds)

boxplot(log(diamonds$carat))

boxplot(log(carat) ~ cut,
        data = diamonds)

##checar si las varianzas son iguales o dif

?anova

#Ho : razon =1
#Ha : razon !=1

anova(diamonds$carat, diamonds$cut)

anova <- aov(carat ~ cut,
             data = diamonds)

summary(anova)

"Utilizando la variable total_intl_charge de la base de datos telecom_service.csv de la sesión 3,
realiza un análisis probabilístico. Para ello, debes determinar la función de distribución de
probabilidad que más se acerque el comportamiento de los datos. Hint: Puedes apoyarte de medidas
descriptivas o técnicas de visualización."

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
summary(df)
library(DescTools)
(promedio <-mean(df$total_intl_charge))
(moda <- Mode(df$total_intl_charge))
(mediana <-median(df$total_intl_charge))
(desv <- sd(df$total_intl_charge))

# Una vez que hayas seleccionado el modelo, realiza lo siguiente:
# Grafica la distribución teórica de la variable aleatoria total_intl_charge (Histograma?)

hist(df$total_intl_charge, prob=T, main="Histograma \ndel total de Cargos internacionales")

curve(dnorm(x, mean = promedio, sd = desv), from=0, to=5, col='blue', main = "Distribución \n teórica", ylab = "f(x)", xlab = "X")

# ¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?

pnorm(q = 1.85, mean = promedio, sd = desv, lower.tail = TRUE)
  
# ¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?

pnorm(q = 3, mean = promedio, sd = desv, lower.tail = FALSE)
  
# ¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?

pnorm(q = 4.85, mean = promedio, sd = desv) - pnorm(q = 2.35, mean = promedio, sd = desv)
  
# Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?

qnorm(p = 0.48, mean = promedio, sd = desv)
  
# ¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 80% de
# probabilidad?

qnorm(p = 0.10, mean = promedio, sd = desv); qnorm(p = 0.90, mean = promedio, sd = desv)

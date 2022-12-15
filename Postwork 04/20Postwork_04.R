"Utilizando la variable total_intl_charge de la base de datos telecom_service.csv de la sesión 3, 
realiza un análisis probabilístico. Para ello, debes determinar la función de distribución de probabilidad 
que más se acerque el comportamiento de los datos. Hint: Puedes apoyarte de medidas descriptivas o técnicas de visualización."

#Una vez que hayas seleccionado el modelo, realiza lo siguiente:
d <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
df <- d$total_intl_charge

barplot(table(df)/length(df),
        main = "Total de cargos internacionales", 
        xlab = "X")

#Grafica la distribución teórica de la variable aleatoria total_intl_charge

df.mean <-mean(df)
df.sd <- sd(df)
curve(dnorm(x, mean = df.mean, sd = df.sd), from=min(df), to=max(df), 
      col='blue', main = "Densidad de Probabilidad del Total de Cargos Internacionales",
      ylab = "f(x)", xlab = "X")

x <- seq(-4, 4, 0.01)*df.sd + df.mean
y <- dnorm(x, mean =  df.mean, sd = df.sd) 
#¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?
pnorm(q = 1.85, mean = df.mean, sd = df.sd)
par(mfrow = c(2, 3))
plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 2.76, " y ", sigma == 0.753)))
polygon(c(min(x), x[x<=1.85], 1.85), c(0, y[x<=1.85], 0), col="red")

#¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?
pnorm(q = 3, mean = df.mean, sd = df.sd,lower.tail = FALSE)
plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 2.76, " y ", sigma == 0.753)))
polygon(c(3, x[x>=3], max(x)), c(0, y[x>=3], 0), col="blue")

#¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?
pnorm(q = 4.85, mean = df.mean, sd = df.sd)-pnorm(q = 2.3, mean = df.mean, sd = df.sd)
plot(x, y, type = "l", xlab="", ylab="")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 2.76, " y ", sigma == 0.753)))
polygon(c(2.35, x[x>=2.35 & x<=4.85], 4.85), c(0, y[x>=2.35 & x<=4.85], 0), col="green")

#Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?
c <-qnorm(p = 0.48, mean = df.mean, sd = df.sd)

c
plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 2.76, " y ", sigma == 0.753)))
polygon(c(min(x), x[x<=c], c), c(0, y[x<=c], 0), col="magenta")

#¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 80% de probabilidad?
a<-qnorm(p = 0.2/2, mean = df.mean, sd = df.sd); b<-qnorm(p = 0.2/2, mean = df.mean, sd = df.sd, lower.tail = FALSE)
a
b
plot(x, y, type = "l", xlab="", ylab="")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 2.76, " y ", sigma == 0.753)))
polygon(c(a, x[x>=a & x<=b], b), c(0, y[x>=a & x<=b], 0), col="yellow")

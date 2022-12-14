################ SESIÓN 7: SERIES DE TIEMPO ##################
##############################################################

# POST WORK, Eq 20"

#Utilizando el siguiente vector numérico, realiza lo que se indica:

#url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
url =  "global.txt"

Global <- scan(url, sep="")
class(Global)

# COMENTARIO: 150 renglones (años) x 12 columnas (meses), 1856 +150 = 2006  
# -0.384 -0.457 -0.673 -0.344 -0.311 -0.071 -0.246 -0.235 -0.380 -0.418 -0.670 -0.386 
# -0.437 -0.150 -0.528 -0.692 -0.629 -0.363 -0.375 -0.328 -0.495 -0.646 -0.754 -0.137 
# -0.452 -1.031 -0.643 -0.328 -0.311 -0.263 -0.248 -0.274 -0.203 -0.121 -0.913 -0.197 
# -0.249 -0.041 -0.082 -0.172 -0.085 -0.278 -0.220 -0.132 -0.436 -0.234 -0.288 -0.486 
# -0.070 -0.526 -0.599 -0.420 -0.273 -0.063 -0.182 -0.256 -0.213 -0.326 -0.696 -0.813 
# -0.858 -0.415 -0.431 -0.443 -0.735 -0.169 -0.227 -0.131 -0.377 -0.375 -0.434 -0.209 
# -0.711 -0.817 -0.435 -0.232 -0.194 -0.322 -0.466 -0.623 -0.345 -0.382 -0.932 -0.768 
# 0.263 -0.063 -0.379 -0.187 -0.320 -0.365 -0.510 -0.359 -0.291 -0.431 -0.362 -0.276 
# -0.834 -0.604 -0.516 -0.482 -0.399 -0.200 -0.138 -0.332 -0.394 -0.711 -0.507 -0.587 

# 1. Crea un objeto de serie de tiempo con los datos de Global. La serie debe ser mensual comenzando en Enero de 1856
# COMENTARIO: empieza en el año 1856, con 12 observaciones por unidad de tiempo
Global.ts <- ts(Global, st = c(1856, 1), fr = 12)


# 2. Realiza una gráfica de la serie de tiempo anterior
#COMENTARIO, la temperatura se mantuvo constante hasta 1950 a partir de alli 
# ha aumentado en promedio 0.7 grados a 2006
plot(Global.ts, xlab = "Tiempo", ylab = "Temperatura en grados Celsius", 
     main = "Temperatura Global",
     sub = "Serie mensual: Ene/1856 a Dic/2006")

# 3. Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia
# COMENTARIO: transformando primera diferencia: diff(Global.ts)
plot(diff(Global.ts), xlab = "", ylab = "")
title(main = "Temperatura Global",
      xlab = "Tiempo", ylab = "Dif Serie",
      sub = "Diferencia de primer órden")

# 4. ¿Consideras que la serie es estacionaria en niveles o en primera diferencia?
# COMENTARIO: Si es estacionaria a partir de la primera diferencia y a partir del año 1900 

# 5. Con base en tu respuesta anterior, obtén las funciones de autocorrelación y autocorrelación parcial?
# COMENTARIO: funcion de autocorrelación  
acf(diff(Global.ts))
#COMENTARIO: función de autocorrelación Parcial
pacf(diff(Global.ts))

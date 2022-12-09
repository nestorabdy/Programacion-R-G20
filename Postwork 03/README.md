# Postwork Sesión 3
## Objetivo
- Realizar un análisis descriptivo de las variables de un dataframe
# Requisitos
1. R, RStudio
2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
3. Curiosidad por investigar nuevos tópicos y funciones de R
# Desarrollo
Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. No olvides excluir los missing values y transformar las variables a su tipo y escala correspondiente.

```R
d <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")
str(d)
d$Categoria <- factor(d$Categoria)
d$Grupo <- factor(d$Grupo)
sum(complete.cases(d))
d <- d[complete.cases(d),]
summary(d)
```
Resultado

```R
> str(d)
'data.frame':	615 obs. of  3 variables:
 $ Categoria : chr  "C1" "C1" "C1" "C1" ...
 $ Grupo     : int  0 1 0 0 0 0 0 0 1 1 ...
 $ Mediciones: num  82.6 112.6 42.8 44.6 21.6 ...
> d$Categoria <- factor(d$Categoria)
> d$Grupo <- factor(d$Grupo)
> sum(complete.cases(d))
[1] 591
> d <- d[complete.cases(d),]
> summary(d)
 Categoria Grupo     Mediciones    
 C1:202    0:444   Min.   :  2.80  
 C2:195    1:147   1st Qu.: 23.45  
 C3:194            Median : 49.30  
                   Mean   : 62.88  
                   3rd Qu.: 82.85  
                   Max.   :290.60
```

1. Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`
```R
mean(d$Mediciones);median(d$Mediciones);Mode(d$Mediciones)
```
Resultado
```R
[1] 62.88494
[1] 49.3
[1] 23.3
attr(,"freq")
[1] 6
```
2. Con base en tu resultado anteior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?


3. Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`
```R
sd(d$Mediciones)
cuartiles.d <- quantile(d$Mediciones, probs = c(0.25, 0.50, 0.75))
cuartiles.d
IQR(d$Mediciones)
```
Resultado
```R
[1] 53.76972
      25%   50%   75% 
    23.45 49.30 82.85 
[1] 59.4
```
4. Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría` ¿Consideras que sólo una categoría está generando el sesgo?
```R
d.table <-table(cut(d$Mediciones, breaks = seq(0,300 , by = 50)),d$Categoria)
d.hist <- as.data.frame(d.table)


ggplot(d.hist, aes(Var1, fill=Var2)) + geom_bar(aes(weight=Freq), position="dodge") + xlab("Categoría") +
  labs(x= "Mediciones",  y="Frecuencias", fill="Categoría") + 
  labs(title = "Histograma por categoria")+
  theme_classic()

```
Resultado

![Histograma de meidicones por categoria](https://github.com/nestorabdy/Programacion-R-G20/blob/main/Postwork%2003/Historgrama_p4.png)

5. Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? ¿Los grupos al interior de cada categoría podrían estar generando el sesgo?
```R
ggplot(d, aes(x=Categoria, y=Mediciones, fill=Grupo))+
  geom_boxplot()+
  labs(title="Boxplot de mediciones por categoría y grupo")+
  theme_classic()
```
Resultado

![Boxplot por categoria y grupo](https://github.com/nestorabdy/Programacion-R-G20/blob/main/Postwork%2003/Boxplot_p4.png)

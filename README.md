# Experimentacion_Estadistica2026

Repositorio del Programa de Maestría Ciencias Forestales 2026

Contenido del curso

**Sesión 1:** 26/01/2026

+ Crear cuenta de Github
+ Crear repositorio del curso
+ Primera sincronización

################################################################################

#SEsion2
# Juan Manuel de la Fuente Solís
# 1752649
# 16/02/2026

#Importar datos
#Funcion read.csv
IE <- read.csv("Base de datos R.csv", header = T) 
IE$Tratamiento <- as.factor(IE$Tratamiento)
# El tratamiento es igual a FERTILIZANTE

#Estadistica descriptiva 

tapply(IE$IE, IE$Tratamiento, mean) #media
tapply(IE$IE, IE$Tratamiento, sd)   #desviación estandar
tapply(IE$IE, IE$Tratamiento, var)  #Varianza

boxplot(IE$IE)
#Este tipo de graficas nos ayuda mucho cuando solo tenemos un valor ya sea en el eje de las x o las y

#como realizar un boxplot para 2 niveles de factor:
boxplot(IE$IE ~ IE$Tratamiento)
#Como cambiarle el color a las graficas:   REVISAR 

boxplot(IE$IE ~ IE$Tratamiento,
        col = "lightblue",
        xlab = "Tratamiento",
        ylab = "IE",
        main = "Vivero FCF",
        ylim = c(0.4, 1.2))


# Pruebas de normalidad ---------------------------------------------------

#existen 2 pruebas
#1.-Shapiro-Wilk 
shapiro.test(IE$IE)

#Prueba de homogeniedad de varianzas 
bartlett.test(IE$IE ~ IE$Tratamiento)

#En conclusión ambos grupos poseen datos normales, ya que ambos estan dentro del rango, es decir su valor de p es mayor a "0.05"
#un histograma se representa de la siguiente manera:
hist(IE$IE,
     col = "red",
     ylim = c(0,12),
     main = "",
     ylab = "Tratamiento",
     xlab = "Variable IE")
#El ancho de una barra perteneciente a un histograma se denomina bean

#####################################################################

#Sesion3
#Correlación-Juan Manuel de la Fuente Solís
#1752649
#23/02/2026
#Correlacion: Se mide la relacion lineal que existe entre 2 variables 
#Esta puede ser de 3 tipos: Positiva, Negativa o que no exista relación
#Para la correlación se necesita tener pares de datos, es decir valores en x y y. 
#La correlacion va de 0 a 1 
#Cuando esta cercana a 0 concluimos que no existe relación lineal entre las variables 
#Entre mas se acerque el valor a 1 existe una mayor relación entre las variables.
# Negativa: A medida que aumenta x el vlor de y va a disminuir.

#Ejemplo:
#Medimos el tiempo de reaccion de 8 estudiantes aplicando una funcion mediante R y les preguntamos su edad.
resp <- data.frame(
  Tiempo = c(12, 15, 17, 18, 20, 21, 22, 26),
  Edad = c(14, 25, 20, 35, 45, 30, 60, 95)
  )
resp

#Crear nuevas columnas con los rangoa (1 a 8)

resp$Rango_Tiempo <- rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad <- rank(resp$Edad, ties.method = "first")

#ver resultado
resp

#Para realizar una grafica de dispeción utilizamos el siguiente comando:
plot(resp$Tiempo, resp$Edad)
plot(resp$Rango_Tiempo, resp$Rango_Edad)

#Ecuación de correlacion de spearman 
cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method = "spearman")

#Correlación Tau de Kendall
#Para calcular este tipo de correlaciones tenemos que tener una clacificacón ordinal, es decir orden ascendente o decendiente 
#Metoo de Kendall

tau <- data.frame(
  A = c(1, 2, 3, 4, 5, 6),
  B = c(3, 1, 4, 2, 6, 5)
)
cor.test(tau$A, tau$B, method = "kendall")

#Nos sirve para determinar puntos de vista y ver que tanto coinciden unas con otras


#Correlacion Punto Biserial
#Ejemplo:

set.seed(123) #Para reproductibilidad
#Numero de observaciones
n <- 20 
#Generar horas de estudio (entre 1 y 10)
Horas_estudio <- sample(1:10, n, replace = TRUE)

#Asignar probabilidad de aprobar en función de horas de estudio
#A más horas, probabilidad más alta

Resultado <-sapply(Horas_estudio, function (horas) {
  ifelse(runif(1) < (horas / 10), "Aprobado", "Reprobado")
})

#Crear data frame 
estudio <- data.frame(
  Estudiante = 1:n,
  Horas_estudio,
  Resultado
)

head(estudio)


#Crear variable dicotómica: 1 = Aprobado, 0 = Reprobado
estudio$Resultado_bin <- ifelse(estudio$Resultado == "Aprobado", 1, 0)
head(estudio)


View(estudio)

################################################################################

#Ejemplo Pearson
x1 <- c(8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8)
y1 <- c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.35, 12.50, 5.56, 7.91, 6.89)

cor.test( x1, y1, method = "pearson")

mean(x1)
mean(y1)
#Tiene una correlación alta y es significativa. 

plot(x1, y1)
#una vez revisada la grafica concluimos que no debemos confiarnos unicamente en a relación que tenga un conjunto de datos, debemos graficarlo y ver su distribución.

########################################################################################

#Regresión lineal 
#Ejercicio 1 
#Los datos de producción de trigo en toneladas (x) y el precio del kilo de harina en pesos (y)en la decada de los 80 en España fueron:
datos <- data.frame(
  Produccion_trigo = c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40),
  Precio_harina = c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
)

plot(datos$Produccion_trigo, datos$Precio_harina)
#Sumatoria de xi*yi
sum_xi_yi <- sum(datos$Produccion_trigo*datos$Precio_harina)
n <- length(datos$Produccion_trigo)
mean_x <- mean(datos$Produccion_trigo)
mean_y <-mean(datos$Precio_harina)

#La sumatoria siguiente no se realizo en clase
#Ejemplo:
#Sum de xi al cuadrado
sumxi2 <- sum(datos$Produccion_trigo^2)
b1 <- (sum_xi_yi - n * mean_x*mean_y)/ (sumxi2-n*mean_x^2)
b1


fit.lm <- lm(datos$Precio_harina ~ datos$Produccion_trigo)
summary(fit.lm)

anova(fit.lm)

#Residuales en el modelo lineal fit.lm
fit.lm$residuals
#Valores ajustados (y prima) en el modelo lineal
fit.lm$fitted.values

#Grafica de y-prima vs residuales
plot(fit.lm$fitted.values, fit.lm$residuals)

library(lmtest)
#Datos
x <- c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40)

#Precio de la harina (y)
y <- c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)

# Modelo
m <- lm(y ~ x)

#Prueba de Breusch-Pagan (Varianza ~ x)
bptest(fit.lm)

#Lo que importa es el PValue(0.5641) me dice que cumplo o no con el criterio

#Agregar una columna con la recta usando Bo y B1
datos$recta <- 74.116 - 1.3537*datos$Produccion
#La formula es y (prima) = alfa + beta*primer valor de xi
datos$residuales <- datos$Precio - datos$recta
datos

#Estimar la varianza de los residuales 
sum(datos$residuales^2)

#Estimar la varianza de los residuales 
sum(datos$residuales^2)/8

plot(datos$Produccion_trigo, datos$Precio_harina,
     col= "indianred",
     xlab = "Produccion trigo (Ton / ha)",
     ylab = "Precio harina (Euros)",
     pch = 19,
     xec = 1.2)
abline(fit.lm, col = "blue")

################################################################################

#Comparación de medias o pruebas de T
#Sirven para comparar grupos experimentales 
#Las pruebas de T son de 3 tipos dependiendo del diseño experimental
#Pruebas de T de una muestra: Existe una media establecida que utilizamos como referencia.
#Pruebas de T de muestras independientes: 2 grupos de individuos con una variable en comun que vamos a comparar.
#Pruebas de T de muestras dependientes:Tenemos 1 grupo de individuos y tenemos que compararlo antes y despues de...
#Es el mismo grupo de individuos que se miden antes y despues de que ocurra algun evento. 
#Las pruebas de T se basan en una comparación entre las dos medias y nos dicen si existe o no existe una diferencia significativa entre ambas. 
#Tienen que tener 3 caracteristicas para poder aplicarlas: 
#que sean normales
#Que tengan homogeneidad 
#Que la base de datos sea mayor a n = 30. 

IE <- read.csv("Base de datos R.csv", header = T)
IE$Tratamiento <- as.factor (IE$Tratamiento)

tapply(IE$IE, IE$Tratamiento, mean)
tapply(IE$IE, IE$Tratamiento, var)

#Grafica

boxplot(IE$IE ~ IE$Tratamiento)
#Como cambiarle el color a las graficas:   REVISAR 

boxplot(IE$IE ~ IE$Tratamiento,
        col = "lightblue",
        xlab = "Tratamiento",
        ylab = "IE",
        main = "Vivero FCF",
        ylim = c(0.4, 1.2))


# Pruebas de normalidad ---------------------------------------------------

#existen 2 pruebas
#1.-Shapiro-Wilk 
shapiro.test(IE$IE)

#Prueba de homogeniedad de varianzas 
bartlett.test(IE$IE ~ IE$Tratamiento)

#En conclusión ambos grupos poseen datos normales, ya que ambos estan dentro del rango, es decir su valor de p es mayor a "0.05"
#un histograma se representa de la siguiente manera:

#utilizaremos la pruena de T
t.test(IE$IE ~ IE$Tratamiento, var.equal =T)

#La prueba de t de dos colas (Se divide la probabilidad de 0.5 en dos, una de cada lado con un valor de 0.025)
#En la greater partimos de que ya sabemos que la media de un grupo es mayor que la media del otro. 
t.test(IE$IE ~ IE$Tratamiento, var.equal =T,
      
       alternative = "greater")

#Utilizamos less cuando sabemos que la media de un grupo es menor a la media del otro
t.test(IE$IE ~ IE$Tratamiento, var.equal =T,
       alternative = "less")

#Prueba de T de una muestra utilizando el indice de esbeltez como media conocida
#Utilizaremos unicamente las plantas con un indice de esbeltez mayor a 0.80
mean(IE$IE)
#Se utiliza mu para colocar la media teoretica o la media conocida o ya establecida. 
t.test(mu = 0.85, IE$IE)

#Prueba de T de muestras dependientes: 
#En esta base de datos tomaremos el fertilizante y el tiempo comparando 2 grupos de plantas y viendo ls diferencias a traves del tiempo antes del fertilizante y despues del fertilizante. 
Ctrl <- subset(IE$IE, IE$Tratamiento == "Ctrl")
Trat <- subset(IE$IE, IE$Tratamiento != "Ctrl")           
(t.test(Ctrl, Trat, paired = T)

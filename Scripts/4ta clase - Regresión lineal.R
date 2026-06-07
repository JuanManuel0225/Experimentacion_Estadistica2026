# Regresión lineal
# La regresión lineal es una técnica estadística que permite modelar y cuantificar
# la relación entre una variable independiente (X) y una variable dependiente (Y), 
# mediante una ecuación lineal que describe la tendencia de los datos. Además, 
# permite evaluar la dirección e intensidad de dicha relación y realizar 
# predicciones de la variable respuesta.

# Ejercicio 1 
# Los datos de producción de trigo en toneladas (x) y el precio del kilo de harina en pesos (y)en la decada de los 80 en España fueron:
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
fit.lm
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
datos$recta <- 74.116 - 1.3537*datos$Produccion_trigo
#La formula es y (prima) = alfa + beta*primer valor de xi
datos$residuales <- datos$Precio_harina - datos$recta
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
     cex = 1.2)
abline(fit.lm, col = "blue")


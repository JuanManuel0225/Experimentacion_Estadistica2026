# 4ta clase de experimentación y métodos estadsticos

# Juan Manuel de la Fuente Solís
# 1752649
# 02/03/2026


x1 <- c(10.0, 8.0, 13.0, 9.0, 11.0, 14.0, 6.0, 4.0, 12.0, 7.0, 5.0)
y1 <- c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68)

cor.test(x1, y1, method = "pearson")

mean(x1)
mean(y1)

plot(x1, y1)
abline(lm(y1 ~ x1))

# x variable independiente, y depende de la x


#Ejercicio 1 Modelo de regresión lineal

datos <- data.frame(
  Produccion_trigo = c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40),
  Precio_harina = c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
)

#Ajuste del modelo de regresión lineal
fit.lm <- lm(Precio_harina ~ Produccion_trigo, data =datos)

plot(datos$Produccion_trigo, datos$Precio_harina,
     col = "blue",
     xlab = "Produccion trigo (Ton /ha)",
     ylab = "Precio harina (Euros)",
     pch = 19,
     cex = 1.2)
abline(fit.lm, col = "red")


#Sumatoria de xi*yi
sum_xi_yi <- sum(datos$Produccion_trigo * datos$Precio_harina)
sum_xi_yi

#Tamaño de la muestra
n <- length(datos$Produccion_trigo)
n

# Modelo
m <- lm(y1 ~ x1)
m
summary(fit.lm)

#Prueba Breush-Pagan(Varianza ~ x)
library("lmtest")
bptest(fit.lm)





x1 <- c(8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8)
y1 <- c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.35, 12.50, 5.56, 7.91, 6.89)

cor.test( x1, y1, method = "pearson")

mean(x1)
mean(y1)
#Tiene una correlación alta y es significativa. 

plot(x1, y1)
#una vez revisada la grafica concluimos que no debemos confiarnos unicamente en a relación que tenga un conjunto de datos, debemos graficarlo y ver su distribución.
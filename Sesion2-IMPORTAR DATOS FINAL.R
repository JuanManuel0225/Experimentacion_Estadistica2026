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


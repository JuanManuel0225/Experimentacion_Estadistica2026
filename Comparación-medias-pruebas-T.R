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



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




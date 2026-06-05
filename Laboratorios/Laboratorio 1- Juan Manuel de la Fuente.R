# Laboratorio 1 (Primer contacto con la consola de R)
# Ejemplo: Considere las facturas menusales de una estudiante universitaria de la
# MCF: telefono celular $300, transporte $240, comestibles $1527, gym $400,
# alquiler $1500, otros $1833. 
# 1.- Encontrar gastos totales: 
300 + 240 + 1527 + 400 + 1500 + 1833

#Para crear variables que almacenen uno o más valores se utiliza <-, ejemplo:

celular <- 300
transporte <- 240
comestibles <- 1527
gym <- 400
alquiler <- 1500
otros <- 1833

celular
transporte
comestibles
gym
alquiler
otros

# 2.- Ahora que tenemos todas las variables, crearemos on ubjeto total con la suma
# los gastos.
gastos <- 300 + 240 + 1527 + 400 + 1500 + 1833
gastos

# 3.- Suponiendo que la estudiante tiene los mismos gastos todos los meses,
# ¿Cuánto gastaría durante un semestre escolar? Suponiendo que el semestre
# implica 5 meses. 
(gastos * 5)

# 4.- Suponiendo la misma suposición sobre los gastos mensuales, ¿Cuánto gastaria
# la estudiante durante un año escolar? (suponiendo que el año tiene 10 meses)
(gastos * 10)

# Autoevaluación
# Toma los onjetos creados de la estudiante (i.e variables) transporte,
# comestibles, gym, alquiler, otros y escribelos dentro de la función de combinación
# c() para crear un vector llamado gastos:

gastos <- c(celular, transporte, comestibles, gym, alquiler, otros)

# Ahora utilizaremos la función gráfica barplot() para producir una grafica de barras
# de gastos:

barplot(gastos)

# Descubra como utilizar sort() para ordenar los elementos en la variable gastos,
#con el fin de organizar los elementos de gastos en orden decreciente. 
sort(gastos)

# Descubra como utilizar sort() y barplot() para producir un gráfico de barras
# con barras en orden decreciente:

gastos <- c(
  Celular = celular,
  Transporte = transporte,
  Comestibles = comestibles,
  Gym = gym,
  Alquiler = alquiler,
  Otros= otros
  )
barplot(
  sort(gastos, decreasing = TRUE),
  col = rainbow(length(gastos)),
  main = "Gastos mensuales",
  ylab = "Monto"
)
# Para que quepan los nombres completos dentro de la gráfica se utilizo una función
# que determinara el margen del gráfico:
# 10 -> margen inferior(abajo)
# 4 -> margen izquiero
# 4 -> margen derecho(arriba)
# 2-> margenderecho
# Esto es fundamental a la hora de ajustar los gráficos en R quedando de la siguiente manera:

par(mar = c(10,4,4,2))

barplot(
  sort(gastos, decreasing = TRUE),
  col = rainbow(length(gastos)),
  las = 2,
  cex.names = 0.8,
  main = "Gastos mensuales"
)



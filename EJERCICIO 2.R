medidas <- function(x){
  print(summary(x))
  cat("Varianza:", var(x), "\n")
  cat("Desv. estándar:", sd(x), "\n")
}

datos <- c(5, 8, 12, 15, 18, 21, 25, 30)

medidas(datos)
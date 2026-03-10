
datos <- c(
  33.57,27.78,40.81,
  38.34,29.01,47.78,
  26.86,54.33,28.99,
  25.21,30.49,27.38,
  36.42,41.50,29.39,
  24.54,41.75,44.68,
  24.49,33.23,47.09,
  29.07,28.21,42.10,
  26.54,27.74,33.48,
  31.44,30.08
)


breaks <- seq(24,56,4)

clases <- cut(datos, breaks=breaks, right=FALSE)

# Frecuencias
freq <- table(clases)

f <- as.numeric(freq)
rel <- f/length(datos)
cum <- cumsum(f)
cumrel <- cumsum(rel)

# Tabla
tabla <- data.frame(
  Clase = names(freq),
  Frec = f,
  Frec_relativa = round(rel,3),
  Frec_acumulada = cum,
  Frec_rel_acumulada = round(cumrel,3)
)

print(tabla,row.names=FALSE)

# Histograma
hist(datos,
     breaks=breaks,
     col="lightblue",
     main="Histograma",
     xlab="Valores")

# Polígono de frecuencias
h <- hist(datos, breaks=breaks, plot=FALSE)

x <- c(breaks[1], h$mids, tail(breaks,1))
y <- c(0, h$counts, 0)

plot(x, y,
     type="l",
     xlab="Valores",
     ylab="Frecuencia",
     main="Poligono de frecuencias",
     lwd=2)


# b
cat("\n(b) Porcentaje < 30:",
    round(mean(datos < 30)*100,2),"%\n")

# c
cat("(c) Porcentaje entre 40 y 49.99:",
    round(mean(datos >=40 & datos <=49.99)*100,2),"%\n")

# d
cat("(d) Porcentaje > 34.99:",
    round(mean(datos >34.99)*100,2),"%\n")

# e
cat("(e) La distribución es ligeramente sesgada a la derecha,",
    "porque hay varios valores altos (40–54) que extienden la cola derecha.\n")

# f
cat("(f) Mediciones < 40:",
    sum(datos <40),"\n")


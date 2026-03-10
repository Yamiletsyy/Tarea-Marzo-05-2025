
datos <- c(
  1.17,2.50,2.00,2.33,1.67,1.33,
  1.17,2.17,2.17,1.33,2.17,2.00,
  2.17,1.17,2.50,2.00,1.50,1.50,
  1.00,2.17,2.17,1.67,2.00,2.00,
  1.33,2.17,2.83,1.50,2.50,2.33,
  0.33,2.17,1.83,2.00,2.17,2.00,
  1.00,2.17,2.17,1.33,2.17,2.50,
  0.83,1.17,2.17,2.50,2.00,2.50,
  0.50,1.50,2.00,2.00,2.00,2.00,
  1.17,1.33,1.67,2.17,1.50,2.00,
  1.67,0.33,1.50,2.17,2.33,2.33,
  1.17,0.00,1.50,2.33,1.83,2.67,
  0.83,1.17,1.50,2.17,2.67,1.50,
  2.00,2.17,1.33,2.00,2.33,2.00,
  2.17,2.17,2.00,2.17,2.00,2.17
)

breaks <- c(0,0.5,1.0,1.5,2.0,2.5,3.0)

clases <- cut(datos, breaks=breaks, right=FALSE)

freq <- table(clases)

f <- as.numeric(freq)
rel <- f/length(datos)
cum <- cumsum(f)
cumrel <- cumsum(rel)

tabla <- data.frame(
  Clase = names(freq),
  Frec = f,
  Frec_relativa = round(rel,3),
  Frec_acumulada = cum,
  Frec_rel_acumulada = round(cumrel,3)
)

print(tabla, row.names=FALSE)

# Histograma
hist(datos,
     breaks=intervalos,
     col="lightblue",
     main="Histograma",
     xlab="Plaque Index")

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
cat("\n(b) Porcentaje < 2.00:",
    round(mean(datos < 2.00)*100,2),"%\n")

# c
cat("(c) Proporción >= 1.50:",
    round(mean(datos >= 1.50),3),"\n")

# d
cat("(d) Porcentaje entre 1.50 y 1.99:",
    round(mean(datos >=1.50 & datos <=1.99)*100,2),"%\n")

# e
cat("(e) Mediciones > 2.49:",
    sum(datos > 2.49),"\n")

# f
cat("(f) Proporción <1.0 o >2.49:",
    round(mean(datos <1.0 | datos >2.49),3),"\n")

# g
cat("(g) Mejor estimación de un valor al azar (media):",
    round(mean(datos),2),"\n")

# h
cat("(h) La distribución es ligeramente sesgada a la izquierda.\n")
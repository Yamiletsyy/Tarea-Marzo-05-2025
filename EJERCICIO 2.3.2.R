
datos <- c(
  8.1,10.0,5.0,7.0,10.0,3.0,
  20.0,4.0,4.0,6.0,6.0,7.0,
  10.0,4.0,3.0,5.0,6.0,6.0,
  6.0,6.0,6.0,5.0,4.0,5.0,
  6.0,25.0,10.0,14.0,6.0,6.0,
  4.0,15.0,5.0,5.0,8.0,19.0,
  21.0,8.3,7.0,8.0,5.0,8.0,
  5.0,7.5,7.0,10.0,15.0,8.0,
  10.0,3.0,15.0,6.0,10.0,8.0,
  7.0,5.0,10.0,3.0,7.0,3.3,
  15.0,5.0,5.0,3.0,7.0,8.0,
  3.0,6.0,6.0,10.0,15.0,6.0,
  3.0,3.0,7.0,5.0,4.0,9.2,
  16.0,7.0,8.0,5.0,10.0,10.0,
  9.0,5.0,5.0,4.0,8.0,4.0,
  3.0,4.0,5.0,8.0,30.0,14.0,
  15.0,2.0,8.0,7.0,12.0,4.0,
  3.8,10.0,25.0,8.0,9.0,14.0,
  30.0,2.0,10.0,5.0,5.0,10.0,
  22.0,5.0,5.0,3.0,4.0,8.0,
  7.5,5.0,8.0,3.0,5.0,7.0,
  8.0,5.0,9.0,11.0,2.0,10.0,
  6.0,5.0,5.0,12.0,9.0,8.0,
  15.0,18.0,10.0,9.0,5.0,6.0,
  6.0,8.0,12.0,10.0,5.0,
  5.0,16.0,8.0,5.0,8.0,
  4.0,16.0,3.0,7.0,13.0
)

breaks <- seq(0,30,by=5)

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
cat("\n(b) Porcentaje entre 10 y 14.9:",
    round(mean(datos >=10 & datos <=14.9)*100,2),"%\n")

# c
cat("(c) Observaciones menores que 20:",
    sum(datos <20),"\n")

# d
cat("(d) Proporción >= 25:",
    round(mean(datos >=25),3),"\n")

# e
cat("(e) Porcentaje <10 o >19.95:",
    round(mean(datos <10 | datos >19.95)*100,2),"%\n")

# f
cat("(f) La distribución está sesgada a la derecha porque la mayoría de los valores se concentran en valores pequeños (aprox. 3–10) y hay algunos valores grandes como 20, 22, 25 y 30 que extienden la cola hacia la derecha.\n")
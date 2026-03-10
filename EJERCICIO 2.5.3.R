datos <- c(
  40.0,47.0,34.0,42.0,54.0,48.0,53.6,56.9,58.0,45.0,
  54.5,54.0,43.0,44.3,53.9,41.8,33.0,43.1,52.4,37.9,
  34.5,40.1,33.0,59.9,62.6,54.1,45.7,40.6,56.6,59.0
)

cat("(a) Media:", round(mean(datos),2),"\n")

cat("(b) Mediana:", median(datos),"\n")

moda <- as.numeric(names(sort(table(datos), decreasing=TRUE)[1]))
cat("(c) Moda:", moda,"\n")

cat("(d) Rango:", max(datos)-min(datos),"\n")

cat("(e) Varianza:", round(var(datos),2),"\n")

cat("(f) Desviación estándar:", round(sd(datos),2),"\n")

cat("(g) Coeficiente de variación:",
    round((sd(datos)/mean(datos))*100,2),"%\n")

cat("(h) Rango intercuartílico:", IQR(datos),"\n")

# boxplot
boxplot(datos,
        horizontal=TRUE,
        col="pink",
        main="Boxplot PaCO2",
        xlab="PaCO2 (mmHg)")

cat("\nMedida de tendencia central recomendada: la MEDIA.\n")
cat("Razón: los datos están relativamente distribuidos alrededor del centro sin valores extremadamente alejados que distorsionen mucho la media.\n")
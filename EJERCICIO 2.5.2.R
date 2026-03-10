datos <- c(500,570,560,570,450,560,570)

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
        col="purple",
        main="Boxplot Heart Rate (Rat Pups)",
        xlab="Heart Rate (bpm)")

cat("\nMedida de tendencia central recomendada: la MEDIA.\n")
cat("Razón: los datos son bastante similares entre sí, con la mayoría de valores entre 560 y 570 bpm, por lo que la media describe bien el centro de la distribución.\n")
datos <- c(230,205,313,207,227,245,173,58,103,181,105,301,169)

cat("(a) Media:", round(mean(datos),2),"\n")

cat("(b) Mediana:", median(datos),"\n")

moda <- as.numeric(names(sort(table(datos), decreasing=TRUE)[1]))
cat("(c) Moda:", moda,"\n")

cat("(d) Rango:", max(datos)-min(datos),"\n")

cat("(e) Varianza:", round(var(datos),2),"\n")

cat("(f) Desviación estándar:", round(sd(datos),2),"\n")

cat("(g) Coeficiente de variación:",
    round((sd(datos)/mean(datos))*100,2),"%\n")

cat("(h) Rango intercuartílico (IQR):", IQR(datos),"\n")

boxplot(datos,
        horizontal=TRUE,
        col="lightblue",
        main="Boxplot CD4 T Cell Counts",
        xlab="CD4 T Cell Count")

cat("\nMedida de tendencia central recomendada: la MEDIANA.\n")
cat("Razón: existen valores muy bajos (por ejemplo 58 y 103) que pueden afectar la media. La mediana es más robusta frente a valores extremos.\n")
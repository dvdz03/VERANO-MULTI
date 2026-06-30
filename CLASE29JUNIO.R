#clase 29jun
# Leer los datos
pgfull

# Tomar solo las columnas de especies
esp <- pgfull[, 1:54]

# Transformar un poco los datos porque hay muchos ceros
esp <- log(esp + 1)

# Hacer el PCA
pca <- prcomp(esp, scale. = TRUE)

# Ver cuánta variación explica cada componente
summary(pca)

# Guardar los componentes principales
pc1 <- pca$x[,1]
pc2 <- pca$x[,2]

# Relacionar los componentes con variables ambientales
cor(pc1, pgfull$hay)
cor(pc1, pgfull$pH)
cor(pc1, pgfull$species)

cor(pc2, pgfull$hay)
cor(pc2, pgfull$pH)
cor(pc2, pgfull$species)

# Gráfica simple del PCA
plot(pc1, pc2,
     xlab = "PC1",
     ylab = "PC2",
     main = "PCA de especies",
     pch = 19)

# Ver PC1 contra biomasa
plot(pc1, pgfull$hay,
     xlab = "PC1",
     ylab = "Biomasa",
     main = "PC1 y biomasa",
     pch = 19)

# Ver PC2 contra pH
plot(pc2, pgfull$pH,
     xlab = "PC2",
     ylab = "pH",
     main = "PC2 y pH",
     pch = 19)

# Ver PC2 contra riqueza de especies
plot(pc2, pgfull$species,
     xlab = "PC2",
     ylab = "Riqueza de especies",
     main = "PC2 y riqueza",
     pch = 19)


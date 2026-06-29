#EJERCICIO 29JUN
pgfull<-read.table("C:/Users/100032608/Downloads/pgfull.txt", header=TRUE)
pgfull
head(pgfull)
#análisis de factores, cuáles son los componentes principales, que factores ambientales  están asociados 
library(GGally)
ggpairs(pgfull)

pgfull2<-pgfull[,1:4]
iris_2
view(iris_2)
especies<-iris$Species

ggpairs(iris)#parece que las variables si tienen relación tal vez no perfecta pero si una relación, hay corrrelaciones significativas 
#pero lo ideal es que todas lo tengan alv


res.pca3 <- prcomp(iris[, -5],  scale = TRUE)

fviz_eig(res.pca3)
get_eig(res.pca3)

fviz_pca_ind(res.pca3, label="none", habillage=iris$Species,
             addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2")


fviz_pca_var(res.pca3,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca3, lrepel=T, habillage=iris$Species, 
                palette="Dark2",
                col.var = "#080878") # Variables color


# Eigenvalues
eig.val <- get_eigenvalue(res.pca3)
eig.val

# Resultado para variables
res.var <- get_pca_var(res.pca3)
res.var$coord          # Coordenadas
res.var$contrib        # Contribución  PCs
res.var$cos2           # Calidad de la representación 

# Resultados para individuos
res.ind <- get_pca_ind(res.pca3)
res.ind$coord          # Coordenadas
res.ind$contrib        # Contribución  PCs
res.ind$cos2           # Calidad de la representación


#####################
otroscp<-princomp(iris_2)


PC1<-otroscp$scores[,1]
PC2<-otroscp$scores[,2]

plot(PC1,PC2,pch=16, col=rainbow(3)[especies])
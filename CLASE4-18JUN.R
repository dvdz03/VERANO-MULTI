niveles<-c(rep("A",3), rep("B",3),rep("C",3),rep("D",3))
niveles
orden<-sample(niveles,12)
orden
barberia<-c(140,108,76,400)
sanguich<-c(2,21,0,42)
noticia<-c(40,5,5,0)
mean(barberia)
mean(sanguich)
mean(noticia)
mean(c(barberia,sanguich,noticia))

promediototal
mean(promediototal)
sumacuadradotrata<-sum((16.25-69.91)^2,(12.5-69.91)^2,(181-69.91)^2)
sumacuadradotrata
sumacuadradotrata*4
(12.5-69.91)^2
(181-69.91)^2

sumatota<-sum(((140-69.91)^2),((108-69.91)^2),((76-69.91)^2),((400-69.91)^2),(2-69.91)^2,(21-69.91)^2,(0-69.91)^2,
  (42-69.91)^2,(40-69.91)^2,(5-69.91)^2,(5-69.91)^2,(0-69.91)^2)
sumatota
cosa<-sumatota-73958.03
sumacuadradostratamientos1<-73958.03
#cuadrado medio de tratamientos
sumacuadradostratamientos1/2
cosa/9
cosa
#ahora necesitamos hacer la prueba de F con 2 grados de libertad en el numerador y 9 grados de libertad en el denominador
36979.01/7586.765
#la probabilidad de la curva f en 2 grados en el numerador y 9 en el denominador 
#buscar la probabilida de obtener un valor tan grande como el que obtuvimos suponiendo que la hipótesis nula es verdadera
pf(4.87,2,9,lower.tail=F)
pf(9,2,9, lower.tail=F)


#ejemplo
set.seed(45678)
sustrato<-c(rep("Arroz",8), rep("Alpiste",8), rep("Sorgo",8 ))
num_arroz<-ceiling(rnorm(8,443, 100))
num_arroz
num_alpiste<-ceiling(rnorm(8,231,100))
num_alpiste
num_sorgo<-ceiling(rnorm(8,172,100))
num_sorgo
rendimiento<-c(num_arroz,num_alpiste,num_sorgo)
tapply(rendimiento,sustrato, summary)
boxplot(rendimiento ~ sustrato)
respuesta<-sqrt(rendimiento)#la transformación más común es la raíz cuadrada, sería adecuado en el caso de tener porcentajes o unan variable cuantitativa discreta
modelo<-aov(respuesta~sustrato)#del lado izquierdo la respuesta y del derecho la predictora o predictoras
summary(modelo)#residuales==errores
#para ver si los residuales se distribuyen de manera noraml 
#primero se tiene que probar la normalidad de los residuales
shapiro.test(modelo$residuals)
#el valor de p aquí es más grande que 0.05 o sea que sin normales
#ahora si la prueba de barlett
bartlett.test(respuesta~sustrato)#esta prueba depende de la normalidad


#ilustrar las pruebas con gráficos de residuales
#valores ajustados en anova son los promedios de los tratamientos y los residuales los calculamos entre la diferencia entre la observación y el valor de la media del tratamiento
#lo que queremos observar en estos gráficos es que los residuales están dispersos por todos lados y no hay ningún patrón. 
#hay patrones que no están bien y que son así bien asuichis como que estén en forma de cono
#también un cono al reves, un rombo, o una curva
#todo esto es evidencia de que las varianzas no son iguales, o sea heterocedasticidad
#la curva es evidencia de no lineal, las heterocidástica son los conos y el rombo
#se grafica el modelo para ver todo esto
plot(modelo,which=1)
plot(modelo,which=2)#si los puntos están cerca de la línea es normal


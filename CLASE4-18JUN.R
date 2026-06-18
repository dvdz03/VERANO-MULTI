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

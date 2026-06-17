#Clase 17JUN
rnorm(2)
#simulación de eso
agua<-rnorm(100000, mean=200, sd=50)
hist(agua, freq = F)
aguaD<-density(agua)
lines(aguaD, col="purple", lwd=3)
abline(v=250, col="orange", lwd=3)
abline(v=150, col="blue", lwd=3)

plot(aguaD, main = "Consumo de agua", xlab = "metros cúbicos", ylab = "densidad")
abline(v=qnorm(0.025, 200, 50))
abline(v=qnorm(0.975, 200, 50))
#no siempre las muestras van a ser así de grandes entonces no vamos a obtener siempre así histogramas bonitos y asuich

#cual será la probabilidad del área antes de la línea, saber que porcentaje son
pnorm(150, mean=200, sd=50)
#ahí nos dice que el 15.86% de los consumidores usan menos de 150 m3 de agua
#ahora para ver lo del otro lado de la cola, es usando lower.tail=F
pnorm(250,mean=200, sd=50, lower.tail=F)
#es exactamente igual, casi el 16% usan más de 250 m3 de agua
#esto se puede para cualquier distribución de probabilidad

z<-1.44
#encontrar la probabilidad de que z sea más grande que ?
v<-pnorm(1.44,lower.tail=F)
v
p<-2*v
p
#entonces la probabilidad de obtener una z tan grande como la que obtuvimos es casi del 15%, concluimos que es plausible que la hipótesis nula sea verdadera, no hay evidencia para rechazarla
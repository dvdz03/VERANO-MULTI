#Clase 17JUN
rnorm(2)
#simulación de eso
agua<-rnorm(100000, mean=200, sd=50)
hist(agua, freq = F)
aguaD<-density(agua)
lines(aguaD, col="purple", lwd=3)
abline(v=250, col="blue", lwd=3)
abline(v=150, col="blue", lwd=3)

plot(aguaD, main = "Consumo de agua", xlab = "metros cúbicos", ylab = "densidad")
abline(v=qnorm(0.025, 200, 50))
abline(v=qnorm(0.975, 200, 50))
#no siempre las muestras van a ser así de grandes entonces no vamos a obtener siempre así histogramas bonitos y asuich

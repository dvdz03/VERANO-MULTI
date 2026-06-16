#clase 2 16JUN
zeta<- qnorm(0.025)
zeta
tcero25<- qt(0.025,24)
xi2cero25<-qchisq(0.025,24)
tcero25
xi2cero25

#cargar los datos
datos1<- read.table("C:/Users/100032608/Downloads/t5_p1.txt", header=TRUE)
datos1
#PENDIENTES LOS DE ABAJO estos todavía no se hacen alv y no se ocupan
datos2<- read.table("C:/Users/100032608/Downloads/t5_p2.txt", header=TRUE)
datos2
datos3<- read.table("C:/Users/100032608/Downloads/t5_p3.txt", header=TRUE)
datos3
datos5<- read.table("C:/Users/100032608/Downloads/t5_p5.txt", header=T)
datos5

#RESUMEN NÚMERICO: MINIMO MÁXIMO QUARTILES
#MEDIANA Y MEDIA
#RANGO DESVIACIÓN VARIANZA

### MEDIDS DE POSICIÓN
summary(datos1$edad)
#rango amplio, media y mediana como cerca entre si pero no tanto
rango <-max(datos1$edad)-min(datos$edad)
#la serie de datos incluye valores muy pequeños y grandes el valor del rango es quien sabe
#Medidas tendencia central y dispersión
library(Rmisc) 
# se usa este porque tiene un 
summarySE(data=datos1, measurevar= "edad")
# lo que nos da es la media, desviación estandar, el error, y el intervalo bueno el valor de e.
# esto para sacar el intervalo de confianza para la media 

#ya todo esto es el resumen numérico pero falta el resumen gráfico
#con un boxplot
boxplot (datos1$edad)
#la distribución tiene un sesgo positivo
hist(datos1$edad)
#es una distribución sesgada que no parece ser normal
#prueba de shapiro
shapiro.test(datos1$edad)
#el valor w se obtiene en una tabla de shapiro, se tiene que el valor de la probabilidad para esa distribucion es el p-value. 
#para entender el valor de p hay que plantear las hipótesis nula y alterna
#NULA: datos se dsitribuyen de manera normal
#ALTERNA: no se distribuyen de manera normal
#en las pruebas de hipótesis se prueba siempre la nula, el valor de 0.88
#el valor de p nos dice cual es la probabiliad de obtener un valor como el de 0.88 suponiendo que la hipóteis nula se cumple.
#entonces como el valor de p es menor al 0.05 entonces significa o poedmos decir que la distribución es significativamente diferente a la distribución normal. 

#Esto se tiene que hacer así de asuichis con todas las bases de datos, gráficos y numéricos. poner el intervalo de confianza, cuanto vale el mínimo y el máximo
#para que quede el análisis exploratorio de los datos
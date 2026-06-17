#script limpio
datos1<- read.table("C:/Users/100032608/Downloads/t5_p1.txt", header=TRUE)
datos1
datos2<- read.table("C:/Users/100032608/Downloads/t5_p2.txt", header=TRUE)
datos2
datos3<- read.table("C:/Users/100032608/Downloads/t5_p3.txt", header=TRUE)
datos3
datos5<- read.table("C:/Users/100032608/Downloads/t5_p5.txt", header=T)
datos5

#Resumen datos 2
summary(datos2$Edad)
library(Rmisc)
summarySE(data=datos2, measurevar= "Edad")
min(datos2$Edad)
max(datos2$Edad)
boxplot(datos2$Edad)
hist(datos2$Edad)
shapiro.test(datos2$Edad)

#Resumen datos 3
summary(datos3$TR)
summarySE(data=datos3, measurevar="TR")
min(datos3$TR)
max(datos3$TR)
boxplot(datos3$TR)
hist(datos3$TR)
shapiro.test(datos3$TR)

#Resumen datos 5
summary(datos5$Cancer)
summarySE(data=datos5, measurevar="Cancer")
min(datos5$Cancer)
max(datos5$Cancer)
boxplot(datos5$Cancer)
hist(datos5$Cancer)
shapiro.test(datos5$Cancer)

tinytex::install_tinytex()

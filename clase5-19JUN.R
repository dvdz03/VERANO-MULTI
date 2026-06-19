#CLASE 19JUN
read.table("C:/Users/100032608/Downloads/ejercicio19JUN - Hoja 1.csv",header=TRUE)->HONGOS
HONGOS##NO ASÍ NO
ejercicio19JUN...Hoja.1
honguis<-read.table("C:/Users/100032608/Downloads/tratamiento-altura.txt",header=T)
honguis
head(honguis)
summary(honguis)
library(Rmisc)
summarySE(honguis,measurevar="altura",groupvars="tratamiento")
boxplot(honguis$altura~honguis$tratamiento)
hist(honguis$altura)
shapiro.test(honguis$altura)

anova1<-aov(altura~tratamiento, data=honguis)
anova1
summary(anova1)
bartlett.test(honguis$altura~honguis$tratamiento)#residuales distribución normal mijito

#prueba post hoc
mijo<-TukeyHSD(anova1)

library(multcompView)
cld1 <- multcompLetters4(anova1, mijo)#sacar los grupos 
cld1
library(dplyr)
dt1 <- group_by(honguis, tratamiento) %>%
  summarise(w=mean(altura), sd = sd(altura)) %>%
  arrange(desc(w))
dt1
print(dt1)

library(ggplot2)
ggplot(dt1, aes(tratamiento, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "tratamientos", y = "Altura promedio") +
  geom_text(aes(label = cld1, y = w + sd), vjust = -0.5) +
  ylim(0,410) +
  theme_few()
install.packages("ggthemes")
library(ggthemes)


boxplot(altura ~ tratamiento, data = honguis,las=2,
        col = c("blue","orange","purple"),
        ylab = "altura",
        xlab = "Tratamientuchis",
        main = "mijitin")

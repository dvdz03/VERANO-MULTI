#EJERCICIO 29JUN
pgfull<-read.table("C:/Users/100032608/Downloads/pgfull.txt", header=TRUE)
pgfull
head(pgfull)
str(pgfull)

install.packages("EFAtools")
library(EFAtools)
library(tidyverse)
install.packages("multiUS")
library(multiUS)

##################
# FASE 2: DISEÑO #
##################


# Subconjunto de los datos

pg_especies<-pgfull[,1:54]#para las especies, las demás son las explicativas creo

DOSPERT_sub <- DOSPERT_raw[1:500,]
view(DOSPERT_sub)

#####################
# FASE 3: SUPUESTOS # 
#####################
BARTLETT(pg_especies)#correlación entre sí.
KMO(pg_especies)#la adecuación de la muestra, arriba de 0.6 o 0.7 es bueno
ani<-antiImage(pg_especies)
print(ani,digits=3)

BARTLETT(gem)
KMO(gem)
antiImage(gem)

BARTLETT(DOSPERT_sub)
KMO(DOSPERT_sub)
ai<-antiImage(DOSPERT_sub)
print(ai, digits = 3)

#################################
# FASE 4: OBTENCIÓN DE FACTORES #
#################################

# Se empleará el método de análisis de factore comunes

# Número de factores
library(future)
install.packages("parallelly",type="binary",dependencies = T)
library(parallelly)
library(EFAtools)
PARALLEL(pg_especies,eigen_type="SMC")
N_FACTORS(pg_especies,criteria=c("PARALLEL","EKC","SMT"),
                       eigen_type_other = c("SMC","PCA"))
N_FACTORS(pg_especies, method = "ULS")#muestra que los datos no son apropiados para el análisis de factores porque el valor de KMO no es alto
#7 factores?

PARALLEL(gem,eigen_type = "SMC")
N_FACTORS(gem,criteria=c("PARALLEL","EKC","SMT"),
          eigen_type_other = c("SMC","PCA"))
N_FACTORS(gem, method = "ULS")

PARALLEL(DOSPERT_sub, eigen_type = "SMC")
?PARALLEL
N_FACTORS(DOSPERT_sub, criteria = c("PARALLEL", "EKC", "SMT"),
          eigen_type_other = c("SMC", "PCA"))
N_FACTORS(DOSPERT_sub, method = "ULS")

#transformación ESTO YA NO
library(vegan)
trans<-decostand(pg_especies,method="hellinger")#esto no
trans<-log(pg_especies+1)#esto ya no
frecuencia<-colSums(pg_especies>0)
frecuencia
gem<-pg_especies[,frecuencia>=1]
gem<-log(gem+1)

# 6 FACTORES
EFA(pg_especies,n_factors= 7)
Factores<-EFA(pg_especies,n_factors=7)

EFA(DOSPERT_sub, n_factors = 6)
Factores<-EFA(DOSPERT_sub, n_factors = 6)

F1<-Factores$unrot_loadings[,1]
F2<-Factores$unrot_loadings[,2]
F3<-Factores$unrot_loadings[,3]
F4<-Factores$unrot_loadings[,4]
F5<-Factores$unrot_loadings[,5]
F6<-Factores$unrot_loadings[,6]
F7<-Factores$unrot_loadings[,7]

F1<-Factores$unrot_loadings[,1]
F2<-Factores$unrot_loadings[,2]
F3<-Factores$unrot_loadings[,3]
F4<-Factores$unrot_loadings[,4]
F5<-Factores$unrot_loadings[,5]
F6<-Factores$unrot_loadings[,6]

Facts<-data.frame(F1,F2,F3,F4,F5,F6,F7)
view(Facts)
vars<-rownames(Facts)
Facts<-mutate(Facts,vars=vars)

Facts<-data.frame(F1,F2,F3,F4,F5,F6)
view(Facts)
vars<-rownames(Facts)
Facts<-mutate(Facts, vars=vars)

install.packages("reshape2")
library(reshape2)
Facts2 <- melt(Facts, id="vars", 
               measure=c("F1","F2","F3","F4","F5","F6","F7"), 
               variable.name="Factor", value.name="Loading")

view(Facts2)

ggplot(Facts2, aes(x= vars, y = abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0) +
  ylab("Magnitud de la carga") + 
  theme_bw(base_size=10)+
  theme(axis.text.y=element_text(size=5))
#esto no porque es exploratorio
dominios<-c(rep("Ethic", 6), rep("Fin",6), rep("Hear",6),
            rep("Recr",6), rep("Soc",6))
Facts<-mutate(Facts, dominios=dominios)

ggplot(Facts, aes(x=F1, y=F2, color=dominios)) +
  geom_point() +
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)


##########################
# FASE 5: INTERPRETACIÓN #
##########################


vmx1<-EFA(pg_especies, n_factors = 7, rotation = "varimax")
vmx1


pmx1<-EFA(pg_especies,n_factors=7,rotation="promax")
pmx1<-EFA(DOSPERT_sub, n_factors = 6, rotation = "promax")

EFA(DOSPERT_sub, n_factors = 6, rotation = "promax")
EFA(pg_especies,n_factors=7,rotation="promax")

final<-factanal(pg_especies,factors=7,rotation="varimax",scores="regression")
Factor1<-final$scores[,1]
Factor2<-final$scores[,2]
Factor3<-final$scores[,3]
Factor4<-final$scores[,4]
Factor5<-final$scores[,5]
Factor6<-final$scores[,6]
Factor7<-final$scores[,7]
ambientales<-pgfull[,57:59]
cor(Factor1,ambientales)
cor(Factor2,ambientales)
cor(Factor3,ambientales)
cor(Factor4,ambientales)
cor(Factor5,ambientales)
cor(Factor6,ambientales)
cor(Factor7,ambientales)

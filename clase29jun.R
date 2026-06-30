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



#####################
# FASE 1: Objetivos #
#####################
# Serie de datos con variables para modelar las preferencias de riesgo de las personas
# se empleó la escala de toma de riesgos de dominio específico (DOSPERT)
# https://link.springer.com/article/10.1007/s11166-022-09398-5
# https://osf.io/pjt57/

##################
# FASE 2: DISEÑO #
##################

# Los ítems miden la propensión a asumir riesgos 
# en seis dominios diferentes: 
# social, (soc)
# recreativo, (rec)
# juegos de azar, (rec)
# salud/seguridad, (hear)
# inversión y (fin)
# ético. (eth)
# FA-R

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

PARALLEL(DOSPERT_sub, eigen_type = "SMC")
?PARALLEL
N_FACTORS(DOSPERT_sub, criteria = c("PARALLEL", "EKC", "SMT"),
          eigen_type_other = c("SMC", "PCA"))
N_FACTORS(DOSPERT_sub, method = "ULS")

# 6 FACTORES

EFA(DOSPERT_sub, n_factors = 6)
Factores<-EFA(DOSPERT_sub, n_factors = 6)

F1<-Factores$unrot_loadings[,1]
F2<-Factores$unrot_loadings[,2]
F3<-Factores$unrot_loadings[,3]
F4<-Factores$unrot_loadings[,4]
F5<-Factores$unrot_loadings[,5]
F6<-Factores$unrot_loadings[,6]

Facts<-data.frame(F1,F2,F3,F4,F5,F6)
view(Facts)
vars<-rownames(Facts)
Facts<-mutate(Facts, vars=vars)

install.packages("reshape2")
library(reshape2)
Facts2 <- melt(Facts, id="vars", 
               measure=c("F1","F2","F3","F4","F5","F6"), 
               variable.name="Factor", value.name="Loading")

view(Facts2)

ggplot(Facts2, aes(vars, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_gradient2(name = "Loading", 
                       high = "blue", mid = "white", low = "red", 
                       midpoint=0, guide=F) +
  ylab("Magnitud de la carga") + 
  theme_bw(base_size=10) 
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


vmx1<-EFA(DOSPERT_sub, n_factors = 6, rotation = "varimax")


pmx1<-EFA(DOSPERT_sub, n_factors = 6, rotation = "promax")

EFA(DOSPERT_sub, n_factors = 6, rotation = "promax")

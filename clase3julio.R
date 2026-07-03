#CLASE 3 JULIO EJERCICIOS LIBRO
# Just for  working with Rmarkdown; not needed for analysis: "knitr"
# Load tidyverse to make things simple: "tidyverse", "broom", "broom.mixed"
# Common lm add-ons: "car", "MASS", "Rmisc", "ez", "afex", "emmeans", "lme4", "lmerTest", "nlme", "VCA", "MuMIn", "lattice", "effectsize", "lmtest", 
# Graphics enhancements: "ggExtra", "patchwork", "viridis", "ggsci", "ggforce"

# Package names
packages <- c("knitr", "tidyverse", "broom", "broom.mixed", 
              "car", "MASS", "Rmisc", "ez", "afex", "emmeans", "lme4", "lmerTest", "nlme", "VCA", "MuMIn", "lattice", "effectsize", "lmtest", 
              "ggExtra", "patchwork", "viridis", "ggsci", "ggforce") 

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {  install.packages(packages[!installed_packages]) }

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
library(vegan)
install.packages("DAAG")
library(DAAG)
library(GGally)

# Cargar paquetes e instrucciones
library(vegan)
library(DAAG)
library(GGally)

#################################################
#################################################


# Constants to produce uniform look across figures and to allow changes as necessary
sc="grey60"   #Symbol colour
ac="grey60"   # Axis colour
lf="grey80"   # bars; light fill
lc="black"    #line color - use in single plots
ls=1.15       #line thickness for geom_line
ss=2          #symbol size
fs = 0.4      #starting point for bar fills
fe = 0.95     #end point for bar fills
pw = 7.5      #plot width
pww = 15      #plot width for wide plots
pwm = 10      #plot width for 1.5 column plots
ph = 7.5      #plot height
phm = 10      #plot height for 1.5 rows
phh = 15      #plot height for double height plots
sym2=c(0,1)   # symbol set for plot with 2 symbols with open symbols
sym3=c(0,1,2)
sym4=c(0,1,2,5)
sym2s=c(15,16)   #symbol sets when symbols are solid
sym3s=c(15,16,17)
sym4s=c(15,16,17,18)
theme_qk <- function(){ 
  font <- "sans"   #assign font family up front
  theme_classic() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_line(color = ac), #grey axis ticks
      axis.line = element_line(color = ac),
      #text elements
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9,                 #font size
        color=ac),                #font color       
    )
}

#################################################
#################################################

# Cargar las variables morfológicas y acústicas
feinmorph <- read_csv("C:/Users/100032608/Downloads/feinmorph2.csv")
view(feinmorph)
feinacoust<-read_csv("C:/Users/100032608/Downloads/feinacoust.csv")
view(feinacoust)

# hl = head lenght    # ew = eye diameter 
# td = tympanum diameter # fl = foot length 
# end = eye to naris distance # tl = thigh length 
# ind = internarial distance  # iod = interorbital distance 
# sl = shank length  # dsa = dorsal snout angle

# Análisis exploratorios
scatterplotMatrix(~hw+hl+td+ew+tl+sl+fl+end+nsd+iod+ind+dsa,data=feinmorph,diagonal=list(method='boxplot'))
ggpairs(feinmorph, columns = 2:13)

par(mfrow=c(2,3))
attach(feinmorph)
boxplot(hw~spp)
boxplot(hl~spp)
boxplot(td~spp)
boxplot(ew~spp)
boxplot(tl~spp)
boxplot(sl~spp)
boxplot(fl~spp)
boxplot(end~spp)
boxplot(nsd~spp)
boxplot(iod~spp)
boxplot(ind~spp)
boxplot(dsa~spp)
# 
par(mfrow=c(1,1))

#recomendable en todos los análisis multivariados escalar las variables para que todos estén en la misma escala media 0 varianza 1, todo en calificaciones Z y con la función scale

# Escalado de las variables media = 0, varianza=1
feinmorph$hw <- scale(feinmorph$hw)
feinmorph$hl <- scale(feinmorph$hl)
feinmorph$td <- scale(feinmorph$td)
feinmorph$ew <- scale(feinmorph$ew)
feinmorph$tl <- scale(feinmorph$tl)
feinmorph$sl <- scale(feinmorph$sl)
feinmorph$fl <- scale(feinmorph$fl)
feinmorph$end <- scale(feinmorph$end)
feinmorph$nsd <- scale(feinmorph$nsd)
feinmorph$iod <- scale(feinmorph$iod)
feinmorph$ind <- scale(feinmorph$ind)
feinmorph$dsa <- scale(feinmorph$dsa)

# Homogeneidad de varianzas multivariada
#  Marti Anderson's PERMDISP2

feinmorph.dist <- dist(feinmorph[,-1])
feinmorph.disp <- betadisper(feinmorph.dist,feinmorph$spp)
permutest(feinmorph.disp)
anova(feinmorph.disp)#que las varianzas son iguales?, que son homogéneas

# Análisis discriminante lineal LDA
# con validación cruzada jackknife
#jackknife hace el remuestreo, técnica de remuestreo para hacerel cuadro del pirzarron 

feinmorphjac.lda <- lda(spp~hw+hl+td+ew+tl+sl+fl+end+nsd+iod+ind+dsa, data=feinmorph, CV=TRUE)
print(feinmorphjac.lda)
table(feinmorph$spp,feinmorphjac.lda$class)#esta es la predicción, calculas las funciones y las usas para clasificar, con los datos que tienes cuantos logras clasificar


# Eficiencia de la clasificación
feinmorphjac.class <- feinmorphjac.lda$class
feinmorphjac.table <- table(feinmorph$spp, feinmorphjac.class)
diag(prop.table(feinmorphjac.table,1))#valores de pertenencia o algo así
#si medio sospechabamos que no iba a salir tan bien porque las variables no estaban separadas con claridad. aún uniendo las variables hay una tasa de error bastante grande.


confusion(feinmorphjac.lda$class,feinmorph$spp)#en general no está tan mal 
feinmorph.lda <- lda(spp~hw+hl+td+ew+tl+sl+fl+end+nsd+iod+ind+dsa, data=feinmorph)
print(feinmorph.lda)

# Predicciones
feinmorph.pred <- predict(feinmorph.lda)
table(feinmorph$spp,feinmorph.pred$class)

# Gráficos
plot(feinmorph.lda, dimen=2)
detach(feinmorph)
###################
# Datos acústicos #
###################

# Análisis explotatorio
attach(feinacoust)
scatterplotMatrix(~crt+cdc+pn+df+tcl+tcr,data=feinacoust,diagonal=list(method='boxplot'))
scatterplot(crt,cdc)
ranitas<-aov(crt~spp)
plot(ranitas)#heterocedástico en cono,los residuales no son normales, ya estandarizados son heterocidásticos y ahí está la curva, las relaciones no son lineales. 
str(feinacoust)
# Estandarización de las variables
feinacoust$crt <- scale(feinacoust$crt)
feinacoust$cdc <- scale(feinacoust$cdc)
feinacoust$pn <- scale(feinacoust$pn)
feinacoust$df <- scale(feinacoust$df)
feinacoust$tcl <- scale(feinacoust$tcl)
feinacoust$tcr <- scale(feinacoust$tcr)

# Homogeneidad de var-cov
feinacoust.dist <- dist(feinacoust[,-(1:2)])
feinacoust.disp <- betadisper(feinacoust.dist,feinacoust$spp)
permutest(feinacoust.disp)#no son homogeneas las varianzas, necesita mucho trabajo

# ADL
feinacoustjac.lda <- lda(spp~crt+cdc+pn+df+tcl+tcr, CV=TRUE, data=feinacoust)
print(feinacoustjac.lda)
table(feinacoust$spp,feinacoustjac.lda$class)

# Proporciones
feinacoustjac.class <- feinacoustjac.lda$class
feinacoustjac.table <- table(feinacoust$spp, feinacoustjac.class)
diag(prop.table(feinacoustjac.table,1))

# matriz de confusión para ver la precisión (validación)
confusion(feinacoustjac.lda$class,feinacoust$spp)#este es para ver la eficiencia, dice que el 95%

feinacoust.lda <- lda(spp~crt+cdc+pn+df+tcl+tcr, data=feinacoust)
print(feinacoust.lda)
# eigenvalores (proporción explicada)
feinacoust.lda$svd
propexp <- feinacoust.lda$svd^2/sum(feinacoust.lda$svd^2)
propexp
feinacoust.pred <- predict(feinacoust.lda)
table(feinacoust$spp,feinacoust.pred$class)
plot(feinacoust.lda, dimen=2)

#TRABAJAR MÁS LAS DESTAS, como estamos usando un método que las variables son lineales, que las varianzas son iguales cuando esto no pasa, no sucede jeje
#trabajarlas para transformar estos datos


# Resumen MANOVAS
morph <- cbind(feinmorph$hw,feinmorph$hl,feinmorph$td,feinmorph$ew,feinmorph$tl,feinmorph$sl,feinmorph$fl,feinmorph$end,feinmorph$nsd,feinmorph$iod,feinmorph$ind,feinmorph$dsa)
feinmorph.man <- manova(morph~spp, data=feinmorph)
summary(feinmorph.man, test="Pillai")
acoust <- cbind(feinacoust$crt,feinacoust$cdc,feinacoust$pn,feinacoust$df,feinacoust$tcl,feinacoust$tcr)
feinacoust.man <- manova(acoust~spp, data=feinacoust)
summary(feinacoust.man, test="Pillai")

# GRÁFICOS
source("../appearance.R")

lda.data <- cbind(feinmorph, predict(feinmorph.lda)$x)

# Agrupaciones
#el hull es como una caja con las funciones discrminantes, es un gráfico para hacer polígonos, 
hull <- 
  lda.data %>%
  group_by(spp) %>% 
  slice(chull(LD1, LD2))
la=c("R.kauffeldi", "R. pipiens", "R. palustris", "R. spheno.", "R. sylvatica")
p1<-ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(shape = spp), show.legend=FALSE, size= ss/2)+
  scale_shape_manual(values=c(0:5))+
  geom_polygon(data=hull, aes(fill = spp,
  ), color=lc,
  alpha = 0.3,
  show.legend = FALSE)+
  scale_fill_grey(start = 0.4, end = .9)

lda2.data <- cbind(feinacoust, predict(feinacoust.lda)$x)


hull2 <- 
  lda2.data %>%
  drop_na() %>%
  group_by(spp) %>% 
  slice(chull(LD1, LD2))
p2<-ggplot(lda2.data, aes(LD1, LD2)) +
  geom_point(aes(shape = spp), size=ss/2)+
  scale_shape_manual(values=c(0:5),
                     name="Species",
                     labels=la,
                     guide =
                       guide_legend(label.theme = element_text(angle = 0, face = "italic", size=6),
                                    title=NULL))+
  labs(y=NULL)+
  geom_polygon(data=hull2, aes(fill = spp,
  ), colour = "black",
  alpha = 0.3,
  show.legend = FALSE)+
  scale_fill_grey(start = 0.4, end = .9)


p3<-p1+p2 & theme_qk()
p3
p1
p2

p1a<-ggplot(lda.data, aes(LD1, LD2, colour=spp)) +
  geom_point(aes(), show.legend=FALSE, size= ss/2)+
  scale_color_viridis_d()+
  geom_polygon(data=hull, aes(fill = spp,
  ), 
  alpha = 0.3,
  show.legend = FALSE)+
  scale_fill_viridis_d()

p2a<-ggplot(lda2.data, aes(LD1, LD2, colour=spp)) +
  geom_point(size=ss/2)+
  scale_color_viridis_d(
    name="Species",
    labels=la,
    guide =
      guide_legend(label.theme = element_text(angle = 0, face = "italic", size=6),
                   title=NULL))+
  labs(y=NULL)+
  geom_polygon(data=hull2, aes(fill = spp,
  ),
  alpha = 0.3,
  show.legend = FALSE)+
  scale_fill_viridis_d()
p3a<-p1a+p2a&theme_qk()   
p3a
p1a
p2a

p3
p3a #gráficos de los determinantes. 

#análisis de conglomerados jerarquico.
#ya está la matriz de distancias 
feinmorph.dist
#etiquetar los renglones 
grupos<-hclust(feinmorph.dist)
plot(grupos)
feinmorph2<-feinmorph[,-1]
feinmorph2
rownames(feinmorph2)<-feinmorph[,1]#hay nombres triplicados y duplicados. 
#tendríamos que hacer no se que para quitar eso de los duplicados y así 
table(feinmorph$spp)
#vector con los números tal vez
unu<-c(seq(from=1,to=157,by=1),c(seq(from=1,to=30,by=1)),c(seq(from=1,to=31,by=1)),c(seq(from=1,to=46,by=1)))
unu
length(unu)
especieses<-paste(feinmorph[,1], unu, sep = "")
head(especieses)
rownames(feinmorph2)<-especieses
feinmorph.dist<-dist(feinmorph2)
grupos<-hclust(feinmorph.dist)

plot(grupos,cex=2)
view(feinmorph2)

install.packages("ggdendro")
library(ggdendro)
library(plotly)
plot_dendro(grupos)


#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
####### ECUACIONES ESTRUCTURALES TEMA 3 JULIO ###########################
#########################################################################
#########################################################################

# https://rpubs.com/Agrele/SEM

library(lavaan)
library(lavaanPlot)
library(piecewiseSEM)
library(tidyverse)


set.seed(2002)
# datos simulados
# riqueza de orugas dependiente de temperatura y tiempo en años
# la precipitación no afecta a la riqueza

semdata <- data.frame('years' = 1:60) |> 
  within({
    precip <- rnorm(60, 100, 10) 
    temperature <- 0.05 + 0.05*years + rnorm(60, 1, 0.5) 
    catrich <- 350 - 2*years -50*temperature + rnorm(60, 50, 50)
  })

view(semdata)

# Gráficos
ggplot(semdata, aes(years, temperature))+
  geom_point()+
  geom_smooth(method = lm)


ggplot(semdata, aes(years, catrich))+
  geom_point()+
  geom_smooth(method = lm)

# Matriz de varianzas-covarianzas
cov(semdata)


##MODELO
# En el siguiente modelo las variables exógenas son años y temperatura.
# Las variables endógenas son la riqueza de orugas y la precipitación.
# Las precipitaciones se predicen por años, la riqueza de orugas se predice por años y la temperatura, la temperatura se predice por años

model1 <- '
  precip + temperature ~ years
  catrich ~ years + temperature'

# Ajuste del modelo
model1.fit <- sem(model1, data = semdata) 


# Resumen
summary(model1.fit, rsq = TRUE, fit.measures = TRUE, standardized = TRUE) 


#Gráfica del modelo
lavaanPlot(name = "model1", model1.fit, coefs = TRUE) # prints path diagram that specifies relationships specified in `model1` along with the estimated path coefficients from the fitted model (here, the unstandardized coefficient estimates in the summary output.)

# Mismo modelo datos escalados
semdata_scaled <- apply(semdata, MARGIN = 2, scale)

model1.scaled <- sem(model1, data = semdata_scaled)
summary(model1.scaled, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

lavaanPlot(name = "scaled", model1.scaled, coefs = TRUE)

# Otro modelo mas simple
model2 <- 'precip +temperature ~ years
    catrich ~ temperature'

model2.fit <- sem(model2, data = semdata_scaled) #still using scaled data
summary(model2.fit, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)

lavaanPlot(name = "model2", model2.fit, labels = names(semdata_scaled), coefs = TRUE)

# Evaluación de modelos
AIC(model1.fit, model1.scaled, model2.fit)

#CLASE 07JUL
library(tidyverse)
install.packages("lavaan")
library(lavaan)
install.packages("lavaanPlot",dependencies=T)
library(lavaanPlot)
install.packages("piecewiseSEM")
library(piecewiseSEM)

# https://rpubs.com/Agrele/SEM

library(lavaan)
library(lavaanPlot)
library(piecewiseSEM)
library(tidyverse)


set.seed(2026)
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
install.packages("AICcmodavg")
library(AICcmodavg)
model_list<-list(model1.fit,model1.scaled,model2.fit)
model_names<-c("mo1","mod1b","mod2")
aictab(cand.set = model_list,modnames=model_names)
anova(model1.scaled,model2.fit)



# https://stats.oarc.ucla.edu/r/seminars/rsem/#s1a
# Se estudian los efectos de los antecedentes de los estudiantes 
# en el rendimiento académico. 
# 9 variables observadas: motivación, armonía, estabilidad, psicología parental negativa, 
# SSE, coeficiente intelectual verbal, lectura, aritmética y ortografía. 
# El investigador principal plantea la hipótesis de tres constructos latentes: 
# Ajuste, Riesgo y Logro 


# Adjustment
## motiv Motivation
## harm Harmony
## stabi Stability
# 
# Risk
## ppsych (Negative) Parental Psychology
## ses SES
## verbal Verbal IQ
# 
# Achievement
## read Reading
## arith Arithmetic
## spell Spelling
# 

dat <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2021/02/worland5.csv")

# Analicemos la matriz de varianzas covarianzas

cov(dat)


m6a <- '
# measurement model
adjust =~ motiv + harm + stabi
risk =~ verbal + ppsych + ses
achieve =~ read + arith + spell
# regressions
achieve ~ adjust + risk
'
fit6a <- sem(m6a, data=dat)
summary(fit6a, standardized=TRUE, fit.measures=TRUE)

lavaanPlot(name = "m6a", fit6a, coefs = TRUE) 
#arriba las latentes independientes o sea los ovalos y luego las dependientes los rectángulos

install.packages("semPlot")
library(semPlot)
data("PoliticalDemocracy")
jul7<-PoliticalDemocracy

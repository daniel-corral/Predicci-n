##### Carga de paquetes #####
library(dplyr)
library(tidyverse)
library(ggplot2)

##### Carga y limpieza de datos #####
dataNBA = read.csv("./nba.csv")

#Limpiamos na
dataNBAclean = na.omit(dataNBA)

# Comprobacion datos duplicados
duplicated(dataNBAclean)
nrow(dataNBAclean[duplicated(dataNBAclean$Player),])
dataNBAclean <- dataNBAclean[!duplicated(dataNBAclean$Player),]

##### Creacion de diferentes modelos ##### 
# Escogemos diferentes modelos para compararlos y seleccionar el... 
# modelo optimo.

# Model
model <- lm(Salary~+Age+PER+TS.+TRB.+OWS+DWS+WS, 
            data = dataNBAclean)
summary(model)

# Model1
model1 <- lm(Salary~ . - Player, 
            data = dataNBAclean)
summary(model1)

# Model2
model2 <- lm(Salary~. - Player - NBA_Country - Tm, 
             data = dataNBAclean)
summary(model2)

##### QQPlot #####
# Comparacion de cuantiles de la distribucion observada con los cuantiles
# teoricos de una distribucion normal con la misma media y desviacion
# estandar que los datos. Cuanto mas se aproximen los datos a una normal,
# mas alineados estan los puntos en torno a la recta.

install.packages("car")
library(car)
qqPlot(model2, labels = row.names(dataNBAclean), id.method = 'identify',
       simulate = TRUE, main = "Q-Q Plot")
# Encontramos 4 puntos fuera de la grafica: 112, 114, 326, 328

##### Histograma + densidad + normal + rug #####
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks = nbreaks, freq = FALSE,
       xlab = "Studentized Residual",
       main = "Distribution of Errors")
  rug(jitter(z), col = "brown")
  curve(dnorm(x, mean = mean(z), sd = sd(z)),
        add = TRUE, col = "blue", lwd = 2)
  lines(density(z)$x, density(z)$y,
        col = "red", lwd = 2, lty = 2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty = 1:2, col = c("blue","red"), cex = .7)
}

residplot(model2)
# Podemos observar que no sigue una distribucion Normal

##### Jarque Bera #####

# Comprobar si una muestra tienen asimetria y curtosis de dist normal
library(fBasics)
vResid = resid(model2)
jbTest(vResid)
# aproximadamente 0 . Rechazamos ho, los datos no tienen asimetria
# y curtosis de distribucion normal

##### Shapiro-Wilk #####

shapiro.test(vResid)
# rechazamos h nula, datos no vienen distribucion normal

##### Linealidad, Componentes o Gráficos de residuos parciales #####
# grafican valores ajustados con respecto predictores, si no hay
# problemas de linealidad se obtiene una recta donde se representan puntos
crPlots(model2)

##### Varianza Constante. Homocedasticidad #####
ncvTest(model2)
spreadLevelPlot(model2)
# varianza no constante, rechazamos h nula

##### Validación Global ##### 
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(model2) 
summary(gvmodel)

##### Multicolinealidad #####
# en la presencia de alta correlacion entre los predictores puede
# producir problemas de impresion de los estimadores. Varianzas 
# estimadores mayores de las que deberian ser.
vif(model2) 
sqrt(vif(model2)) > 2

##### Outliers #####
# observaciones anomalas. Identificamos valores atipicos mediante un
# Bonferroni p-values. 
outlierTest(model2)
# rechazamos ho, por lo tanto no hay atipicos

# valores extremos representacion grafica
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h = c(2,3)*p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(model2)

# Valores Influyentes #
cutoff <- 4/(nrow(dataNBAclean) - length(model2$coefficients) - 2)
plot(model2, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

avPlots(model2, ask = FALSE, id.method = "identify")

influencePlot(model2, id.method = "identify", main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance" )

##### Interaccion ##### 
regresInter = lm(Salary~Age*PER*TS.*TRB.*OWS*DWS*WS, 
               data = dataNBAclean)
summary(regresInter)
# comprobamos que no hay interacciones significativas

##### Selección de variables #####
AIC(model,model1, model2)
BIC(model,model1, model2)
# segun AIC y BIC model2 es el mejor entre esos

##### Metodos de seleccion de variables #####
install.packages("leaps")
library(leaps)
regfit.full = regsubsets(Salary~. - Player - NBA_Country - Tm, 
                         data = dataNBAclean)
reg.summary = summary(regfit.full)
reg.summary
# creamos modelo con las variables obtenidas
bestmodel = lm(Salary ~ NBA_DraftNumber + Age + G + MP + DRB. + USG. + OWS + 
                 VORP, 
               data = dataNBAclean)
summary(bestmodel)
# R cuadrado ajustado 0.5327

reg.summary$rss
reg.summary$cp
reg.summary$aic
reg.summary$bic

# Forward Stepwise
install.packages("MASS")
library(leaps)
library(MASS)

regfit.fwd = regsubsets(Salary~. - Player - NBA_Country - Tm,
                      dataNBAclean,
                      method = "forward")
summary(regfit.fwd )
forwardmodel = lm(Salary~+NBA_DraftNumber+Age+G+MP+DRB.+USG.+WS+VORP, 
                  data = dataNBAclean)
summary(forwardmodel)
# Con Adjusted R-squared = 0.5322

# Backward Stepwise
stepAIC(model2, direction = "backward")
backwardmodel = lm(Salary~+NBA_DraftNumber+Age+G+MP+PER+X3PAr+ORB.+TRB.+USG.+WS+OBPM, 
                  data = dataNBAclean)
summary(backwardmodel)
# Con Adjusted R-squared = 0.5329

# Modelo mixto
stepAIC(model2, direction = "both")
mixmodel = lm(Salary ~ NBA_DraftNumber + Age + G + MP + PER + X3PAr + ORB. + 
                TRB. + USG. + WS + OBPM, 
                   data = dataNBAclean)
summary(mixmodel)
# Con Adjusted R-squared = 0.5329
# Podemos observar como tanto el modelo mixto como el backward son el mejor modelo. Ambos modelos son iguales.

# PREDECIMOS 10 NOMBRES ALEATORIOS DEL DATASET CON SEMILLA 1234
set.seed(1234)
mixmodelpredict <- predict(mixmodel, dataNBAclean)
mixmodelpredict[sample(1:481, 10)]


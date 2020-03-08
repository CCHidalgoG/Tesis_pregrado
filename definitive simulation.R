####Simulaciónes########

library(knitr)
library(mvtnorm)
library(randomForest)
library(nortest)
library(forestFloor)
library(MASS)
library(scatterplot3d)
library(visreg)
library(car)
library(rgl)

########################################################################
###################################CASO 1##############################
#######################################################################

##Variables independientes independientes##
rm(list=ls())
set.seed(1)

#Simulacion de datos normales multivariados
# Simularemos varias distribuciones normales en tres dimensiones:
N=1000
mu=c(0,0,0)
sigma <- matrix(c(1,0.1,0.95,0.1,1,0.9,0.95,0.9,1),ncol=3,nrow=3)
sigma=Matrix::nearPD(sigma)
for (i in 1:15){
  sigma[i]=Matrix::nearPD(sigma$mat)
  diag(sigma$mat)=1
  sigma=Matrix::nearPD(sigma$mat)
}
sigma$mat
## Simulacion de los datos incorrelacionados:
muestra1=MASS::mvrnorm(N, mu=mu, Sigma=sigma$mat)
colnames(muestra1)=c("X1","X2","Y")

##Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

####Modelo lineal múltiple
modelm= lm(Y~X1+X2,data=entren)#Entrenamos el modelo con el training set

#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
plot(valid[,3],valid[,4],xlab="Valor real", ylab="valor estimado"
     , las=1, col="skyblue")#Gráfico para el caso uno
##Media de los errores
residuos_lm=valid[,3]-valid[,4]

lillie.test(residuos_lm)

plot(residuos_lm, xlab="", ylab= "Residuos", main="Modelo lineal multivariado", 
     col="blue")


with(valid,plot3d(X1,X2,Y, col="blue", size=0.7, type="s", 
                  main="3D Linear Model Fit"))
#Creamos el plano
newdat <- expand.grid(X1=seq(-3,3,by=0.1),X2=seq(-3,3,by=0.1))
newdat$y_lm=predict(modelm,newdata=newdat)

with(newdat,surface3d(unique(X1),unique(X2),y_lm, 
                      alpha=0.3,front="line", back="line"))

###Random forest
Bosque1=randomForest(Y~X1+X2, data=entren,ntree=800,importance=T,keep.inbag = TRUE)
plot(Bosque1)
y_rf=predict(Bosque1,valid)
plot(valid[,3],y_rf,xlab="Valor real", ylab="valor estimado", main="", col= "chartreuse2")
#
residuos_rf=valid[,3]-y_rf
#
lillie.test(residuos_rf)
##
plot(residuos_rf, xlab="", ylab= "Residuos random fores", main="", 
     col="chartreuse2")

ff=forestFloor(Bosque1, X =valid[,1:2])
show3d(ff,col="green",plot.rgl=list(size=8)) 

#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)


########################################################################
###################################CASO 2##############################
#######################################################################

rm(list=ls())
set.seed(1)
#Ver gregorutti
# Simularemos varias distribuciones normales en tres dimensiones:
N=1000 ##Número de muestra extraída
mu=c(0,0,0)##" Media normal multivariada
sigma <- matrix(c(1, 0.9,0.95,0.9,1,0.95,0.95,0.95,1),ncol=3,nrow=3)#Matriz varianza covarianza

## Simulacion de los datos correlacionados:
muestra1=rmvnorm(N,mean=mu,sigma=sigma,method = "eigen")#Obtenemos muestra con datos anteriores
colnames(muestra1)=c("X1","X2","Y")#Nombres de las columnas

##Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

#Modelos

####Modelo lineal múltiple
modelm= lm(Y~X1+X2,data=entren)#Entrenamos el modelo con el training set

#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
plot(valid[,3],valid[,4],xlab="Valor real", ylab="valor estimado"
     , las=1, col="skyblue")#Gráfico para el caso uno
##Media de los errores
residuos_lm=valid[,3]-valid[,4]

lillie.test(residuos_lm)

plot(residuos_lm, xlab="", ylab= "Residuos", main="Modelo lineal multivariado", 
     col="blue")


with(valid,plot3d(X1,X2,Y, col="blue", size=0.7, type="s", 
                  main="3D Linear Model Fit"))
#Creamos el plano
newdat <- expand.grid(X1=seq(-3,3,by=0.1),X2=seq(-3,3,by=0.1))
newdat$y_lm=predict(modelm,newdata=newdat)

with(newdat,surface3d(unique(X1),unique(X2),y_lm, 
                      alpha=0.3,front="line", back="line"))

###Random forest
Bosque1=randomForest(Y~X1+X2, data=entren,ntree=800,importance=T,keep.inbag = TRUE)
plot(Bosque1)
y_rf=predict(Bosque1,valid)
plot(valid[,3],y_rf,xlab="Valor real", ylab="valor estimado", main="", col= "chartreuse2")
mean((valid[,3]-y_rf)^2)
#
residuos_rf=valid[,3]-y_rf
#
lillie.test(residuos_rf)
##
plot(residuos_rf, xlab="", ylab= "Residuos random fores", main="", 
     col="chartreuse2")

ff=forestFloor(Bosque1, X =valid[,1:2])
show3d(ff,col="green",plot.rgl=list(size=8)) 

#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)

########################################################################
###################################CASO 3##############################
#######################################################################

rm(list=ls())
set.seed(1)
#Tres variables
N=1000
mu=c(0,0,0,0)
C=matrix(c(1, 0.94,0.01,0.94,1,0.01,0.01,0.01,1),ncol=3,nrow=3)
tau=c(0.9,0.9,0.65)
vary=1
sigma<-rbind(cbind(C,tau),c(tau,1))
sigma=Matrix::nearPD(sigma)
## Simulacion de los datos correlacionados:
muestra1=MASS::mvrnorm(n=1000, mu=mu, Sigma=sigma$mat)
colnames(muestra1)=c("X1","X2","X3","Y")

##Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

#Modelos

####Modelo lineal múltiple
modelm= lm(Y~X1+X2+X3,data=entren)#Entrenamos el modelo con el training set

#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
plot(valid[,4],valid[,5],xlab="Valor real", ylab="valor estimado"
     , las=1, col="skyblue", main="Modelo lineal")#Gráfico para el caso uno
##Media de los errores
residuos_lm=valid[,4]-valid[,5]

lillie.test(residuos_lm)

plot(residuos_lm, xlab="", ylab= "Residuos", main="Modelo lineal multivariado", 
     col="blue")

###Random forest
Bosque1=randomForest(Y~X1+X2+X3, data=entren,ntree=800)
y_rf=predict(Bosque1,valid)
plot(valid[,4],y_rf,xlab="Valor real", ylab="valor estimado",
     main="Bosque aleatorio", col= "chartreuse2")
#
residuos_rf=valid[,4]-y_rf
#
lillie.test(residuos_rf)
##
plot(residuos_rf, xlab="", ylab= "Residuos random fores", main="", 
     col="chartreuse2")
#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)

########################################################################
###################################CASO 4##############################
#######################################################################

rm(list=ls())
set.seed(1)
#Veinte variables
p=20
C=matrix(rep(0.88:0.88,400),ncol=p,nrow=p)
diag(C)=1
tau=c(rep(0.85:0.85,20))
vary=1
mu=c(rep(0:0,21))
sigma<-rbind(cbind(C,tau),c(tau,1))
sigma=Matrix::nearPD(sigma)
## Simulacion de los datos correlacionados:
muestra1=MASS::mvrnorm(n=1000, mu=mu, Sigma=sigma$mat)

colnames(muestra1)=c("X1","X2","X3","X4","X5",
                     "X6","X7","X8","X9","X10","X11","X12","X13","X14","X15",
                     "X16","X17","X18","X19","X20","Y")
##Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

#Modelos

####Modelo lineal múltiple
modelm= lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+
             X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20,
           data=entren)#Entrenamos el modelo con el training set

#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
plot(valid[,21],valid[,22],xlab="Valor real", ylab="valor estimado"
     , las=1, col="skyblue", main="Modelo lineal")#Gráfico para el caso uno
##Media de los errores
residuos_lm=valid[,21]-valid[,22]

lillie.test(residuos_lm)

plot(residuos_lm, xlab="", ylab= "Residuos", main="Modelo lineal multivariado", 
     col="blue")

###Random forest
Bosque1=randomForest(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+
                       X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20, 
                     data=entren,ntree=800)
y_rf=predict(Bosque1,valid)
plot(valid[,21],y_rf,xlab="Valor real", ylab="valor estimado",
     main="Bosque aleatorio", col= "chartreuse2")
#
residuos_rf=valid[,4]-y_rf
#
lillie.test(residuos_rf)
##
plot(residuos_rf, xlab="", ylab= "Residuos random fores", main="", 
     col="chartreuse2")
#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)

########################################################################
###################################CASO 5##############################
#######################################################################

rm(list=ls())
set.seed(1)
#Veinte variables


N=1000
p=20
q=5
mu=c(rep(0:0,20))
C=matrix(rep(0.92:0.92,400),ncol=p,nrow=p)
C[16:20,]=0.2
C[,16:20]=0.2
diag(C)=1
tau=c(rep(0.87:0.87,15),rep(0.4:0.4,5))
vary=1
mu=c(rep(0:0,21))
sigma<-rbind(cbind(C,tau),c(tau,1))
sigma=Matrix::nearPD(sigma)
for (i in 1:15){
  sigma=Matrix::nearPD(sigma$mat)
  diag(sigma$mat)=1
  sigma=Matrix::nearPD(sigma$mat)
}

## Simulacion de los datos correlacionados:
muestra1=MASS::mvrnorm(n=1000, mu=mu, Sigma=sigma$mat)
colnames(muestra1)=c("X1","X2","X3","X4","X5","X6","X7",
                     "X8","X9","X10","X11","X12","X13","X14",
                     "X15","X16","X17","X18","X19","X20","Y")


##Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

#Modelos

####Modelo lineal múltiple
modelm= lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+
             X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20,
           data=entren)#Entrenamos el modelo con el training set

#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
plot(valid[,21],valid[,22],xlab="Valor real", ylab="valor estimado"
     , las=1, col="skyblue", main="Modelo lineal")#Gráfico para el caso uno
##Media de los errores
residuos_lm=valid[,21]-valid[,22]

lillie.test(residuos_lm)

plot(residuos_lm, xlab="", ylab= "Residuos", main="Modelo lineal multivariado", 
     col="blue")

###Random forest
Bosque1=randomForest(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+
                       X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20, 
                     data=entren,ntree=800,importance=T,keep.inbag = TRUE)
y_rf=predict(Bosque1,valid)
plot(valid[,21],y_rf,xlab="Valor real", ylab="valor estimado",
     main="Bosque aleatorio", col= "chartreuse2")
#
residuos_rf=valid[,4]-y_rf
#
lillie.test(residuos_rf)
##
plot(residuos_rf, xlab="", ylab= "Residuos random fores", main="", 
     col="chartreuse2")
#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)

########################################################################
###################################CASO 6##############################
#######################################################################

rm(list=ls())
set.seed(1)
#cien variables
library(Matrix)
#Creamos una matriz aleatoria simétrica de correlación
C<-Matrix(rnorm(1600),40)#Creamos la matriz
C<-forceSymmetric(C)#La tornamos simétrica
diag(C)=1#Matriz de correlación
tau=runif(40, 0.2,0.99)#correlaciones con la variables Y
vary=runif(40,1,5)#Varianza de cad muestra extraída
mu=runif(41,-2,2)
sigma<-rbind(cbind(C,tau),c(tau,1))
sigma=Matrix::nearPD(sigma)
for (i in 1:15){
  sigma=Matrix::nearPD(sigma$mat)
  diag(sigma$mat)=1
  sigma=Matrix::nearPD(sigma$mat)
}

## Simulacion de los datos aleatorios normales con media
#mu= runif(100, -3,3) y varianza runif(100,-10,10):
muestra1=MASS::mvrnorm(n=1000, mu=mu, Sigma=sigma$mat)


#Asignamos nombres a las variables creadas
nam=NULL
for(i in 1:40) { 
  nam <- rbind(nam,paste("X", i, sep = ""))
}
colnames(muestra1)=c(nam,"Y")

##Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

#Modelos

####Modelo lineal múltiple
fo=paste("Y~X1")
for (i in 2:40){
  fo=paste(fo,"+",colnames(muestra1)[i])
}

#Modelo lineal
modelm= lm(formula(fo),
           data=entren)#Entrenamos el modelo con el training set
#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
plot(valid[,41],valid[,42],xlab="Valor real", ylab="valor estimado"
     , las=1, col="skyblue", main="Modelo lineal")#Gráfico para el caso uno
##Media de los errores
residuos_lm=valid[,41]-valid[,42]

lillie.test(residuos_lm)

plot(residuos_lm, xlab="", ylab= "Residuos", main="Modelo lineal multivariado", 
     col="blue")

###Random forest
Bosque1=randomForest(formula(fo), data=entren,ntree=800)

y_rf=predict(Bosque1,valid)
plot(valid[,41],y_rf,xlab="Valor real", ylab="valor estimado",
     main="Bosque aleatorio", col= "chartreuse2")
#
residuos_rf=valid[,41]-y_rf
#
lillie.test(residuos_rf)
##
plot(residuos_rf, xlab="", ylab= "Residuos random fores", main="", 
     col="chartreuse2")
#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)


########################################################################
##########################CASOs 7, 8, 9, 10, 11 y 12####################
#######################################################################


#######CASO 7 = cuatro variables correlacionadas
rm(list=ls())
set.seed(1)
#cien variables
library(Matrix)
p=4
caso7=matrix(rep(0.8:0.8,16),ncol=p,nrow=p)
diag(caso7)=1
tau=c(rep(0.85:0.85,4))
vary=1
mu=c(rep(0:0,5))
sigma<-rbind(cbind(caso7,tau),c(tau,1))
sigma=Matrix::nearPD(sigma)

## Simulacion de los datos correlacionados:
muestra1=MASS::mvrnorm(n=1000, mu=mu, Sigma=sigma$mat)
colnames(muestra1)=c("X1","X2","X3","X4","Y")

#Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

#Modelo lineal
modelm= lm(Y~.,
           data=entren)#Entrenamos el modelo con el training set
#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
##Media de los errores
residuos_lm=valid[,5]-valid[,6]
mean(residuos_lm^2)
###Random forest
Bosque1=randomForest(Y~., data=entren,ntree=800)

y_rf=predict(Bosque1,valid)
#
residuos_rf=valid[,5]-y_rf
#
mean(residuos_rf^2)
##
#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)

#######CASO 8 = seis variables correlacionadas
rm(list=ls())
set.seed(1)
#cien variables
library(Matrix)
p=6
C=matrix(rep(0.8:0.8,36),ncol=p,nrow=p)
diag(C)=1
tau=c(rep(0.85:0.85,6))
vary=1
mu=c(rep(0:0,7))
sigma<-rbind(cbind(C,tau),c(tau,1))
sigma=Matrix::nearPD(sigma)

## Simulacion de los datos correlacionados:
muestra1=MASS::mvrnorm(n=1000, mu=mu, Sigma=sigma$mat)
colnames(muestra1)=c("X1","X2","X3","X4","X5","X6","Y")

#Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

#Modelo lineal
modelm= lm(Y~.,
           data=entren)#Entrenamos el modelo con el training set
#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
##Media de los errores
residuos_lm=valid[,5]-valid[,6]
mean(residuos_lm^2)
###Random forest
Bosque1=randomForest(Y~., data=entren,ntree=800)

y_rf=predict(Bosque1,valid)
#
residuos_rf=valid[,5]-y_rf
#
mean(residuos_rf^2)
##
#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)

#######CASO 9 = ocho variables correlacionadas
rm(list=ls())
set.seed(1)
#cien variables
library(Matrix)
p=8
C=matrix(rep(0.8:0.8,64),ncol=p,nrow=p)
diag(C)=1
tau=c(rep(0.85:0.85,8))
vary=1
mu=c(rep(0:0,9))
sigma<-rbind(cbind(C,tau),c(tau,1))
sigma=Matrix::nearPD(sigma)

## Simulacion de los datos correlacionados:
muestra1=MASS::mvrnorm(n=1000, mu=mu, Sigma=sigma$mat)
colnames(muestra1)=c("X1","X2","X3","X4","X5","X6","X7","X8","Y")

#Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

#Modelo lineal
modelm= lm(Y~.,
           data=entren)#Entrenamos el modelo con el training set
#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
##Media de los errores
residuos_lm=valid[,5]-valid[,6]
mean(residuos_lm^2)
###Random forest
Bosque1=randomForest(Y~., data=entren,ntree=800)

y_rf=predict(Bosque1,valid)
#
residuos_rf=valid[,5]-y_rf
#
mean(residuos_rf^2)
##
#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)

#######CASO 10 = diez variables correlacionadas
rm(list=ls())
set.seed(1)
#cien variables
library(Matrix)
p=10
C=matrix(rep(0.8:0.8,100),ncol=p,nrow=p)
diag(C)=1
tau=c(rep(0.85:0.85,10))
vary=1
mu=c(rep(0:0,11))
sigma<-rbind(cbind(C,tau),c(tau,1))
sigma=Matrix::nearPD(sigma)

## Simulacion de los datos correlacionados:
muestra1=MASS::mvrnorm(n=1000, mu=mu, Sigma=sigma$mat)
colnames(muestra1)=c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","Y")

#Entrenamiento
sampling=sample(1:nrow(muestra1),round(0.75*nrow(muestra1)))
#Conjunto entrenamiento
entren= data.frame(muestra1[sampling,])
#Conjunto validación
valid=data.frame(muestra1[-sampling,])

#Modelo lineal
modelm= lm(Y~.,
           data=entren)#Entrenamos el modelo con el training set
#Predicción
valid$y_lm=predict(modelm,valid)#Según el modelo estimado, realizamos las predicciones
##Media de los errores
residuos_lm=valid[,5]-valid[,6]
mean(residuos_lm^2)
###Random forest
Bosque1=randomForest(Y~., data=entren,ntree=800)

y_rf=predict(Bosque1,valid)
#
residuos_rf=valid[,5]-y_rf
#
mean(residuos_rf^2)
##
#Razón entre el error del random forest y el modelo lineal

mean(residuos_rf^2)/mean(residuos_lm^2)


# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 2 #####

install.packages("pacman") #Instalar librería si no cuenta con esta 
library(pacman) #Llamar librería
#Se cargan las librerías a usar en el presente Problem Set
p_load(caret, 
       Matrix,
       recipes,
       rio, #Instalar librerías que falten
       tidyverse,
       glmnet,
       dplyr,
       readr,
       gamlr,
       tidymodels,
       ggplot2,
       scales,
       ggpubr,
       skimr,
       rvest,
       caret,
       stringr,
       boot,
       caret,
       modeest,
       recipes,
       glmnet,
       stargazer,
       pROC)
rm(list = ls()) #Limpia las variables que existan al momento de correr el código
###Base de datos Problem set 2
library(readr)
#Se debe poner el directorio de donde está el script:
#Session-> Set Working directory -> To source file location, para lo cual se debe descargar el repositorioDatos_test_hogares<-readRDS("../Elementos_Guardados/test_hogares.rds") #Guardar las bases de datos
DTEST_P<-data.frame(readRDS("../Elementos_Guardados/test_personas.rds"))  #Guardar las bases de datos
DTEST_H <- data.frame(readRDS("../Elementos_Guardados/test_hogares.rds"))
DTRAIN_H<-data.frame(readRDS("../Elementos_Guardados/train_hogares.rds")) #Guardar las bases de datos
DTRAIN_P<-data.frame(readRDS("../Elementos_Guardados/train_personas.rds"))
summary(DTRAIN_H$Lp)
summary(DTEST_H$Lp)
plot(hist(DTRAIN_H$Lp),main="Distribución Línea Pobreza, train Hogares",
     xlab="Lp",
     ylab="Frecuencia")
plot(hist(DTEST_H$Lp),main="Distribución Línea Pobreza, test Hogares",
     xlab="Lp",
     ylab="Frecuencia")
#Para formular el modelo, es necesario contar con las variables que expliquen la pobreza de un hogar y con el parámetro definido de la línea de pobreza para analizar si los hogares son pobres o no: $257.000
#Vamos a utilizar: #ingtotugarr, Npersug, lp, dominio de la casa, id
#Vamos crear 2 bases de datos, en donde solo se cuenten las variablers de interés.
DaTRAIN_H <- data.frame()
DaTRAIN_H<- subset(DTRAIN_H, select = c("id", "Ingtotugarr", "Npersug", "Lp", "Dominio", "Pobre", "P5090", "P5000"))
DaTEST_H <- data.frame()
DaTEST_H<- subset(DTEST_H, select = c("id", "Npersug", "Lp", "Dominio", "P5090", "P5000"))
#Se procede análisis de las variables:
cantidad_na <- sapply(DaTRAIN_H, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(DaTRAIN_H)
porcentaje_na <-porcentaje_na*100
porcentaje_na #Visualizo el porcentaje de los datos que tienen N
#Se evidencia que la base de datos DaTRAIN_H no cuenta con missing values.
cantidad_na_th <- sapply(DaTEST_H, function(x) sum(is.na(x)))
cantidad_na_th <- data.frame(cantidad_na_th)
porcentaje_na_th <- cantidad_na_th/nrow(DaTEST_H)
porcentaje_na_th <-porcentaje_na_th*100
porcentaje_na_th
#Se evidencia que no existen variables con missing values.
#Análisis de cada variable:
#Descripción Npersug
Npersug<- DaTRAIN_H$Npersug
class(Npersug)
plot(hist(Npersug),col = "blue", main="Histograma Número de personas en la unidad de gasto",
     xlab="Npersug",
     ylab="Frecuencia")
mean(Npersug)
min(Npersug)
max(Npersug)
modeNpersug <- function(Npersug){
  return(as.numeric(names(which.max(table(Npersug)))))
}
modeNpersug(Npersug)
#Descipción Lp
Lp<- DaTRAIN_H$Lp
class(Lp)
plot(hist(Lp),col = "red", main="Histograma Línea de pobreza",
     xlab="Lp",
     ylab="Frecuencia")
min(Lp)
max(Lp)
mean(Lp)
modeLp <- function(Lp){
  return(as.numeric(names(which.max(table(Lp)))))
}
modeLp(Lp)
summary(Lp)
#Descripción Dominio
DOM <- factor(DaTRAIN_H$Dominio)
class(DOM)
summary(DOM)
plot(DOM)

#Mirar como sacar el porcentaje de cada ciudad/ Rural 
#Descripción P5090 = Ocupación de la vivienda habitada
library(tidyverse)
OcViv <- DaTRAIN_H$P5090
class(OcViv)
skim(OcViv)
OcVivl <- factor(OcViv, labels = c("Propia_Pagada", "Propia_Pagando", "Arriendo",
                                        "Usufructo", "Posesión_Sin_Titulo",
                                        "Otra"))
summary(OcVivl)
DaTRAIN_H <- cbind(DaTRAIN_H, OcVivl)
#Categorización P5090 en DaTEST_H
OcVivl <- DaTEST_H$P5090
class(OcVivT)
skim(OcVivT)
OcVivl <- factor(OcVivl, labels = c("Propia_Pagada", "Propia_Pagando", "Arriendo",
                                   "Usufructo", "Posesión_Sin_Titulo",
                                   "Otra"))
summary(OcVivTl)

DaTEST_H<- cbind(DaTEST_H, OcVivl)
#Descripción de P5000
P5000<- DaTRAIN_H$P5000
class(P5000)
plot(hist(P5000),col = "black", main="Histograma No. de cuartos de la vivienda",
     xlab="P5000",
     ylab="Frecuencia")
min(P5000)
max(P5000)
mean(P5000)
modeP5000 <- function(P5000){
  return(as.numeric(names(which.max(table(P5000)))))
}

modeP5000(P5000)
summary(P5000)

###Clasificación
table(DaTRAIN_H$Pobre)

library(stargazer)

DaTRAIN_H<-DaTRAIN_H %>% mutate(Pobre_dummy=factor(Pobre,levels=c(1,0), labels=c("Si", "No")))
table(DaTRAIN_H$Pobre_dummy)

#Se dividirá la base Training personas para obtener las siguientes bases: otra train, mini test y la evaluation para calcular el ROC
require(caret)
set.seed(10101)
Split_1 <- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1)

other_ <- DaTRAIN_H[-Split_1,]
DaTRAIN_H_mini<- DaTRAIN_H[ Split_1,] #Base mini train

set.seed(10101)
Split_2<- createDataPartition(other_$Pobre, p = 1/3) [[1]]
Evaluation_H <- other_[ Split_2,] #Base evaluacion para ROC
Testing_H <- other_[-Split_2,] #Base mini test
#Se comprueba la proporción de la variable  "Pobre" en la base de datos
prop.table(table(DaTRAIN_H_mini$Pobre_dummy)) #Para la base mini train
prop.table(table(Evaluation_H$Pobre_dummy)) 
prop.table(table(Testing_H$Pobre_dummy)) 

#Se realiza el K-fold como método de control del modelo

Varios_parametros<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_modg <- trainControl(method = "cv",
                         number = 5,
                         summaryFunction = Varios_parametros,
                         classProbs = TRUE,
                         verbose=FALSE,
                         savePredictions = T)
#logit
set.seed(10101)
#Se realiza el modelo de clasificacón con la base de control 
logit_caret_modg <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data =DaTRAIN_H_mini ,
  method = "glm", #Para logit
  trControl = ctrl_def_modg,
  family = "binomial",
  preProcess = c("center", "scale"))

logit_caret_modg

#Lambdas para Lasso
lambdas <- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_acc <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data = DaTRAIN_H_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

logit_lasso_acc

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_roc <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data = DaTRAIN_H_mini,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale"))

logit_lasso_roc

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_Resultados <- data.frame(Pobre = Evaluation_H$Pobre_dummy)
Eval_Resultados$Roc <- predict(logit_lasso_roc,
                           newdata = Evaluation_H,
                           type = "prob")[,1]

library(pROC)
#Se calcula el ROC para la regresión
rf_ROC <- roc(Eval_Resultados$Pobre, Eval_Resultados$Roc, levels = rev(levels(Eval_Resultados$Pobre)))

rf_ROC

#Se calcula el Cut off
rf_Thresh <- coords(rf_ROC, x = "best", best.method = "closest.topleft")
rf_Thresh

#Se evalúan los resultados
Eval_Resultados<-Eval_Resultados %>% mutate(hat_def_05=ifelse(Eval_Resultados$Roc>0.5,"Si","No"),
                                    hat_def_rf_Thresh=ifelse(Eval_Resultados$Roc>rf_Thresh$threshold,"Si","No"))

#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_Resultados,table(Pobre,hat_def_05))
#Cuando el threshold es obtenido del ROC
with(Eval_Resultados,table(Pobre,hat_def_rf_Thresh))

#Up-sampling
set.seed(10101)
upSampled_Train_H <- upSample(x = DaTRAIN_H_mini,
                           y = DaTRAIN_H_mini$Pobre_dummy,
                           ## Mantener la variable de clasificación con el mismo nombre:
                           yname = "Pobre_dummy")

dim(upSampled_Train_H)
table(upSampled_Train_H$Pobre_dummy)

set.seed(10101)
logit_lasso_upsample <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data = upSampled_Train_H,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)
logit_lasso_upsample

#Down-sampling
set.seed(10101)
downSampled_Train_H <- downSample(x = DaTRAIN_H_mini,
                               y = DaTRAIN_H_mini$Pobre_dummy,
                               ## keep the class variable name the same:
                               yname = "Pobre_dummy")

table(downSampled_Train_H$Pobre_dummy)

set.seed(10101)
logit_lasso_downsample <- train(
  Pobre_dummy ~Npersug+Lp +factor(OcVivl)+ factor(Dominio)+P5000,
  data = downSampled_Train_H,
  method = "glmnet",
  trControl = ctrl_def_modg,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdas),
  preProcess = c("center", "scale")
)

logit_lasso_downsample

predictors<-c("Npersug","Lp","OcVivl", "Dominio","P5000") 
            
head(DaTRAIN_H_mini[predictors])
testResults <- data.frame(Pobre = Testing_H$Pobre_dummy)
testResults$logit<- predict(logit_caret_modg,
                            newdata = Testing_H, 
                            type = "prob")[,1]
testResults$lasso<- predict(logit_lasso_roc,
                            newdata = Testing_H,
                            type = "prob")[,1]
testResults$lasso_thresh<- predict(logit_lasso_roc,
                                   newdata = Testing_H,
                                   type = "prob")[,1]
testResults$lasso_upsample<- predict(logit_lasso_upsample,
                                     newdata = Testing_H,
                                     type = "prob")[,1]
testResults$mylogit_lasso_downsample<- predict(logit_lasso_downsample,
                                               newdata = Testing_H,
                                               type = "prob")[,1]

testResults<-testResults %>%
  mutate(logit=ifelse(logit>0.5,"Si","No"),
         lasso=ifelse(lasso>0.5,"Si","No"),
         lasso_thresh=ifelse(lasso_thresh>rf_Thresh$threshold,"Si","No"),
         lasso_upsample=ifelse(lasso_upsample>0.5,"Si","No"),
         mylogit_lasso_downsample=ifelse(mylogit_lasso_downsample>0.5,"Si","No")
)

with(testResults,table(Pobre,logit))
with(testResults,table(Pobre,lasso))
with(testResults,table(Pobre,lasso_thresh))
with(testResults,table(Pobre,lasso_upsample))
with(testResults,table(Pobre,mylogit_lasso_downsample))


#Se realiza el mismo procedimiento con diferentes modelos
#Modelo1
model1 <- as.formula("Pobre ~ OcVivl")
#Se dividirá la base Training personas para obtener las siguientes bases: otra train, mini test y la evaluation para calcular el ROC
require(caret)
set.seed(10101)
Split_1Mod1<- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1Mod1)
other_Mod1 <- DaTRAIN_H[-Split_1Mod1,]
DaTRAIN_H_mini_Mod1<- DaTRAIN_H[ Split_1Mod1,] #Base mini train

set.seed(10101)
Split_2Mod1<- createDataPartition(other_Mod1$Pobre, p = 1/3) [[1]]
Evaluation_H_Mod1 <- other_Mod1[ Split_2Mod1,] #Base evaluacion para ROC
Testing_H_Mod1 <- other_Mod1[-Split_2Mod1,] #Base mini test

#Se realiza el K-fold como método de control del modelo
Varios_parametrosMod1<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_Mod1 <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametrosMod1,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)
#logit
set.seed(10101)
#Se realiza el modelo de clasificacón con la base de control 
logit_caret_Mod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data =DaTRAIN_H_mini_Mod1 ,
  method = "glm", #Para logit
  trControl = ctrl_def_Mod1,
  family = "binomial",
  preProcess = c("center", "scale"))
logit_caret_Mod1

#Lambdas para Lasso
lambdasMod1<- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_SensMod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data = DaTRAIN_H_mini_Mod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale"))

logit_lasso_SensMod1

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_rocMod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data = DaTRAIN_H_mini_Mod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale"))

logit_lasso_rocMod1

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_ResultadosMod1 <- data.frame(Pobre = Evaluation_H_Mod1$Pobre_dummy)
Eval_ResultadosMod1$Roc <- predict(logit_lasso_rocMod1,
                               newdata = Evaluation_H_Mod1,
                               type = "prob")[,1]

library(pROC)
#Se calcula el ROC para la regresión
rf_ROCMod1 <- roc(Eval_ResultadosMod1$Pobre, Eval_ResultadosMod1$Roc, levels = rev(levels(Eval_ResultadosMod1$Pobre)))

rf_ROCMod1

#Se calcula el Cut off
rf_ThreshMod1 <- coords(rf_ROCMod1, x = "best", best.method = "closest.topleft")
rf_ThreshMod1

#Se evalúan los resultados
Eval_ResultadosMod1<-Eval_ResultadosMod1 %>% mutate(hat_def_05Mod1=ifelse(Eval_ResultadosMod1$Roc>0.5,"Si","No"),
                                            hat_def_rf_ThreshMod1=ifelse(Eval_ResultadosMod1$Roc>rf_ThreshMod1$threshold,"Si","No"))


#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_ResultadosMod1,table(Pobre,hat_def_05Mod1))
#Cuando el threshold es obtenido del ROC
with(Eval_ResultadosMod1,table(Pobre,hat_def_rf_ThreshMod1))

#Up-sampling
set.seed(10101)
upSampled_Train_HMod1<- upSample(x = DaTRAIN_H_mini_Mod1,
                              y = DaTRAIN_H_mini_Mod1$Pobre_dummy,
                              ## Mantener la variable de clasificación con el mismo nombre:
                              yname = "Pobre_dummy")

dim(upSampled_Train_HMod1)
table(upSampled_Train_HMod1$Pobre_dummy)

set.seed(10101)
logit_lasso_upsampleMod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data = upSampled_Train_HMod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale")
)
logit_lasso_upsampleMod1

#Down-sampling
set.seed(10101)
downSampled_Train_HMod1 <- downSample(x = DaTRAIN_H_mini_Mod1,
                                  y = DaTRAIN_H_mini_Mod1$Pobre_dummy,
                                  ## keep the class variable name the same:
                                  yname = "Pobre_dummy")

table(downSampled_Train_HMod1$Pobre_dummy)

set.seed(10101)
logit_lasso_downsampleMod1 <- train(
  Pobre_dummy ~factor(OcVivl),
  data = downSampled_Train_HMod1,
  method = "glmnet",
  trControl = ctrl_def_Mod1,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod1),
  preProcess = c("center", "scale")
)

logit_lasso_downsampleMod1
predictorsMod1<-c("OcVivl") 

head(DaTRAIN_H_mini_Mod1[predictorsMod1])
testResultsMod1 <- data.frame(Pobre = Testing_H_Mod1$Pobre_dummy)
testResultsMod1$logitm1<- predict(logit_caret_Mod1,
                            newdata = Testing_H_Mod1,
                            type = "prob")[,1]
testResultsMod1$lassom1<- predict(logit_lasso_rocMod1,
                            newdata = Testing_H_Mod1,
                            type = "prob")[,1]
testResultsMod1$lasso_threshm1<- predict(logit_lasso_rocMod1,
                                   newdata = Testing_H_Mod1,
                                   type = "prob")[,1]
testResultsMod1$lasso_upsamplem1<- predict(logit_lasso_upsampleMod1,
                                     newdata = Testing_H_Mod1,
                                     type = "prob")[,1]
testResultsMod1$mylogit_lasso_downsamplem1<- predict(logit_lasso_downsampleMod1,
                                               newdata = Testing_H_Mod1,
                                               type = "prob")[,1]
testResultsMod1<-testResultsMod1 %>%
  mutate(logitm1=ifelse(logitm1>0.5,"Si","No"),
         lassom1=ifelse(lassom1>0.5,"Si","No"),
         lasso_threshm1=ifelse(lasso_threshm1>rf_Thresh$threshold,"Si","No"),
         lasso_upsamplem1=ifelse(lasso_upsamplem1>0.5,"Si","No"),
         lasso_upsamplem1_thres=ifelse(lasso_upsamplem1> rf_Thresh$threshold,"Si","No"),
         mylogit_lasso_downsamplem1_thres=ifelse(mylogit_lasso_downsamplem1>rf_Thresh$threshold,"Si","No")
         )


with(testResultsMod1,table(Pobre,logitm1))
with(testResultsMod1,table(Pobre,lassom1))
with(testResultsMod1,table(Pobre,lasso_threshm1))
with(testResultsMod1,table(Pobre,lasso_upsamplem1))
with(testResultsMod1,table(Pobre,mylogit_lasso_downsamplem1))
xmod1<- model.matrix(~OcVivl, DaTRAIN_H)
ymod1 <- DaTRAIN_H$Pobre
lasso.mod1 <- glmnet(xmod1, ymod1, alpha = 1 , lambda = lambdasMod1)
lasso.mod1$beta
model_log_1 <- stats::glm(model1,family=binomial(link="logit"), data= DaTRAIN_H)
summary(model_log_1)
tidy(model_log_1)
###Prediccion
DaTRAIN_H$PredMod_Log_1 <- stats::predict.glm(model_log_1 , newdata= DaTRAIN_H, type="response")
summary(DaTRAIN_H$PredMod_Log_1)
tidy(PredMod_Log_1) 
mean(DaTRAIN_H$PredMod_Log_1) 
ClasPredMod_Log_1 <- ifelse(DaTRAIN_H$PredMod_Log_1>rf_Thresh$threshold,1,0)
summary(ClasPredMod_Log_1)
head(ClasPredMod_Log_1)
#Clasificación Modelo 1
cm_log1 = confusionMatrix(data= factor(ClasPredMod_Log_1) , 
                         reference= factor(DaTRAIN_H$Pobre) , 
                         mode="sens_spec" , positive="1")
cm_log1
View(logit_lasso_upsampleMod1)
View(logit_lasso_upsampleMod1[["results"]]) 

#Modelo3
model3 <- as.formula("Pobre ~ Lp + OcVivl")
require(caret)
set.seed(10101)
DaTRAIN_H<-DaTRAIN_H %>% mutate(Pobre_dummy=factor(Pobre,levels=c(1,0), labels=c("Si", "No")))

Split_1Mod3<- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1Mod3)
other_Mod3 <- DaTRAIN_H[-Split_1Mod3,]
DaTRAIN_H_mini_Mod3<- DaTRAIN_H[ Split_1Mod3,] #Base mini train
view(DaTRAIN_H_mini_Mod3)
set.seed(10101)
Split_2Mod3<- createDataPartition(other_Mod3$Pobre, p = 1/3) [[1]]
Evaluation_H_Mod3 <- other_Mod3[ Split_2Mod3,] #Base evaluacion para ROC
Testing_H_Mod3 <- other_Mod3[-Split_2Mod3,] #Base mini test

#Se realiza el K-fold como método de control del modelo
Varios_parametrosMod3<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_Mod3 <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametrosMod3,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)
#logit
set.seed(10101)

#Se realiza el modelo de clasificacón con la base de control
logit_caret_Mod3 <- train(
  Pobre_dummy ~Lp + OcVivl,
  data =DaTRAIN_H_mini_Mod3 ,
  method = "glm", #Para logit
  trControl = ctrl_def_Mod3,
  family = "binomial",
  preProcess = c("center", "scale"))
logit_caret_Mod3

#Lambdas para Lasso
lambdasMod3<- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_SensMod3 <- train(
  Pobre_dummy ~Lp + OcVivl,
  data = DaTRAIN_H_mini_Mod3,
  method = "glmnet",
  trControl = ctrl_def_Mod3,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod3),
  preProcess = c("center", "scale"))

logit_lasso_SensMod3

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_rocMod3 <- train(
  Pobre_dummy ~Lp + OcVivl,
  data = DaTRAIN_H_mini_Mod3,
  method = "glmnet",
  trControl = ctrl_def_Mod3,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod3),
  preProcess = c("center", "scale"))

logit_lasso_rocMod3

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_ResultadosMod3 <- data.frame(Pobre = Evaluation_H_Mod3$Pobre_dummy)
Eval_ResultadosMod3$Roc <- predict(logit_lasso_rocMod3,
                                   newdata = Evaluation_H_Mod3,
                                   type = "prob")[,1]

library(pROC)
#Se calcula el ROC para la regresión
rf_ROCMod3 <- roc(Eval_ResultadosMod3$Pobre, Eval_ResultadosMod3$Roc, levels = rev(levels(Eval_ResultadosMod3$Pobre)))

rf_ROCMod3

#Se calcula el Cut off
rf_ThreshMod3 <- coords(rf_ROCMod3, x = "best", best.method = "closest.topleft")
rf_ThreshMod3

#Se evalúan los resultados
Eval_ResultadosMod3<-Eval_ResultadosMod3 %>% mutate(hat_def_05Mod3=ifelse(Eval_ResultadosMod3$Roc>0.5,"Si","No"),
                                                    hat_def_rf_ThreshMod3=ifelse(Eval_ResultadosMod3$Roc>rf_ThreshMod3$threshold,"Si","No"))


#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_ResultadosMod3,table(Pobre,hat_def_05Mod3))
#Cuando el threshold es obtenido del ROC
with(Eval_ResultadosMod3,table(Pobre,hat_def_rf_ThreshMod3))

#Up-sampling
set.seed(10101)
upSampled_Train_HMod3<- upSample(x = DaTRAIN_H_mini_Mod3,
                                 y = DaTRAIN_H_mini_Mod3$Pobre_dummy,
                                 ## Mantener la variable de clasificación con el mismo nombre:
                                 yname = "Pobre_dummy")

dim(upSampled_Train_HMod3)
table(upSampled_Train_HMod3$Pobre_dummy)

set.seed(10101)
logit_lasso_upsampleMod3 <- train(
  Pobre_dummy ~ Lp + OcVivl,
  data = upSampled_Train_HMod3,
  method = "glmnet",
  trControl = ctrl_def_Mod3,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod3),
  preProcess = c("center", "scale")
)
logit_lasso_upsampleMod3

#Down-sampling
set.seed(10101)
downSampled_Train_HMod3 <- downSample(x = DaTRAIN_H_mini_Mod3,
                                      y = DaTRAIN_H_mini_Mod3$Pobre_dummy,
                                      ## keep the class variable name the same:
                                      yname = "Pobre_dummy")

table(downSampled_Train_HMod3$Pobre_dummy)

set.seed(10101)
logit_lasso_downsampleMod3 <- train(
  Pobre_dummy ~Lp + OcVivl,
  data = downSampled_Train_HMod3,
  method = "glmnet",
  trControl = ctrl_def_Mod3,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod3),
  preProcess = c("center", "scale")
)

logit_lasso_downsampleMod3

predictorsMod3<-c(" Lp + OcVivl") 

head(DaTRAIN_H_mini_Mod3[predictorsMod3])


testResultsMod3 <- data.frame(Pobre = Testing_H_Mod3$Pobre_dummy)
testResultsMod3$logitm3<- predict(logit_caret_Mod3,
                                  newdata = Testing_H_Mod3,
                                  type = "prob")[,1]
testResultsMod3$lassom3<- predict(logit_lasso_rocMod3,
                                  newdata = Testing_H_Mod3,
                                  type = "prob")[,1]
testResultsMod3$lasso_threshm3<- predict(logit_lasso_rocMod3,
                                         newdata = Testing_H_Mod3,
                                         type = "prob")[,1]
testResultsMod3$lasso_upsamplem3<- predict(logit_lasso_upsampleMod3,
                                           newdata = Testing_H_Mod3,
                                           type = "prob")[,1]
testResultsMod3$mylogit_lasso_downsamplem3<- predict(logit_lasso_downsampleMod3,
                                                     newdata = Testing_H_Mod3,
                                                     type = "prob")[,1]

testResultsMod3<-testResultsMod3 %>%
  mutate(logitm3=ifelse(logitm3>0.5,"Si","No"),
         lassom3=ifelse(lassom3>0.5,"Si","No"),
         lasso_threshm3=ifelse(lasso_threshm3>rf_ThreshMod3$threshold,"Si","No"),
         lasso_upsamplem3=ifelse(lasso_upsamplem3>0.5,"Si","No"),
         lasso_upsamplem3_thres=ifelse(lasso_upsamplem3 > rf_ThreshMod3$threshold,"Si","No"), 
         mylogit_lasso_downsamplem3=ifelse(mylogit_lasso_downsamplem3>rf_ThreshMod3$threshold,"Si","No")
         )

with(testResultsMod3,table(Pobre,logitm3))
with(testResultsMod3,table(Pobre,lassom3))
with(testResultsMod3,table(Pobre,lasso_threshm3))
with(testResultsMod3,table(Pobre,lasso_upsamplem3))
with(testResultsMod3,table(Pobre,mylogit_lasso_downsamplem3))
with(testResultsMod3,table(Pobre,lasso_upsamplem3_thres))

View(logit_lasso_upsampleMod3[["results"]]) 

Mod_log_3 <- stats::glm(model3,family=binomial(link = "logit"), data= DaTRAIN_H)
tidy(Mod_log_3)
#Predicción
###Prediccion
DaTRAIN_H$PredMod_Log_3 <- stats::predict.glm(Mod_log_3 , newdata= DaTRAIN_H, type="response")
summary(DaTRAIN_H$PredMod_Log_3)
rule3 = mean(DaTRAIN_H$PredMod_Log_3) 
ClasPredMod_Log_3 <- ifelse(DaTRAIN_H$PredMod_Log_3>rf_ThreshMod3$threshold,1,0)
summary(ClasPredMod_Log_3) 
cm_log3 = confusionMatrix(data= factor(ClasPredMod_Log_3) , 
                          reference= factor(DaTRAIN_H$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_log3
#Modelo4
model4 <- as.formula("Pobre ~ P5000 + OcVivl + Dominio")
require(caret)
set.seed(10101)
DaTRAIN_H<-DaTRAIN_H %>% mutate(Pobre_dummy=factor(Pobre,levels=c(1,0), labels=c("Si", "No")))

Split_1Mod4<- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1Mod4)
other_Mod4 <- DaTRAIN_H[-Split_1Mod4,]
DaTRAIN_H_mini_Mod4<- DaTRAIN_H[ Split_1Mod4,] #Base mini train
view(DaTRAIN_H_mini_Mod4)
set.seed(10101)
Split_2Mod4<- createDataPartition(other_Mod4$Pobre, p = 1/3) [[1]]
Evaluation_H_Mod4 <- other_Mod4[ Split_2Mod4,] #Base evaluacion para ROC
Testing_H_Mod4 <- other_Mod4[-Split_2Mod4,] #Base mini test

#Se realiza el K-fold como método de control del modelo
Varios_parametrosMod4<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_Mod4 <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametrosMod4,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)
#logit
set.seed(10101)

#Se realiza el modelo de clasificacón con la base de control
logit_caret_Mod4 <- train(
  Pobre_dummy ~P5000 + OcVivl + Dominio,
  data =DaTRAIN_H_mini_Mod4 ,
  method = "glm", #Para logit
  trControl = ctrl_def_Mod4,
  family = "binomial",
  preProcess = c("center", "scale"))
logit_caret_Mod4

#Lambdas para Lasso
lambdasMod4<- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_SensMod4 <- train(
  Pobre_dummy ~P5000 + OcVivl + Dominio,
  data = DaTRAIN_H_mini_Mod4,
  method = "glmnet",
  trControl = ctrl_def_Mod4,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod4),
  preProcess = c("center", "scale"))

logit_lasso_SensMod4

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_rocMod4 <- train(
  Pobre_dummy ~P5000 + OcVivl + Dominio,
  data = DaTRAIN_H_mini_Mod4,
  method = "glmnet",
  trControl = ctrl_def_Mod4,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod4),
  preProcess = c("center", "scale"))

logit_lasso_rocMod4

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_ResultadosMod4 <- data.frame(Pobre = Evaluation_H_Mod4$Pobre_dummy)
Eval_ResultadosMod4$Roc <- predict(logit_lasso_rocMod4,
                                   newdata = Evaluation_H_Mod4,
                                   type = "prob")[,1]

library(pROC)
#Se calcula el ROC para la regresión
rf_ROCMod4 <- roc(Eval_ResultadosMod4$Pobre, Eval_ResultadosMod4$Roc, levels = rev(levels(Eval_ResultadosMod4$Pobre)))

rf_ROCMod4

#Se calcula el Cut off
rf_ThreshMod4 <- coords(rf_ROCMod4, x = "best", best.method = "closest.topleft")
rf_ThreshMod4

#Se evalúan los resultados
Eval_ResultadosMod4<-Eval_ResultadosMod4 %>% mutate(hat_def_05Mod4=ifelse(Eval_ResultadosMod4$Roc>0.5,"Si","No"),
                                                    hat_def_rf_ThreshMod4=ifelse(Eval_ResultadosMod4$Roc>rf_ThreshMod4$threshold,"Si","No"))


#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_ResultadosMod4,table(Pobre,hat_def_05Mod4))
#Cuando el threshold es obtenido del ROC
with(Eval_ResultadosMod4,table(Pobre,hat_def_rf_ThreshMod4))

#Up-sampling
set.seed(10101)
upSampled_Train_HMod4<- upSample(x = DaTRAIN_H_mini_Mod4,
                                 y = DaTRAIN_H_mini_Mod4$Pobre_dummy,
                                 ## Mantener la variable de clasificación con el mismo nombre:
                                 yname = "Pobre_dummy")

dim(upSampled_Train_HMod4)
table(upSampled_Train_HMod4$Pobre_dummy)

set.seed(10101)
logit_lasso_upsampleMod4 <- train(
  Pobre_dummy ~ P5000 + OcVivl + Dominio,
  data = upSampled_Train_HMod4,
  method = "glmnet",
  trControl = ctrl_def_Mod4,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod4),
  preProcess = c("center", "scale")
)
logit_lasso_upsampleMod4

#Down-sampling
set.seed(10101)
downSampled_Train_HMod4 <- downSample(x = DaTRAIN_H_mini_Mod4,
                                      y = DaTRAIN_H_mini_Mod4$Pobre_dummy,
                                      ## keep the class variable name the same:
                                      yname = "Pobre_dummy")

table(downSampled_Train_HMod4$Pobre_dummy)

set.seed(10101)
logit_lasso_downsampleMod4 <- train(
  Pobre_dummy ~P5000 + OcVivl + Dominio,
  data = downSampled_Train_HMod4,
  method = "glmnet",
  trControl = ctrl_def_Mod4,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod4),
  preProcess = c("center", "scale")
)

logit_lasso_downsampleMod4

predictorsMod4<-c(" P5000 + OcVivl + Dominio") 

head(DaTRAIN_H_mini_Mod4[predictorsMod4])
testResultsMod4 <- data.frame(Pobre = Testing_H_Mod4$Pobre_dummy)
testResultsMod4$logitm4<- predict(logit_caret_Mod4,
                                  newdata = Testing_H_Mod4,
                                  type = "prob")[,1]
testResultsMod4$lassom4<- predict(logit_lasso_rocMod4,
                                  newdata = Testing_H_Mod4,
                                  type = "prob")[,1]
testResultsMod4$lasso_threshm4<- predict(logit_lasso_rocMod4,
                                         newdata = Testing_H_Mod4,
                                         type = "prob")[,1]
testResultsMod4$lasso_upsamplem4<- predict(logit_lasso_upsampleMod4,
                                           newdata = Testing_H_Mod4,
                                           type = "prob")[,1]
testResultsMod4$mylogit_lasso_downsamplem4<- predict(logit_lasso_downsampleMod4,
                                                     newdata = Testing_H_Mod4,
                                                     type = "prob")[,1]

testResultsMod4<-testResultsMod4 %>%
  mutate(logitm4=ifelse(logitm4>0.5,"Si","No"),
         lassom4=ifelse(lassom4>0.5,"Si","No"),
         lasso_threshm4=ifelse(lasso_threshm4>rf_ThreshMod4$threshold,"Si","No"),
         lasso_upsamplem4=ifelse(lasso_upsamplem4>rf_ThreshMod4$threshold,"Si","No"),
         mylogit_lasso_downsamplem4=ifelse(mylogit_lasso_downsamplem4>rf_ThreshMod4$threshold,"Si","No")) 


with(testResultsMod4,table(Pobre,logitm4))
with(testResultsMod4,table(Pobre,lassom4)) 
with(testResultsMod4,table(Pobre,lasso_threshm4))
with(testResultsMod4,table(Pobre,lasso_upsamplem4))
with(testResultsMod4,table(Pobre,mylogit_lasso_downsamplem4))

View(logit_lasso_upsampleMod4[["results"]]) 
View(logit_lasso_downsampleMod4[["results"]]) 
Mod_log_4 <- stats::glm(model4,family=binomial(link = "logit"), data= DaTRAIN_H)
#Predicción
###Prediccion
model4
DaTRAIN_H$PredMod_Log_4 <- stats::predict.glm(Mod_log_4 , newdata= DaTRAIN_H, type="response")
summary(DaTRAIN_H$PredMod_Log_4)
ClasPredMod_Log_4 <- ifelse(DaTRAIN_H$PredMod_Log_4>rf_ThreshMod4$threshold,1,0)
summary(ClasPredMod_Log_4) 
cm_log4 =  confusionMatrix(data= factor(ClasPredMod_Log_4), 
                          reference= factor(DaTRAIN_H$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_log4
#MODEL 5
model5 <- as.formula("Pobre ~ P5000 + OcVivl")
require(caret)
set.seed(10101)
DaTRAIN_H<-DaTRAIN_H %>% mutate(Pobre_dummy=factor(Pobre,levels=c(1,0), labels=c("Si", "No")))

Split_1Mod5<- createDataPartition(DaTRAIN_H$Pobre, p = .7) [[1]]
length(Split_1Mod5)
other_Mod5 <- DaTRAIN_H[-Split_1Mod5,]
DaTRAIN_H_mini_Mod5<- DaTRAIN_H[ Split_1Mod5,] #Base mini train
view(DaTRAIN_H_mini_Mod5)
set.seed(10101)
Split_2Mod5<- createDataPartition(other_Mod5$Pobre, p = 1/3) [[1]]
Evaluation_H_Mod5 <- other_Mod5[ Split_2Mod5,] #Base evaluacion para ROC
Testing_H_Mod5 <- other_Mod5[-Split_2Mod5,] #Base mini test

#Se analizan las proporciones de la varibale "Pobre" en la base de datos
prop.table(table(DaTRAIN_H_mini_Mod5$Pobre_dummy)) #Para la base mini train
prop.table(table(Evaluation_H_Mod5$Pobre_dummy)) 
prop.table(table(Testing_H_Mod5$Pobre_dummy)) 

#Se realiza el K-fold como método de control del modelo
Varios_parametrosMod5<-function(...)c(twoClassSummary(...), defaultSummary(...))

ctrl_def_Mod5 <- trainControl(method = "cv",
                              number = 5,
                              summaryFunction = Varios_parametrosMod5,
                              classProbs = TRUE,
                              verbose=FALSE,
                              savePredictions = T)
#logit
set.seed(10101)

#Se realiza el modelo de clasificacón con la base de control
logit_caret_Mod5 <- train(
  Pobre_dummy ~P5000 + OcVivl,
  data =DaTRAIN_H_mini_Mod5 ,
  method = "glm", #Para logit
  trControl = ctrl_def_Mod5,
  family = "binomial",
  preProcess = c("center", "scale"))
logit_caret_Mod5

#Lambdas para Lasso
lambdasMod5<- 10^seq(-4, 0.01, length = 200)

#Ahora, se hará la prueba tomando como métrica la Sensibilidad
set.seed(10101)
logit_lasso_SensMod5 <- train(
  Pobre_dummy ~P5000 + OcVivl,
  data = DaTRAIN_H_mini_Mod5,
  method = "glmnet",
  trControl = ctrl_def_Mod5,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod5),
  preProcess = c("center", "scale"))

logit_lasso_SensMod5

#Ahora, se hará la prueba tomando como métrica el ROC

set.seed(10101)
logit_lasso_rocMod5 <- train(
  Pobre_dummy ~P5000 + OcVivl,
  data = DaTRAIN_H_mini_Mod5,
  method = "glmnet",
  trControl = ctrl_def_Mod5,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod5),
  preProcess = c("center", "scale"))

logit_lasso_rocMod5

#Calcularemos la regla para realizar la clasificación (Cut off)

Eval_ResultadosMod5 <- data.frame(Pobre = Evaluation_H_Mod5$Pobre_dummy)
Eval_ResultadosMod5$Roc <- predict(logit_lasso_rocMod5,
                                   newdata = Evaluation_H_Mod5,
                                   type = "prob")[,1]

library(pROC)
#Se calcula el ROC para la regresión
rf_ROCMod5 <- roc(Eval_ResultadosMod5$Pobre, Eval_ResultadosMod5$Roc, levels = rev(levels(Eval_ResultadosMod5$Pobre)))

rf_ROCMod5

#Se calcula el Cut off
rf_ThreshMod5 <- coords(rf_ROCMod5, x = "best", best.method = "closest.topleft")
rf_ThreshMod5

#Se evalúan los resultados
Eval_ResultadosMod5<-Eval_ResultadosMod5 %>% mutate(hat_def_05Mod5=ifelse(Eval_ResultadosMod5$Roc>0.5,"Si","No"),
                                                    hat_def_rf_ThreshMod5=ifelse(Eval_ResultadosMod5$Roc>rf_ThreshMod5$threshold,"Si","No"))


#Cuando el threshold es igual a 0.5 (regla de Bayes)
with(Eval_ResultadosMod5,table(Pobre,hat_def_05Mod5))
#Cuando el threshold es obtenido del ROC
with(Eval_ResultadosMod5,table(Pobre,hat_def_rf_ThreshMod5))

#Up-sampling
set.seed(10101)
upSampled_Train_HMod5<- upSample(x = DaTRAIN_H_mini_Mod5,
                                 y = DaTRAIN_H_mini_Mod5$Pobre_dummy,
                                 ## Mantener la variable de clasificación con el mismo nombre:
                                 yname = "Pobre_dummy")

dim(upSampled_Train_HMod5)
table(upSampled_Train_HMod5$Pobre_dummy)

set.seed(10101)
logit_lasso_upsampleMod5<- train(
  Pobre_dummy ~ P5000 + OcVivl,
  data = upSampled_Train_HMod5,
  method = "glmnet",
  trControl = ctrl_def_Mod5,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod5),
  preProcess = c("center", "scale")
)
logit_lasso_upsampleMod5

#Down-sampling
set.seed(10101)
downSampled_Train_HMod5 <- downSample(x = DaTRAIN_H_mini_Mod5,
                                      y = DaTRAIN_H_mini_Mod5$Pobre_dummy,
                                      ## keep the class variable name the same:
                                      yname = "Pobre_dummy")

table(downSampled_Train_HMod5$Pobre_dummy)

set.seed(10101)
logit_lasso_downsampleMod5 <- train(
  Pobre_dummy ~P5000 + OcVivl,
  data = downSampled_Train_HMod5,
  method = "glmnet",
  trControl = ctrl_def_Mod5,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=lambdasMod5),
  preProcess = c("center", "scale")
)

logit_lasso_downsampleMod5

predictorsMod5<-c(" P5000 + OcVivl ") 
model5
head(DaTRAIN_H_mini_Mod5[predictorsMod5])
testResultsMod5<- data.frame(Pobre = Testing_H_Mod5$Pobre_dummy)
testResultsMod5$logitm5<- predict(logit_caret_Mod5,
                                  newdata = Testing_H_Mod5,
                                  type = "prob")[,1]
testResultsMod5$lassom5<- predict(logit_lasso_rocMod5,
                                  newdata = Testing_H_Mod5,
                                  type = "prob")[,1]
testResultsMod5$lasso_threshm5<- predict(logit_lasso_rocMod5,
                                         newdata = Testing_H_Mod5,
                                         type = "prob")[,1]
testResultsMod5$lasso_upsamplem5<- predict(logit_lasso_upsampleMod5,
                                           newdata = Testing_H_Mod5,
                                           type = "prob")[,1]
testResultsMod5$mylogit_lasso_downsamplem5<- predict(logit_lasso_downsampleMod5,
                                                     newdata = Testing_H_Mod5,
                                                     type = "prob")[,1]
testResultsMod5<-testResultsMod5 %>%
  mutate(logitm5=ifelse(logitm5>0.5,"Si","No"),
         lassom5=ifelse(lassom5>0.5,"Si","No"),
         lasso_threshm5=ifelse(lasso_threshm5>rf_ThreshMod5$threshold,"Si","No"),
         lasso_upsamplem5=ifelse(lasso_upsamplem5>rf_ThreshMod5$threshold,"Si","No"),
         mylogit_lasso_downsamplem5=ifelse(mylogit_lasso_downsamplem5>rf_ThreshMod5$threshold,"Si","No")
  )

with(testResultsMod5,table(Pobre,logitm5))
with(testResultsMod5,table(Pobre,lassom5))
with(testResultsMod5,table(Pobre,lasso_threshm5))
with(testResultsMod5,table(Pobre,lasso_upsamplem5))
with(testResultsMod5,table(Pobre,mylogit_lasso_downsamplem5))
Mod_log_5 <- stats::glm(model5,family=binomial(link = "logit"), data= DaTRAIN_H)
tidy(Mod_log_5)
View(logit_lasso_upsampleMod5[["results"]]) 
#Predicción
###Prediccion
DaTRAIN_H$PredMod_Log_5 <- stats::predict.glm(Mod_log_5 , newdata= DaTRAIN_H, type="response")
summary(DaTRAIN_H$PredMod_Log_5)
head(DaTRAIN_H$PredMod_Log_5)
head(DaTRAIN_H$Pobre)
ClasPredMod_Log_5 <- ifelse(DaTRAIN_H$PredMod_Log_5>rf_ThreshMod5$threshold,1,0)
summary(ClasPredMod_Log_5) 
cm_log5 = confusionMatrix(data= factor(ClasPredMod_Log_5) , 
                          reference= factor(DaTRAIN_H$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_log5
head(DaTRAIN_H$PredMod_Log_5)
tail(DaTRAIN_H$PredMod_Log_5)
head(ClasPredMod_Log_5)
tail(ClasPredMod_Log_5)
tail(DaTRAIN_H$Pobre)
head(DaTRAIN_H$Pobre)
library(scales)
#Predicción 
##TEST FINAL 
testResultsModFinal<- predict(logit_lasso_upsampleMod4,
                              newdata = DaTEST_H,
                              type = "prob")[,1]

TestResulstFinal_1<- ifelse(testResultsModFinal>rf_ThreshMod4$threshold,"Si","No")
TestResulstFinal<- ifelse(testResultsModFinal>rf_ThreshMod4$threshold,"1","0")
View(TestResulstFinal)
stargazer(TestResulstFinal_1)


TestResulstFinal_1 <- cbind(DaTEST_H$id , TestResulstFinal)
TestResultsMod1Tabla<- table(TestResulstFinal_1)
  View(TestResulstFinal_1)
write.csv (TestResulstFinal_1, "../Elementos_Guardados/predictions_beleno_gaona_c3_r116.csv")
write.csv(TestResulstFinal, "../Elementos_Guardados/predictionsmod_beleno_gaona_c3_r116.csv")
write.csv(TestResultsMod1Tabla, "../Elementos_Guardados/predictionsTabla_beleno_gaona_c3_r116.csv")



##===============================================================================
##===================Modelo de regresión=========================================
##==================================================================================


#Se realizará la división del data set de "training" en 2, una base de mini training y otra para mini test, con el fin de poder calcular
#los MSE en términos de una variable predicha.
DTRAIN_HR <- data.frame()
DTRAIN_HR<- subset(DTRAIN_H, select = c("id", "Ingtotugarr","Ingtotug", "Ingpcug", "Npersug", "Lp", "Dominio", "Pobre", "P5090", "P5000"))
DTEST_HR<- data.frame()
DTEST_HR<- subset(DTEST_H, select = c("id", "Npersug", "Lp", "Dominio", "P5090", "P5000"))
DTEST_PR <- data.frame()
DTEST_PR<- subset(DTEST_P, select = c("id", "P6210", "P6020", "P6040", "Dominio", "Oficio"))
DTRAIN_PR<- data.frame()
DTRAIN_PR<- subset(DTRAIN_P, select = c("id", "P6210", "P6020", "P6040", "Dominio", "Oficio", "Ingtot"))

cantidad_naR <- sapply(DTRAIN_HR, function(x) sum(is.na(x)))
cantidad_naR <- data.frame(cantidad_naR)
porcentaje_naR <- cantidad_naR/nrow(DTRAIN_HR)
porcentaje_naR <-porcentaje_naR*100
porcentaje_naR 
cantidad_naPR <- sapply(DTRAIN_PR, function(x) sum(is.na(x)))
cantidad_naPR <- data.frame(cantidad_naPR)
porcentaje_naPR <- cantidad_naPR/nrow(DTRAIN_PR)
porcentaje_naPR <-porcentaje_naPR*100
porcentaje_naPR 

DTRAIN_PR$Oficio[is.na(DTRAIN_PR$Oficio)] = 0 #Se imputa 0a los NA, esta clasificación es que no tienen oficio
DTRAIN_PR$P6210[is.na(DTRAIN_PR$P6210)] = 9 #Se imputa 9 a los NA, para determinar que no es claro el nivel educativo
DTRAIN_PR$Ingtot[is.na(DTRAIN_PR$Ingtot)] = 0 #Se imputa 0 a los NA, que no reciben ingresos

cantidad_naPRT <- sapply(DTEST_PR, function(x) sum(is.na(x)))
cantidad_naPRT <- data.frame(cantidad_naPRT)
porcentaje_naPRT <- cantidad_naPRT/nrow(DTEST_PR)
porcentaje_naPRT <-porcentaje_naPRT*100
porcentaje_naPRT 

DTEST_PR$Oficio[is.na(DTEST_PR$Oficio)] = 0 #Se imputa 0a los NA, esta clasificación es que no tienen oficio
DTEST_PR$P6210[is.na(DTEST_PR$P6210)] = 9 #Se imputa 9 a los NA, para determinar que no es claro el nivel educativo


#Descripción
Edad <- DTRAIN_PR$P6040
Sexo <- DTRAIN_PR$P6020
Educ <- DTRAIN_PR$P6210
View(DTRAIN_PR)
DTRAIN_PR <- cbind(DTRAIN_PR, Sexo, Educ, Edad)
DTRAIN_PR <- DTRAIN_PR%>% mutate(Edad2=Edad^2)
View(DTRAIN_PR)

DTRAIN_PR<- DTRAIN_PR %>% mutate(Ocup=ifelse(DTRAIN_PR$Oficio>0,1,0)) #Se crea la variable Ocup para establecer si las personas se encuentran  #con algún oficio. Como se imputó cero para los NA de esta variable, se asume que no tienen ninguna ocupación

DTRAIN_PR <- DTRAIN_PR %>% #Se vuelven categóricas las variables que así lo sean en la BD
  mutate_at(.vars = c(
    "Sexo", "Educ", "Dominio", "Oficio", "Ocup"),
    .funs = factor)                                                                   

#Descripción de las variables a usar en el modelo del train
min(Edad)
max(Edad)
mean(Edad)

class(Sexo)
levels(Sexo)
summary(Sexo)
table(Sexo)

class(Educ)
levels(Educ)
summary(Educ)
table(Educ)

modeEduc <- function(Npersug){
  return(as.numeric(names(which.max(table(Npersug)))))
}
modeEduc(Educ)




rm(Sexo, Edad, Educ) #Se borran para no generar duplicación de valores, en caso tal
Sexo <- DTEST_PR$P6020
Edad<- DTEST_PR$P6040
Educ <- DTEST_PR$P6210
DTEST_PR <- cbind(DTEST_PR, Edad, Sexo, Educ)
DTEST_PR <- DTEST_PR%>% mutate(Edad2=Edad^2)
View(DTEST_PR)

DTEST_PR<- DTEST_PR %>% mutate(Ocup=ifelse(DTEST_PR$Oficio>0,1,0))


DTEST_PR <- DTEST_PR %>% #Se vuelven categóricas las variables que así lo sean en la BD
  mutate_at(.vars = c(
    "Sexo", "Educ", "Dominio", "Oficio", "Ocup"),
    .funs = factor)

#Descripción de las variables a usar en el modelo del test
min(Edad)
max(Edad)
mean(Edad)

class(Sexo)
levels(Sexo)
summary(Sexo)
table(Sexo)

class(Educ)
levels(Educ)
summary(Educ)
table(Educ)

modeEduc <- function(Npersug){
  return(as.numeric(names(which.max(table(Npersug)))))
}
modeEduc(Educ)


#Análisis de Ingtotugarr e Ingotupcg

summary(DTRAIN_H$Ingtotugarr)
summary(DTRAIN_H$Ingtotug)



#====================================================================================================
#====================================================================================================

#Se harán las regresiones anteriores teniendo en cuenta la base de Train completa (sin subdividirla). Se graficarán los MSE para el train.

#Modelo 1 
model_base_10 <- lm(Ingtot ~ Edad + Edad2 + factor(Sexo) + factor(Educ) + factor (Oficio) + factor (Dominio), data =DTRAIN_PR)
#model_base_10 <- lm(Ingtot ~ Edad + Edad2 + factor(Sexo) + factor(Educ) + factor (Ocup) + factor (Dominio), data =DTRAIN_PR)
predicciones_train_mb10 <- predict(model_base_10, newdata = DTRAIN_PR)

# MSE de entrenamiento para modelo 1 (base completa de train)

training_mse_mb10 <- mean((predicciones_train_mb10 - DTRAIN_PR$Ingtot)^2)
paste("Error (mse) de entrenamiento modelo 1:", training_mse_mb10)
stargazer(model_base_10, type="text")
#====================================================================================================
#Modelo 2

x_subtrain_P_full <- model.matrix(Ingtot~ Edad + Edad2 + factor(Sexo) + factor(Educ) + factor (Oficio) + factor (Dominio), data = DTRAIN_PR)[, -1]
#x_subtrain_P_full <- model.matrix(Ingtot~ Edad + Edad2 + factor(Sexo) + factor(Educ) + factor (Ocup) + factor (Dominio), data = DTRAIN_PR)[, -1]
y_subtrain_P_full <- DTRAIN_PR$Ingtot

modelobase1Ri_f <- glmnet(
  x           = x_subtrain_P_full,
  y           = y_subtrain_P_full,
  alpha       = 0,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_errormod1Ri_f <- cv.glmnet(
  x           = x_subtrain_P_full,
  y           = y_subtrain_P_full,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
modelo_1_Ridge_f <- glmnet(
  x           = x_subtrain_P_full,
  y           = y_subtrain_P_full,
  alpha       = 0,
  lambda      = cv_errormod1Ri_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mb1ri_f <- predict(modelo_1_Ridge_f, newx = x_subtrain_P_full)

# MSE de entrenamiento

training_mse_mb1ri_f <- mean((predicciones_train_mb1ri_f - y_subtrain_P_full)^2)
paste("Error (mse) de entrenamiento modelo 2:", training_mse_mb1ri_f)

#Modelo 3

modelobase1lass_f <- glmnet(
  x           = x_subtrain_P_full,
  y           = y_subtrain_P_full,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_m1_Lass_f <- cv.glmnet(
  x      = x_subtrain_P_full,
  y      = y_subtrain_P_full,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


modelo1_Lasso_f <- glmnet(
  x           = x_subtrain_P_full,
  y           = y_subtrain_P_full,
  alpha       = 1,
  lambda      = cv_error_m1_Lass_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mod1_Lass_f <- predict(modelo1_Lasso_f, newx = x_subtrain_P_full)

training_mse_mod1_Lass_f <- mean((predicciones_train_mod1_Lass_f - y_subtrain_P_full)^2)
print(paste("Error (mse) de entrenamiento modelo 3:", training_mse_mod1_Lass_f ))

#Modelo 4

x_subtrain_P_mod2_f <- model.matrix(Ingtot~ Edad + Edad2 + factor(Sexo) + factor(Educ), data = DTRAIN_PR)[, -1]
y_subtrain_P_mod2_f <- DTRAIN_PR$Ingtot

modelobase2Ri_f <- glmnet(
  x           = x_subtrain_P_mod2_f,
  y           = y_subtrain_P_mod2_f,
  alpha       = 0,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_errormod2Ri_f <- cv.glmnet(
  x           = x_subtrain_P_mod2_f,
  y           = y_subtrain_P_mod2_f,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
modelo_2_Ridge_f <- glmnet(
  x           = x_subtrain_P_mod2_f,
  y           = y_subtrain_P_mod2_f,
  alpha       = 0,
  lambda      = cv_errormod2Ri_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mb2ri_f <- predict(modelo_2_Ridge_f, newx = x_subtrain_P_mod2_f)

# MSE de entrenamiento

training_mse_mb2ri_f <- mean((predicciones_train_mb2ri_f - y_subtrain_P_mod2_f)^2)
paste("Error (mse) de entrenamiento modelo 4:", training_mse_mb2ri_f)

#Modelo 5

modelobase2lass_f <- glmnet(
  x           = x_subtrain_P_mod2_f,
  y           = y_subtrain_P_mod2_f,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_m2_Lass_f <- cv.glmnet(
  x      = x_subtrain_P_mod2_f,
  y      = y_subtrain_P_mod2_f,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


modelo2_Lasso_f <- glmnet(
  x           = x_subtrain_P_mod2_f,
  y           = y_subtrain_P_mod2_f,
  alpha       = 1,
  lambda      = cv_error_m1_Lass_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mod2_Lass_f <- predict(modelo2_Lasso_f, newx = x_subtrain_P_mod2_f)

training_mse_mod2_Lass_f <- mean((predicciones_train_mod2_Lass_f - y_subtrain_P_mod2_f)^2)
print(paste("Error (mse) de entrenamiento modelo 5:", training_mse_mod2_Lass_f ))

#Modelo 6

x_subtrain_P_mod3_f <- model.matrix(Ingtot~ Edad + Edad2 + factor(Sexo) + factor(Educ)+factor (Oficio), data = DTRAIN_PR)[, -1]
#x_subtrain_P_mod3_f <- model.matrix(Ingtot~ Edad + Edad2 + factor(Sexo) + factor(Educ)+factor (Ocup), data = DTRAIN_PR)[, -1]
y_subtrain_P_mod3_f <- DTRAIN_PR$Ingtot


modelobase3Ri_f <- glmnet(
  x           = x_subtrain_P_mod3_f ,
  y           = y_subtrain_P_mod3_f,
  alpha       = 0,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_errormod3Ri_f <- cv.glmnet(
  x           = x_subtrain_P_mod3_f ,
  y           = y_subtrain_P_mod3_f,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
modelo_3_Ridge_f <- glmnet(
  x           = x_subtrain_P_mod3_f ,
  y           = y_subtrain_P_mod3_f,
  alpha       = 0,
  lambda      = cv_errormod3Ri_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mb3ri_f <- predict(modelo_3_Ridge_f, newx = x_subtrain_P_mod3_f )

# MSE de entrenamiento

training_mse_mb3ri_f <- mean((predicciones_train_mb3ri_f - y_subtrain_P_mod3_f)^2)
paste("Error (mse) de entrenamiento modelo 6:", training_mse_mb3ri_f)


#Modelo 7
modelobase3lass_f <- glmnet(
  x           = x_subtrain_P_mod3_f,
  y           = y_subtrain_P_mod3_f,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_m3_Lass_f <- cv.glmnet(
  x      = x_subtrain_P_mod3_f,
  y      = y_subtrain_P_mod3_f,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


modelo3_Lasso_f <- glmnet(
  x           = x_subtrain_P_mod3_f,
  y           = y_subtrain_P_mod3_f,
  alpha       = 1,
  lambda      = cv_error_m3_Lass_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mod3_Lass_f <- predict(modelo3_Lasso_f, newx = x_subtrain_P_mod3_f)

training_mse_mod3_Lass_f <- mean((predicciones_train_mod3_Lass_f - y_subtrain_P_mod3_f)^2)
print(paste("Error (mse) de entrenamiento modelo 7:", training_mse_mod3_Lass_f ))


#Para el modelo 8
# ==============================================================================
x_subtrain_P_mod4_f <- model.matrix(Ingtot~ Edad + Edad2 + factor(Sexo), data = DTRAIN_PR)[, -1]
y_subtrain_P_mod4_f <- DTRAIN_PR$Ingtot

modelobase4Ri_f <- glmnet(
  x           = x_subtrain_P_mod4_f,
  y           = y_subtrain_P_mod4_f,
  alpha       = 0,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_errormod4Ri_f <- cv.glmnet(
  x           = x_subtrain_P_mod4_f ,
  y           = y_subtrain_P_mod4_f,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
modelo_4_Ridge_f <- glmnet(
  x           = x_subtrain_P_mod4_f ,
  y           = y_subtrain_P_mod4_f,
  alpha       = 0,
  lambda      = cv_errormod4Ri_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mb4ri_f <- predict(modelo_4_Ridge_f, newx = x_subtrain_P_mod4_f )

# MSE de entrenamiento

training_mse_mb4ri_f <- mean((predicciones_train_mb4ri_f -y_subtrain_P_mod4_f)^2)
paste("Error (mse) de entrenamiento modelo 8:", training_mse_mb4ri_f)

#Modelo 9

modelobase4lass_f <- glmnet(
  x           = x_subtrain_P_mod4_f,
  y           = y_subtrain_P_mod4_f,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)



set.seed(10101)
cv_error_m4_Lass_f <- cv.glmnet(
  x      = x_subtrain_P_mod4_f,
  y      = y_subtrain_P_mod4_f,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)



paste("Mejor valor de lambda encontrado para modelo 9:", cv_error_m4_Lass_f$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviación estándar para modelo 9:", cv_error_m4_Lass_f$lambda.1se)

# Mejor modelo lambda óptimo + 1sd

modelo4_Lasso_f <- glmnet(
  x           = x_subtrain_P_mod4_f,
  y           = y_subtrain_P_mod4_f,
  alpha       = 1,
  lambda      = cv_error_m4_Lass_f$lambda.1se,
  standardize = TRUE
)




# Predicciones de entrenamiento modelo 9

predicciones_train_mod4_Lass_f <- predict(modelo4_Lasso_f, newx = x_subtrain_P_mod4_f)

# MSE de entrenamiento modelo 9

training_mse_mod4_Lass_f <- mean((predicciones_train_mod4_Lass_f - y_subtrain_P_mod4_f)^2)
print(paste("Error (mse) de entrenamiento modelo 9:", training_mse_mod4_Lass_f ))



MSE_modelos<-c(training_mse_mb10, training_mse_mb1ri_f, training_mse_mod1_Lass_f, training_mse_mb2ri_f, training_mse_mod2_Lass_f, training_mse_mb3ri_f, training_mse_mod3_Lass_f, training_mse_mb4ri_f, training_mse_mod4_Lass_f)
modelos_<-c('modelo1','modelo 2', 'modelo3', 'modelo 4', 'modelo5','modelo6','modelo7','modelo8', 'modelo9')
MSE_errores<-data.frame(modelos_,MSE_modelos)

#Se grafica el resultado
ggplot(data=MSE_errores, aes(x = modelos_, y = MSE_modelos, group=1)) + 
  geom_line()+   geom_point()+  labs(title = "Comparación MSE diferentes modelos en términos de MSE (con base training)") 

#Se realiza la confussion matrix para los 9 modelos

#=========================================================================================================================
#Modelo 1

Datos_prediccion_train_1<-data.frame(DTRAIN_PR$id,predicciones_train_mb10)

colnames(Datos_prediccion_train_1) <- c('id','Ingtot_predicted')

sum_ingresos_hogar_1<-Datos_prediccion_train_1 %>% group_by(id) %>% summarize(Ingtot_hogar1=sum(Ingtot_predicted,na.rm = TRUE)) 

#Se asume que la suma del ingreso individual del hogar es aproximadamente igual al Ingtotugarr, al ingreso con imputación de arriendo, teniendo
#en cuenta que la mayoría de hogares no presenta dichos arriendos y son casi iguales.

DTRAIN_HR<-left_join(DTRAIN_HR, sum_ingresos_hogar_1) #Se une la variable sum_ingresos_hogar a la base train


DTRAIN_HR<- DTRAIN_HR %>% mutate(Pobre_predicho_1=ifelse(DTRAIN_HR$Ingtot_hogar1<Lp*Npersug,1,0)) #Se realiza la clasificación final

#Se calcula la "Confussion matrix" para determinar el nivel de accuracy y Sensitivity
CM_Reg_train_1 = confusionMatrix(data= factor(DTRAIN_HR$Pobre_predicho_1) , 
                               reference= factor(DTRAIN_HR$Pobre) , 
                               mode="sens_spec" , positive="1")
CM_Reg_train_1

#Se revisa la proporción de Pobres (1) y No Pobres (0)
prop.table(table(DTRAIN_HR$Pobre)) 
prop.table(table(DTRAIN_HR$Pobre_predicho_1)) 

#Se obtiene información descriptiva de las variables
summary (DTRAIN_HR$Pobre)
summary (DTRAIN_HR$Pobre_predicho_1)

#=========================================================================================================================
#Modelo 2

Datos_prediccion_train_2<-data.frame(DTRAIN_PR$id,predicciones_train_mb1ri_f)

colnames(Datos_prediccion_train_2) <- c('id','Ingtot_predicted')

sum_ingresos_hogar_2<-Datos_prediccion_train_2 %>% group_by(id) %>% summarize(Ingtot_hogar2=sum(Ingtot_predicted,na.rm = TRUE)) 

#Se asume que la suma del ingreso individual del hogar es aproximadamente igual al Ingtotugarr, al ingreso con imputación de arriendo, teniendo
#en cuenta que la mayoría de hogares no presenta dichos arriendos y son casi iguales.

DTRAIN_HR<-left_join(DTRAIN_HR, sum_ingresos_hogar_2) #Se une la variable sum_ingresos_hogar a la base train


DTRAIN_HR<- DTRAIN_HR %>% mutate(Pobre_predicho_2=ifelse(DTRAIN_HR$Ingtot_hogar2<Lp*Npersug,1,0)) #Se realiza la clasificación final

#Se calcula la "Confussion matrix" para determinar el nivel de accuracy y Sensitivity
CM_Reg_train_2 = confusionMatrix(data= factor(DTRAIN_HR$Pobre_predicho_2) , 
                                 reference= factor(DTRAIN_HR$Pobre) , 
                                 mode="sens_spec" , positive="1")
CM_Reg_train_2

#Se revisa la proporción de Pobres (1) y No Pobres (0)
prop.table(table(DTRAIN_HR$Pobre)) 
prop.table(table(DTRAIN_HR$Pobre_predicho_2)) 

#Se obtiene información descriptiva de las variables
summary (DTRAIN_HR$Pobre)
summary (DTRAIN_HR$Pobre_predicho_2)

#=========================================================================================================================
#Modelo 3

Datos_prediccion_train_3<-data.frame(DTRAIN_PR$id,predicciones_train_mod1_Lass_f)

colnames(Datos_prediccion_train_3) <- c('id','Ingtot_predicted')

sum_ingresos_hogar_3<-Datos_prediccion_train_3 %>% group_by(id) %>% summarize(Ingtot_hogar3=sum(Ingtot_predicted,na.rm = TRUE)) 

#Se asume que la suma del ingreso individual del hogar es aproximadamente igual al Ingtotugarr, al ingreso con imputación de arriendo, teniendo
#en cuenta que la mayoría de hogares no presenta dichos arriendos y son casi iguales.

DTRAIN_HR<-left_join(DTRAIN_HR, sum_ingresos_hogar_3) #Se une la variable sum_ingresos_hogar a la base train


DTRAIN_HR<- DTRAIN_HR %>% mutate(Pobre_predicho_3=ifelse(DTRAIN_HR$Ingtot_hogar3<Lp*Npersug,1,0)) #Se realiza la clasificación final

#Se calcula la "Confussion matrix" para determinar el nivel de accuracy y Sensitivity
CM_Reg_train_3 = confusionMatrix(data= factor(DTRAIN_HR$Pobre_predicho_3) , 
                                 reference= factor(DTRAIN_HR$Pobre) , 
                                 mode="sens_spec" , positive="1")
CM_Reg_train_3

#Se revisa la proporción de Pobres (1) y No Pobres (0)
prop.table(table(DTRAIN_HR$Pobre)) 
prop.table(table(DTRAIN_HR$Pobre_predicho_3)) 

#Se obtiene información descriptiva de las variables
summary (DTRAIN_HR$Pobre)
summary (DTRAIN_HR$Pobre_predicho_3)

#=========================================================================================================================
#Modelo 4

Datos_prediccion_train_4<-data.frame(DTRAIN_PR$id,training_mse_mb2ri_f)

colnames(Datos_prediccion_train_4) <- c('id','Ingtot_predicted')

sum_ingresos_hogar_4<-Datos_prediccion_train_4 %>% group_by(id) %>% summarize(Ingtot_hogar4=sum(Ingtot_predicted,na.rm = TRUE)) 

#Se asume que la suma del ingreso individual del hogar es aproximadamente igual al Ingtotugarr, al ingreso con imputación de arriendo, teniendo
#en cuenta que la mayoría de hogares no presenta dichos arriendos y son casi iguales.

DTRAIN_HR<-left_join(DTRAIN_HR, sum_ingresos_hogar_4) #Se une la variable sum_ingresos_hogar a la base train


DTRAIN_HR<- DTRAIN_HR %>% mutate(Pobre_predicho_4=ifelse(DTRAIN_HR$Ingtot_hogar4<Lp*Npersug,1,0)) #Se realiza la clasificación final

#Se calcula la "Confussion matrix" para determinar el nivel de accuracy y Sensitivity
CM_Reg_train_4 = confusionMatrix(data= factor(DTRAIN_HR$Pobre_predicho_4) , 
                                 reference= factor(DTRAIN_HR$Pobre) , 
                                 mode="sens_spec" , positive="1")
CM_Reg_train_4

#Se revisa la proporción de Pobres (1) y No Pobres (0)
prop.table(table(DTRAIN_HR$Pobre)) 
prop.table(table(DTRAIN_HR$Pobre_predicho_4)) 

#Se obtiene información descriptiva de las variables
summary (DTRAIN_HR$Pobre)
summary (DTRAIN_HR$Pobre_predicho_4)

#=========================================================================================================================
#Modelo 5

Datos_prediccion_train_5<-data.frame(DTRAIN_PR$id,predicciones_train_mod2_Lass_f)

colnames(Datos_prediccion_train_5) <- c('id','Ingtot_predicted')

sum_ingresos_hogar_5<-Datos_prediccion_train_5 %>% group_by(id) %>% summarize(Ingtot_hogar5=sum(Ingtot_predicted,na.rm = TRUE)) 

#Se asume que la suma del ingreso individual del hogar es aproximadamente igual al Ingtotugarr, al ingreso con imputación de arriendo, teniendo
#en cuenta que la mayoría de hogares no presenta dichos arriendos y son casi iguales.

DTRAIN_HR<-left_join(DTRAIN_HR, sum_ingresos_hogar_5) #Se une la variable sum_ingresos_hogar a la base train


DTRAIN_HR<- DTRAIN_HR %>% mutate(Pobre_predicho_5=ifelse(DTRAIN_HR$Ingtot_hogar5<Lp*Npersug,1,0)) #Se realiza la clasificación final

#Se calcula la "Confussion matrix" para determinar el nivel de accuracy y Sensitivity
CM_Reg_train_5 = confusionMatrix(data= factor(DTRAIN_HR$Pobre_predicho_5) , 
                                 reference= factor(DTRAIN_HR$Pobre) , 
                                 mode="sens_spec" , positive="1")
CM_Reg_train_5

#Se revisa la proporción de Pobres (1) y No Pobres (0)
prop.table(table(DTRAIN_HR$Pobre)) 
prop.table(table(DTRAIN_HR$Pobre_predicho_5)) 

#Se obtiene información descriptiva de las variables
summary (DTRAIN_HR$Pobre)
summary (DTRAIN_HR$Pobre_predicho_5)

#=========================================================================================================================
#Modelo 6

Datos_prediccion_train_6<-data.frame(DTRAIN_PR$id,predicciones_train_mb3ri_f)

colnames(Datos_prediccion_train_6) <- c('id','Ingtot_predicted')

sum_ingresos_hogar_6<-Datos_prediccion_train_6 %>% group_by(id) %>% summarize(Ingtot_hogar6=sum(Ingtot_predicted,na.rm = TRUE)) 

#Se asume que la suma del ingreso individual del hogar es aproximadamente igual al Ingtotugarr, al ingreso con imputación de arriendo, teniendo
#en cuenta que la mayoría de hogares no presenta dichos arriendos y son casi iguales.

DTRAIN_HR<-left_join(DTRAIN_HR, sum_ingresos_hogar_6) #Se une la variable sum_ingresos_hogar a la base train


DTRAIN_HR<- DTRAIN_HR %>% mutate(Pobre_predicho_6=ifelse(DTRAIN_HR$Ingtot_hogar6<Lp*Npersug,1,0)) #Se realiza la clasificación final

#Se calcula la "Confussion matrix" para determinar el nivel de accuracy y Sensitivity
CM_Reg_train_6 = confusionMatrix(data= factor(DTRAIN_HR$Pobre_predicho_6) , 
                                 reference= factor(DTRAIN_HR$Pobre) , 
                                 mode="sens_spec" , positive="1")
CM_Reg_train_6

#Se revisa la proporción de Pobres (1) y No Pobres (0)
prop.table(table(DTRAIN_HR$Pobre)) 
prop.table(table(DTRAIN_HR$Pobre_predicho_6)) 

#Se obtiene información descriptiva de las variables
summary (DTRAIN_HR$Pobre)
summary (DTRAIN_HR$Pobre_predicho_6)

#=========================================================================================================================
#Modelo 7

Datos_prediccion_train_7<-data.frame(DTRAIN_PR$id,predicciones_train_mod3_Lass_f)

colnames(Datos_prediccion_train_7) <- c('id','Ingtot_predicted')

sum_ingresos_hogar_7<-Datos_prediccion_train_7 %>% group_by(id) %>% summarize(Ingtot_hogar7=sum(Ingtot_predicted,na.rm = TRUE)) 

#Se asume que la suma del ingreso individual del hogar es aproximadamente igual al Ingtotugarr, al ingreso con imputación de arriendo, teniendo
#en cuenta que la mayoría de hogares no presenta dichos arriendos y son casi iguales.

DTRAIN_HR<-left_join(DTRAIN_HR, sum_ingresos_hogar_7) #Se une la variable sum_ingresos_hogar a la base train


DTRAIN_HR<- DTRAIN_HR %>% mutate(Pobre_predicho_7=ifelse(DTRAIN_HR$Ingtot_hogar7<Lp*Npersug,1,0)) #Se realiza la clasificación final

#Se calcula la "Confussion matrix" para determinar el nivel de accuracy y Sensitivity
CM_Reg_train_7 = confusionMatrix(data= factor(DTRAIN_HR$Pobre_predicho_7) , 
                                 reference= factor(DTRAIN_HR$Pobre) , 
                                 mode="sens_spec" , positive="1")
CM_Reg_train_7

#Se revisa la proporción de Pobres (1) y No Pobres (0)
prop.table(table(DTRAIN_HR$Pobre)) 
prop.table(table(DTRAIN_HR$Pobre_predicho_7)) 

#Se obtiene información descriptiva de las variables
summary (DTRAIN_HR$Pobre)
summary (DTRAIN_HR$Pobre_predicho_7)

#=========================================================================================================================
#Modelo 8

Datos_prediccion_train_8<-data.frame(DTRAIN_PR$id,predicciones_train_mb4ri_f)

colnames(Datos_prediccion_train_8) <- c('id','Ingtot_predicted')

sum_ingresos_hogar_8<-Datos_prediccion_train_8 %>% group_by(id) %>% summarize(Ingtot_hogar8=sum(Ingtot_predicted,na.rm = TRUE)) 

#Se asume que la suma del ingreso individual del hogar es aproximadamente igual al Ingtotugarr, al ingreso con imputación de arriendo, teniendo
#en cuenta que la mayoría de hogares no presenta dichos arriendos y son casi iguales.

DTRAIN_HR<-left_join(DTRAIN_HR, sum_ingresos_hogar_8) #Se une la variable sum_ingresos_hogar a la base train


DTRAIN_HR<- DTRAIN_HR %>% mutate(Pobre_predicho_8=ifelse(DTRAIN_HR$Ingtot_hogar8<Lp*Npersug,1,0)) #Se realiza la clasificación final

#Se calcula la "Confussion matrix" para determinar el nivel de accuracy y Sensitivity
CM_Reg_train_8 = confusionMatrix(data= factor(DTRAIN_HR$Pobre_predicho_8) , 
                                 reference= factor(DTRAIN_HR$Pobre) , 
                                 mode="sens_spec" , positive="1")
CM_Reg_train_8

#Se revisa la proporción de Pobres (1) y No Pobres (0)
prop.table(table(DTRAIN_HR$Pobre)) 
prop.table(table(DTRAIN_HR$Pobre_predicho_8)) 

#Se obtiene información descriptiva de las variables
summary (DTRAIN_HR$Pobre)
summary (DTRAIN_HR$Pobre_predicho_8)

#=========================================================================================================================
#Modelo 9

Datos_prediccion_train_9<-data.frame(DTRAIN_PR$id,predicciones_train_mod4_Lass_f)

colnames(Datos_prediccion_train_9) <- c('id','Ingtot_predicted')

sum_ingresos_hogar_9<-Datos_prediccion_train_9 %>% group_by(id) %>% summarize(Ingtot_hogar9=sum(Ingtot_predicted,na.rm = TRUE)) 

#Se asume que la suma del ingreso individual del hogar es aproximadamente igual al Ingtotugarr, al ingreso con imputación de arriendo, teniendo
#en cuenta que la mayoría de hogares no presenta dichos arriendos y son casi iguales.

DTRAIN_HR<-left_join(DTRAIN_HR, sum_ingresos_hogar_9) #Se une la variable sum_ingresos_hogar a la base train


DTRAIN_HR<- DTRAIN_HR %>% mutate(Pobre_predicho_9=ifelse(DTRAIN_HR$Ingtot_hogar9<Lp*Npersug,1,0)) #Se realiza la clasificación final

#Se calcula la "Confussion matrix" para determinar el nivel de accuracy y Sensitivity
CM_Reg_train_9 = confusionMatrix(data= factor(DTRAIN_HR$Pobre_predicho_9) , 
                                 reference= factor(DTRAIN_HR$Pobre) , 
                                 mode="sens_spec" , positive="1")
CM_Reg_train_9

#Se revisa la proporción de Pobres (1) y No Pobres (0)
prop.table(table(DTRAIN_HR$Pobre)) 
prop.table(table(DTRAIN_HR$Pobre_predicho_9)) 

#Se obtiene información descriptiva de las variables
summary (DTRAIN_HR$Pobre)
summary (DTRAIN_HR$Pobre_predicho_9)


#=====================================================================================


##Se realiza la predicción sobre la base Test inicial para el modelo escogido (modelo 1): 
predicciones_test_final <- predict(model_base_10, newdata = DTEST_PR)
Datos_prediccion_test<-data.frame(DTEST_PR$id,predicciones_test_final)
colnames(Datos_prediccion_test) <- c('id','Ingtot_predicted_test')

##Se realiza el cálculo del ingreso total del hogar, con base en la regresión realizada anteriormente, para cada hogar
sum_ingresos_hogar_t<-Datos_prediccion_test %>% group_by(id) %>% summarize(Ingtot_hogar_p=sum(Ingtot_predicted_test,na.rm = TRUE))
DTEST_HR<-left_join(DTEST_HR, sum_ingresos_hogar_t)

##Se realiza la transformación en estado binario del ingreso, determinando si el hogar es pobre o no
DTEST_HR<- DTEST_HR %>% mutate(Pobre_predicho_final=ifelse(DTEST_HR$Ingtot_hogar_p<Lp*Npersug,1,0))

#Visualización de datos
view(DTEST_HR$Pobre_predicho_final)
prop.table(table(DTEST_HR$Pobre_predicho_final)) 

TestResulstFinal_2 <- cbind(TestResulstFinal_1 , DTEST_HR$Pobre_predicho_final )
colnames(TestResulstFinal_2) <- c('id','Pobre_classification','Pobre_income')
write.csv (TestResulstFinal_2, "../Elementos_Guardados/predictions_beleno_gaona_c4_r34.csv")

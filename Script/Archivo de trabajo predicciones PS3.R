# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 3 #####

#####Código de predicción de los precios de las viviendas con las variables ya seleccionadas y guardadas en archivos rds exportados del código: Limpieza de datos

install.packages("pacman") #Instalar librería si no cuenta con esta 
library(pacman) #Llamar librería#Se cargan las librerías a usar en el presente Problem Set
p_load(caret, 
       Matrix,
       recipes,
       rio, 
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
       pROC, 
       sf,
       leaflet,
       tmaptools,
       class,
       rgeos,
       nngeo,
       osmdata,
       randomForest,
       xgboost,
       nnls,
       data.table,
       ranger, SuperLearner, caret)
require("tidyverse")

DTEST_H<-data.frame(readRDS("../Elementos_Guardados/DTESTHOUSE.rds"))  #Guardar las bases de datos
DTRAIN_H <- data.frame(readRDS("../Elementos_Guardados/DTRAINHOUSE.rds"))

#Se elimina la columna de precio en la base Dtest 
DTEST_H<-DTEST_H%>% mutate(price = NULL)

#Se crean las variables Dummies para la ciudad (Bogotá o Medellin) y para el tipo de propiedad (Casa o Apartamento)
#Para la dummy de ciudad se escogió que Medellín fuese 1, Bogotá 0 y Apartamento es 1 y Casa es igual a cero

DTEST_H<-DTEST_H%>% mutate(Medellin = ifelse(l3=="Medellín", 1,0))
DTRAIN_H<-DTRAIN_H%>% mutate(Medellin = ifelse(l3=="Medellín", 1,0))

DTEST_H<-DTEST_H%>% mutate(Apto = ifelse(property_type=="Apartamento", 1,0))
DTRAIN_H<-DTRAIN_H%>% mutate(Apto = ifelse(property_type=="Apartamento", 1,0))

#Se realizarán xx modelos para realizar las predicciones del precio de las viviendas, el modelo con menor MSE será con el cual se realizará la predicción en la base Test

#Modelo OLS

modelo1a <- lm(price ~ factor(Medellin) + factor(Apto) + factor(parqueaderoT) + factor(ascensorT) + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park +surface_new_3, data =DTRAIN_H)

predicciones_mod1a <- predict(modelo1a, newdata = DTRAIN_H)
Diferencia_mod1a <- (predicciones_mod1a - DTRAIN_H$price)
Diferencia_mod1a<-data.frame(Diferencia_mod1a)
MSE_mod1a<- mean(sqrt((predicciones_mod1a - DTRAIN_H$price)^2))

require(stargazer)
stargazer(modelo1)




#Modelo 2 Lasso

x_traina <- model.matrix(price ~ factor(Medellin) + factor(Apto) + factor(parqueaderoT) + factor(ascensorT) + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park+surface_new_3, data =DTRAIN_H)[, -1]
y_traina <- DTRAIN_H$price

modelobase2lass_f <- glmnet(
  x           = x_traina,
  y           = y_traina,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_m2_Lass_f <- cv.glmnet(
  x      = x_traina,
  y      = y_traina,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


modelo2_Lasso_f <- glmnet(
  x           = x_traina,
  y           = y_traina,
  alpha       = 1,
  lambda      = cv_error_m2_Lass_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mod2_Lass_f <- predict(modelo2_Lasso_f, newx = x_traina)
MSE_mod2 <- mean(sqrt((predicciones_train_mod2_Lass_f -y_traina)^2))
Diferencia_mod2 <- (predicciones_train_mod2_Lass_f - DTRAIN_H$price)
Diferencia_mod2<-data.frame(Diferencia_mod2)

#Modelo 3 - Random Forest

set.seed(10101)


modelo3_forest <- randomForest(
  price ~ Medellin + Apto + parqueaderoT + ascensorT + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park+surface_new_3,
  data = DTRAIN_H,
  num.trees = 5,
  write.forest = TRUE
)

varImp(modelo3_forest,scale=TRUE)

predicciones_mod3_rf_completo<-predict(modelo3_forest, data = DTRAIN_H)
#predicciones_mod3_rf<-predicciones_mod3_rf_completo$predictions

#predicciones_mod3_rf<-data.frame(predicciones_mod3_rf)


MSE_mod3 <- sqrt(mean((predicciones_mod3_rf-DTRAIN_H$price)^2))
Diferencia_mod3 <- (predicciones_mod3_rf - DTRAIN_H$price)
Diferencia_mod3<-data.frame(Diferencia_mod3)

#Modelo 4 - XGBoost

xgb_train <- xgb.DMatrix(data = x_traina, label = y_traina)
xgb_test <- xgb.DMatrix(data = x_traina, label = y_traina) #Como se está haciendo sobre la misma base train, se pone el xgb_test como la misma base train

watchlist <-list(train=xgb_train, test=xgb_test)

model4<- xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

summary(model4)
predicciones_mod4 <-predict(model4, xgb_test) #El xgb_test corresponde a la misma base train

MSE_mod4 <- sqrt(mean((predicciones_mod4-DTRAIN_H$price)^2))
Diferencia_mod4 <- (predicciones_mod4 - DTRAIN_H$price)
Diferencia_mod4<-data.frame(Diferencia_mod4)

#Modelo- Superlearner
#==========================================================================================================================================================
folds = 5

x_SL<-subset(DTRAIN_H, select=c("Medellin", "Apto", "parqueaderoT", "ascensorT", "bathrooms","habitaciones", "min_dist_bar_","min_dist_transp_", "min_dist_park", "surface_new_3"))
Y_SL<-DTRAIN_H$price

D <- data.frame(x_SL, Y_SL)
index <- list(2739,2739,2739,2738,2738)
splt <- lapply(1:folds, function(ind) D[index[[ind]], ])



library(SuperLearner)

# fitY <- SuperLearner(Y = Y_SL, X = data.frame(x_SL),
#                      method = "method.NNLS", SL.library = c("SL.lm", "SL.ranger"),
#                      cvControl = list(V = folds, validRows = index))
# 
# fitY #Este código no sirvió

m1 <- lapply(1:folds, function(ii) lm(Y_SL ~Medellin + Apto + parqueaderoT + ascensorT + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park, data = rbindlist(splt[-ii])))
m2 <- lapply(1:folds, function(ii) ranger(Y_SL ~Medellin + Apto + parqueaderoT + ascensorT + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park,data = rbindlist(splt[-ii])))


p1 <- lapply(1:folds, function(ii) predict(m1[[ii]], newdata = rbindlist(splt[ii])))
p2 <- lapply(1:folds, function(ii) predict(m2[[ii]], data = rbindlist(splt[ii]))$predictions)

for (i in 1:folds) {
splt[[i]] <- cbind(splt[[i]], p1[[i]],
                   p2[[i]])
}

head(splt[[1]])

risk1 <- lapply(1:folds, function(ii) mean((splt[[ii]][,10] - splt[[ii]][, 11])^2))
risk2 <- lapply(1:folds, function(ii) mean((splt[[ii]][, 10] - splt[[ii]][, 12])^2))

a <- rbind(cbind("lm", mean(do.call(rbind, risk1), na.rm = T)),
           cbind("RF", mean(do.call(rbind,risk2), na.rm = T)))
a

X_ <- data.frame(do.call(rbind, splt))[, -1]

summary(X_)
X_<-subset(X_,  select=c("Y_SL","p1..i..", "p2..i..") )
names(X_) <- c("y", "lm", "RF")
head(X_)

SL.r <- nnls(cbind(X_[, 2], X_[, 3]), X_[, 1])$x
alpha <- as.matrix(SL.r/sum(SL.r))
alpha #Prueba y error: A nuestro criterio, el Super learner no funcionó
#====================================================================================================================

### Clasificación  de modelos por evaluación de compra


Precio_compra_mod1<-ifelse(Diferencia_mod1a<=-40000000,0, predicciones_mod1a)
Precio_compra_mod1<-data.frame(Precio_compra_mod1)
Precio_tot_mod1<-colSums(Precio_compra_mod1)
view(Precio_tot_mod1)
Zeros_mod1<-ifelse(Precio_compra_mod1==0,TRUE, FALSE)
table(Zeros_mod1)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_mod2<-ifelse(Diferencia_mod2<=-40000000,0, predicciones_train_mod2_Lass_f )
Precio_compra_mod2<-data.frame(Precio_compra_mod2)
Precio_tot_mod2<-colSums(Precio_compra_mod2)
view(Precio_tot_mod2)
Zeros_mod2<-ifelse(Precio_compra_mod2==0,TRUE, FALSE)
table(Zeros_mod2)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_mod3<-ifelse(Diferencia_mod3<=-40000000,0, predicciones_mod3_rf_completo  )
Precio_compra_mod3<-data.frame(Precio_compra_mod3)
Precio_tot_mod3<-colSums(Precio_compra_mod3)
view(Precio_tot_mod3)
Zeros_mod3<-ifelse(Precio_compra_mod3==0,TRUE, FALSE)
table(Zeros_mod3)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_mod4<-ifelse(Diferencia_mod4<=-40000000,0, predicciones_mod4  )
Precio_compra_mod4<-data.frame(Precio_compra_mod4)
Precio_tot_mod4<-colSums(Precio_compra_mod4)
view(Precio_tot_mod4)
Zeros_mod4<-ifelse(Precio_compra_mod4==0,TRUE, FALSE)
table(Zeros_mod4) #La cantidad de falsos (diferente de cero) son las propiedades compradas


#############################################################################################################
#Se repite código

#Modelo OLS

modelo1 <- lm(price ~ factor(Medellin) + factor(Apto) + factor(parqueaderoT) + factor(ascensorT) + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park, data =DTRAIN_H)

predicciones_mod1 <- predict(modelo1, newdata = DTRAIN_H)
Diferencia_mod1 <- (predicciones_mod1 - DTRAIN_H$price)
Diferencia_mod1<-data.frame(Diferencia_mod1)
MSE_mod1<- mean(sqrt((predicciones_mod1 - DTRAIN_H$price)^2))

require(stargazer)
stargazer(modelo1, type="text")

#Modelo 2 Lasso

x_train <- model.matrix(price ~ factor(Medellin) + factor(Apto) + factor(parqueaderoT) + factor(ascensorT) + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park, data =DTRAIN_H)[, -1]
y_train <- DTRAIN_H$price

modelobase2lass_f <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_m2_Lass_f <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


modelo2_Lasso_f <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error_m2_Lass_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mod2_Lass_f <- predict(modelo2_Lasso_f, newx = x_train)
MSE_mod2 <- mean(sqrt((predicciones_train_mod2_Lass_f -y_train)^2))
Diferencia_mod2 <- (predicciones_train_mod2_Lass_f - DTRAIN_H$price)
Diferencia_mod2<-data.frame(Diferencia_mod2)

#Modelo 3 - Random Forest

set.seed(10101)


modelo3_forest <- randomForest(
  price ~ Medellin + Apto + parqueaderoT + ascensorT + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park,
  data = DTRAIN_H,
  num.trees = 5,
  write.forest = TRUE
)
getTerminalNodeIDs(modelo3_forest , DTRAIN_H)

library(ranger)
varImp(modelo3_forest,scale=TRUE)

predicciones_mod3_rf_completo<-predict(modelo3_forest, data = DTRAIN_H)
predicciones_mod3_rf<-predicciones_mod3_rf_completo$predictions
#predicciones_mod3_rf<-data.frame(predicciones_mod3_rf)


MSE_mod3 <- sqrt(mean((predicciones_mod3_rf-DTRAIN_H$price)^2))
Diferencia_mod3 <- (predicciones_mod3_rf - DTRAIN_H$price)
Diferencia_mod3<-data.frame(Diferencia_mod3)

#Modelo 4 - XGBoost

xgb_train <- xgb.DMatrix(data = x_train, label = y_train)
xgb_test <- xgb.DMatrix(data = x_train, label = y_train)

watchlist <-list(train=xgb_train, test=xgb_test)

model4<- xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

summary(model4)
predicciones_mod4 <-predict(model4, xgb_test)

MSE_mod4 <- sqrt(mean((predicciones_mod4-DTRAIN_H$price)^2))
Diferencia_mod4 <- (predicciones_mod4 - DTRAIN_H$price)
Diferencia_mod4<-data.frame(Diferencia_mod4)

#Modelo 5- Superlearner

folds = 5

x_SL<-subset(DTRAIN_H, select=c("Medellin", "Apto", "parqueaderoT", "ascensorT", "bathrooms","habitaciones", "min_dist_bar_","min_dist_transp_", "min_dist_park"))
Y_SL<-DTRAIN_H$price

D <- data.frame(x_SL, Y_SL)
index <- list(2739,2739,2739,2738,2738)
splt <- lapply(1:folds, function(ind) D[index[[ind]], ])



library(SuperLearner)

fitY <- SuperLearner(Y = Y_SL, X = data.frame(x_SL),
                     method = "method.NNLS", SL.library = c("SL.lm", "SL.ranger"),
                     cvControl = list(V = folds, validRows = index))

fitY

m1 <- lapply(1:folds, function(ii) lm(Y_SL ~Medellin + Apto + parqueaderoT + ascensorT + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park, data = rbindlist(splt[-ii])))
m2 <- lapply(1:folds, function(ii) ranger(Y_SL ~Medellin + Apto + parqueaderoT + ascensorT + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park,data = rbindlist(splt[-ii])))


p1 <- lapply(1:folds, function(ii) predict(m1[[ii]], newdata = rbindlist(splt[ii])))
p2 <- lapply(1:folds, function(ii) predict(m2[[ii]], data = rbindlist(splt[ii]))$predictions)

for (i in 1:folds) {
  splt[[i]] <- cbind(splt[[i]], p1[[i]],
                     p2[[i]])
}

head(splt[[1]])

risk1 <- lapply(1:folds, function(ii) mean((splt[[ii]][,10] - splt[[ii]][, 11])^2))
risk2 <- lapply(1:folds, function(ii) mean((splt[[ii]][, 10] - splt[[ii]][, 12])^2))

a <- rbind(cbind("lm", mean(do.call(rbind, risk1), na.rm = T)),
           cbind("RF", mean(do.call(rbind,risk2), na.rm = T)))
a

X_ <- data.frame(do.call(rbind, splt))[, -1]

summary(X_)
X_<-subset(X_,  select=c("Y_SL","p1..i..", "p2..i..") )
names(X_) <- c("y", "lm", "RF")
head(X_)

SL.r <- nnls(cbind(X_[, 2], X_[, 3]), X_[, 1])$x
alpha <- as.matrix(SL.r/sum(SL.r))
alpha #Prueba y error: A nuestro criterio, el Super learner no funcionó

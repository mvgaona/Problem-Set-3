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
       rvest,
       caret,
       stringr,
       boot,
       caret,
       modeest,
       stargazer,
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

DTEST<-data.frame(readRDS("../Elementos_Guardados/test.rds")) #Se importa el archivo original enviado por el profesor, para tomar de referencia el orden del property ID



#Se realizarán xx modelos para realizar las predicciones del precio de las viviendas, el modelo con menor MSE será con el cual se realizará la predicción en la base Test

#Modelo OLS

modelo1a <- lm(price ~ factor(Medellin) + factor(Apto) + factor(parqueaderoT) + factor(ascensorT) + bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park +surface_new_3, data =DTRAIN_H)

predicciones_mod1a <- predict(modelo1a, newdata = DTRAIN_H)
Diferencia_mod1a <- (predicciones_mod1a - DTRAIN_H$price)
Diferencia_mod1a<-data.frame(Diferencia_mod1a)
MSE_mod1a<- mean(sqrt((predicciones_mod1a - DTRAIN_H$price)^2))

require(stargazer)
stargazer(modelo1a)

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

MSE_mod3 <- sqrt(mean((predicciones_mod3_rf_completo-DTRAIN_H$price)^2))
Diferencia_mod3 <- (predicciones_mod3_rf_completo - DTRAIN_H$price)
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


#Modelo OLS para variables relevantes del ejercicio anterior en RF (6 variables)

modelo1b <- lm(price ~  bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park+surface_new_3, data =DTRAIN_H)

predicciones_mod1b <- predict(modelo1b, newdata = DTRAIN_H)
Diferencia_mod1b <- (predicciones_mod1b - DTRAIN_H$price)
Diferencia_mod1b<-data.frame(Diferencia_mod1b)
MSE_mod1b<- mean(sqrt((predicciones_mod1b - DTRAIN_H$price)^2))


#Modelo 2c Lasso

x_trainb <- model.matrix(price ~bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park+surface_new_3, data =DTRAIN_H)[, -1]
y_trainb <- DTRAIN_H$price

modelobase2lass_fb <- glmnet(
  x           = x_trainb,
  y           = y_trainb,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_m2_Lass_fb <- cv.glmnet(
  x      = x_trainb,
  y      = y_trainb,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


modelo2_Lasso_fb <- glmnet(
  x           = x_trainb,
  y           = y_trainb,
  alpha       = 1,
  lambda      = cv_error_m2_Lass_fb$lambda.1se,
  standardize = TRUE
)

predicciones_train_mod2_Lass_fb <- predict(modelo2_Lasso_fb, newx = x_trainb)
MSE_mod2b <- mean(sqrt((predicciones_train_mod2_Lass_fb -y_trainb)^2))
Diferencia_mod2b <- (predicciones_train_mod2_Lass_fb - DTRAIN_H$price)
Diferencia_mod2b<-data.frame(Diferencia_mod2b)

#Modelo 3b - Random Forest

set.seed(10101)


modelo3_forestb <- randomForest(
  price ~ bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+min_dist_park+surface_new_3,
  data = DTRAIN_H,
  num.trees = 5,
  write.forest = TRUE
)



predicciones_mod3_rf_completob<-predict(modelo3_forestb, data = DTRAIN_H)

MSE_mod3b <- sqrt(mean((predicciones_mod3_rf_completob-DTRAIN_H$price)^2))
Diferencia_mod3b <- (predicciones_mod3_rf_completob - DTRAIN_H$price)
Diferencia_mod3b<-data.frame(Diferencia_mod3b)

#Modelo 4b - XGBoost

xgb_trainb <- xgb.DMatrix(data = x_trainb, label = y_trainb)
xgb_testb <- xgb.DMatrix(data = x_trainb, label = y_trainb)

watchlistb <-list(train=xgb_trainb, test=xgb_testb)

model4b<- xgb.train(data = xgb_trainb, max.depth = 3, watchlist=watchlistb, nrounds = 100)

summary(model4b)
predicciones_mod4b <-predict(model4b, xgb_testb) #Es la misma base train

MSE_mod4b <- sqrt(mean((predicciones_mod4b-DTRAIN_H$price)^2))
Diferencia_mod4b <- (predicciones_mod4b - DTRAIN_H$price)
Diferencia_mod4b<-data.frame(Diferencia_mod4b)

### Clasificación  de modelos por evaluación de compra

Precio_compra_mod1b<-ifelse(Diferencia_mod1b<=-40000000,0, predicciones_mod1b)
Precio_compra_mod1b<-data.frame(Precio_compra_mod1b)
Precio_tot_mod1b<-colSums(Precio_compra_mod1b)
view(Precio_tot_mod1b)
Zeros_mod1b<-ifelse(Precio_compra_mod1b==0,TRUE, FALSE)
table(Zeros_mod1b)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_mod2b<-ifelse(Diferencia_mod2b<=-40000000,0, predicciones_train_mod2_Lass_fb )
Precio_compra_mod2b<-data.frame(Precio_compra_mod2b)
Precio_tot_mod2b<-colSums(Precio_compra_mod2b)
view(Precio_tot_mod2b)
Zeros_mod2b<-ifelse(Precio_compra_mod2b==0,TRUE, FALSE)
table(Zeros_mod2b)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_mod3b<-ifelse(Diferencia_mod3b<=-40000000,0, predicciones_mod3_rf_completob  )
Precio_compra_mod3b<-data.frame(Precio_compra_mod3b)
Precio_tot_mod3b<-colSums(Precio_compra_mod3b)
view(Precio_tot_mod3b)
Zeros_mod3b<-ifelse(Precio_compra_mod3b==0,TRUE, FALSE)
table(Zeros_mod3b)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_mod4b<-ifelse(Diferencia_mod4b<=-40000000,0, predicciones_mod4b  )
Precio_compra_mod4b<-data.frame(Precio_compra_mod4b)
Precio_tot_mod4b<-colSums(Precio_compra_mod4b)
view(Precio_tot_mod4b)
Zeros_mod4b<-ifelse(Precio_compra_mod4b==0,TRUE, FALSE)
table(Zeros_mod4b) #La cantidad de falsos (diferente de cero) son las propiedades compradas

#############################################################################################################


#Modelo OLS para variables relevantes del ejercicio anterior en OLS (5 variables)

modelo1c <- lm(price ~  bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+surface_new_3, data =DTRAIN_H)

predicciones_mod1c <- predict(modelo1c, newdata = DTRAIN_H)
Diferencia_mod1c <- (predicciones_mod1c - DTRAIN_H$price)
Diferencia_mod1c<-data.frame(Diferencia_mod1c)
MSE_mod1c<- mean(sqrt((predicciones_mod1c - DTRAIN_H$price)^2))


#Modelo 2b Lasso

x_trainc <- model.matrix(price ~bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+surface_new_3, data =DTRAIN_H)[, -1]
y_trainc <- DTRAIN_H$price

modelobase2lass_fc <- glmnet(
  x           = x_trainc,
  y           = y_trainc,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_m2_Lass_fc <- cv.glmnet(
  x      = x_trainc,
  y      = y_trainc,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


modelo2_Lasso_fc <- glmnet(
  x           = x_trainc,
  y           = y_trainc,
  alpha       = 1,
  lambda      = cv_error_m2_Lass_fc$lambda.1se,
  standardize = TRUE
)

predicciones_train_mod2_Lass_fc <- predict(modelo2_Lasso_fc, newx = x_trainc)
MSE_mod2c <- mean(sqrt((predicciones_train_mod2_Lass_fc -y_trainc)^2))
Diferencia_mod2c <- (predicciones_train_mod2_Lass_fc - DTRAIN_H$price)
Diferencia_mod2c<-data.frame(Diferencia_mod2c)

#Modelo 3b - Random Forest

set.seed(10101)


modelo3_forestc <- randomForest(
  price ~ bathrooms+habitaciones+min_dist_bar_+min_dist_transp_+surface_new_3,
  data = DTRAIN_H,
  num.trees = 5,
  write.forest = TRUE
)



predicciones_mod3_rf_completoc<-predict(modelo3_forestc, data = DTRAIN_H)

MSE_mod3c <- sqrt(mean((predicciones_mod3_rf_completoc-DTRAIN_H$price)^2))
Diferencia_mod3c <- (predicciones_mod3_rf_completoc - DTRAIN_H$price)
Diferencia_mod3c<-data.frame(Diferencia_mod3c)

#Modelo 4c - XGBoost

xgb_trainc <- xgb.DMatrix(data = x_trainc, label = y_trainc)
xgb_testc <- xgb.DMatrix(data = x_trainc, label = y_trainc)

watchlistc <-list(train=xgb_trainc, test=xgb_testc)

model4c<- xgb.train(data = xgb_trainc, max.depth = 3, watchlist=watchlistc, nrounds = 100)

summary(model4c)
predicciones_mod4c <-predict(model4c, xgb_testc) #Es la misma base train

MSE_mod4c <- sqrt(mean((predicciones_mod4c-DTRAIN_H$price)^2))
Diferencia_mod4c <- (predicciones_mod4c - DTRAIN_H$price)
Diferencia_mod4c<-data.frame(Diferencia_mod4c)

### Clasificación  de modelos por evaluación de compra

Precio_compra_mod1c<-ifelse(Diferencia_mod1c<=-40000000,0, predicciones_mod1c)
Precio_compra_mod1c<-data.frame(Precio_compra_mod1c)
Precio_tot_mod1c<-colSums(Precio_compra_mod1c)
view(Precio_tot_mod1c)
Zeros_mod1c<-ifelse(Precio_compra_mod1c==0,TRUE, FALSE)
table(Zeros_mod1c)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_mod2c<-ifelse(Diferencia_mod2c<=-40000000,0, predicciones_train_mod2_Lass_fc )
Precio_compra_mod2c<-data.frame(Precio_compra_mod2c)
Precio_tot_mod2c<-colSums(Precio_compra_mod2c)
view(Precio_tot_mod2c)
Zeros_mod2c<-ifelse(Precio_compra_mod2c==0,TRUE, FALSE)
table(Zeros_mod2c)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_mod3c<-ifelse(Diferencia_mod3c<=-40000000,0, predicciones_mod3_rf_completoc  )
Precio_compra_mod3c<-data.frame(Precio_compra_mod3c)
Precio_tot_mod3c<-colSums(Precio_compra_mod3c)
view(Precio_tot_mod3c)
Zeros_mod3c<-ifelse(Precio_compra_mod3c==0,TRUE, FALSE)
table(Zeros_mod3c)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_mod4c<-ifelse(Diferencia_mod4c<=-40000000,0, predicciones_mod4c  )
Precio_compra_mod4c<-data.frame(Precio_compra_mod4c)
Precio_tot_mod4c<-colSums(Precio_compra_mod4c)
view(Precio_tot_mod4c)
Zeros_mod4c<-ifelse(Precio_compra_mod4c==0,TRUE, FALSE)
table(Zeros_mod4c) #La cantidad de falsos (diferente de cero) son las propiedades compradas


#===========================================================================================
#Gráfica de MSE- para apéndice

sqrt_MSE_modelos<-c(MSE_mod1a, MSE_mod2, MSE_mod3, MSE_mod4)
modelos_<-c('modelo1_10var','modelo2_10var', 'modelo3_10var', 'modelo4_10var')
sqrt_MSE_errores<-data.frame(modelos_,sqrt_MSE_modelos)

#Se grafica el resultado
ggplot(data=sqrt_MSE_errores, aes(x = modelos_, y = sqrt_MSE_modelos, group=1)) + 
  geom_line()+   geom_point()+  labs(title = "Comparación diferentes modelos en términos de sqrt_MSE") 

#=====================================
#De acuerdo a la comparación de los 12 modelos, el modelo modelo3_forest con 10 variables explicativas es el cual tiene la mejor proporción de dinero invertido/viviendas comparas. 
#Por lo tanto, Se realiza la predicción con el modelo modelo3_forest en la base DTEST_H 
Predicciones_PreciosViv <- predict(modelo3_forest,  newdata = DTEST_H)
Predicciones_PreciosViv <- data.frame (Predicciones_PreciosViv)
View(Predicciones_PreciosViv)
Predicciones_PreciosViv <- cbind(DTEST_H$property_id ,Predicciones_PreciosViv)
View(Predicciones_PreciosViv)
View(cbind(DTEST_H$property_id, Predicciones_PreciosViv$`DTEST_H$property_id`))

colnames(Predicciones_PreciosViv) <- c('property_id','price')

Property_id_or<-DTEST$property_id
Property_id_or<- data.frame (Property_id_or)
colnames(Property_id_or) <- c('property_id','price')
Predicciones_PreciosViv <- cbind(Predicciones_PreciosViv, Property_id_or)
rm (Predicciones_PreciosViv)

Property_id_or<-left_join(Property_id_or, Predicciones_PreciosViv, by="property_id" )

colnames(Property_id_or) <- c('property_id','price')
saveRDS(Property_id_or, "../Elementos_Guardados/Predicciones.rds")
write.csv (Property_id_or, "../Elementos_Guardados/predictions_beleno_gaona.csv")

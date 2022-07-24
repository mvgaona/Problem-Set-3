# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 3 #####

#####SuperLearner

require("tidyverse")
p_load(ranger, SuperLearner, caret)

folds = 5
fitY <- SuperLearner(Y = DTRAIN$price, X = DTRAIN,
                     method = "method.NNLS", SL.library = c("SL.lm", "SL.ranger"),
                     cvControl = list(V = folds, validRows = index))
PriceS <- predict(fitY, newdata = DTEST,onlySL = T)$pred


SuperLearner(Y = DTRAIN$price, X = DTRAIN, SL.library = c("SL.lm", "SL.ranger"),
             method = "method.NNLS", cvControl = list(V = folds, validRows = index))


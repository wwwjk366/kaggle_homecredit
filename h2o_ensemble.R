#h2o random forest
library(h2o)
library(tidyverse)
library(xgboost)
library(magrittr)
library(futile.logger)
library(caret)
set.seed(0)

#---------------------------
futile.logger::flog.info("Loading data...")
tr <- read_csv("input/application_train.csv.zip") 
tri <- 1:nrow(tr)
y <- tr$TARGET
tr_te <- readRDS("tr_te.RDS") %>% as.data.frame()
#---------------------------
futile.logger::flog.info("Preparing data for model (separate train and test, etc)...")

test <- tr_te[-tri, ]
train <- tr_te[tri, ] 
train %<>%
  mutate(TARGET = fct_recode(as.factor(y), defaulter = "1", repayer = "0")) 

index <- createDataPartition(train$TARGET, p=0.9, list=FALSE)
train_set <- train[index,]
test_set <- train[-index,]
# start -------------------------------------------------------------------

h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "16G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

tr_h2o <- as.h2o(train)
te_h2o <- as.h2o(test_set)

predictors <- names(tr_te)



# XGB ---------------------------------------------------------------------
h2o_xgb <- h2o.xgboost(x = predictors,
                       y = "TARGET",
                       training_frame = tr_h2o,
                       distribution = "bernoulli",
                       stopping_rounds = 200,
                       stopping_metric = "AUC",
                       ntrees = 2000,
                       max_depth = 7,
                       min_rows = 1,
                       learn_rate = 0.025,
                       subsample =  0.85, 
                       colsample_bytree = 0.6,
                       colsample_bylevel = 0.6,
                       # col_sample_rate = 0.9,
                       booster = "gbtree",
                       nfolds = 5,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       gamma = 0.0,
                       verbose = FALSE,
                       seed = 0)

summary(h2o_xgb)

h2o.saveModel(h2o_xgb, path = ".") # define your path here

# h2o.loadModel(my_xgb2)

xgb_predictions <- h2o.predict(
  object = h2o_xgb,
  newdata = as.h2o(test))


#Predicting using random forest model
# testSet$pred_rf <- predict(object = model_rf,testSet[,predictors])
# 
# read_csv("input/sample_submission.csv.zip") %>%  
#   mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
#          TARGET = as.data.frame(xgb_predictions) %>% pull(defaulter)) %>%
#   write_csv(paste0("h2o_xgb_", round(h2o_xgb@model$cross_validation_metrics@metrics$AUC, 5), ".csv"))
# 
# 


# RF ----------------------------------------------------------------------

model_rf <- h2o::h2o.randomForest(x = predictors,
                                  y = "TARGET", 
                                  training_frame = tr_h2o, 
                                  validation_frame = te_h2o,
                                  ntrees = 500
)



# GBM ---------------------------------------------------------------------



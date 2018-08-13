library(tidyverse)
library(xgboost)
library(magrittr)
library(caret)
set.seed(0)

#---------------------------
cat("Loading data...\n")

bbalance <- read_csv("input/bureau_balance.csv.zip") 
bureau <- read_csv("input/bureau.csv.zip")
cc_balance <- read_csv("input/credit_card_balance.csv.zip")
payments <- read_csv("input/installments_payments.csv.zip") 
pc_balance <- read_csv("input/POS_CASH_balance.csv.zip")
prev <- read_csv("input/previous_application.csv.zip")
tr <- read_csv("input/application_train.csv.zip") 
te <- read_csv("input/application_test.csv.zip")

#---------------------------
cat("Preprocessing...\n")

# 
# tr %<>% 
#   bind_rows(tr %>% filter(TARGET == 1))

#I commented out ones that kxx used for creating new variables (e.g. AMT_CREDIT / AMT_ANNUITY)
#I am forced to do this because the score is poor if I don't.
# 
# #tr$AMT_INCOME_TOTAL <- log1p(tr$AMT_INCOME_TOTAL)
# #tr$AMT_CREDIT <- log1p(tr$AMT_CREDIT)
# #tr$AMT_ANNUITY <- log1p(tr$AMT_ANNUITY)
# #tr$AMT_GOODS_PRICE <- log1p(tr$AMT_GOODS_PRICE)
# tr$REGION_POPULATION_RELATIVE <- sqrt(tr$REGION_POPULATION_RELATIVE)
# #tr$DAYS_BIRTH <- sqrt(abs(tr$DAYS_BIRTH))
# #tr$DAYS_EMPLOYED <- sqrt(abs(tr$DAYS_EMPLOYED))
# tr$DAYS_REGISTRATION <- sqrt(abs(tr$DAYS_REGISTRATION))
# #tr$OWN_CAR_AGE <- sqrt(abs(tr$OWN_CAR_AGE))
# tr$APARTMENTS_AVG <- log1p(50*tr$APARTMENTS_AVG)
# tr$YEARS_BEGINEXPLUATATION_AVG <- (tr$YEARS_BEGINEXPLUATATION_AVG)^30
# tr$YEARS_BUILD_AVG <- (tr$YEARS_BUILD_AVG)^3
# tr$COMMONAREA_AVG <- (tr$COMMONAREA_AVG)^(-1/200)
# tr$ELEVATORS_AVG <- (tr$ELEVATORS_AVG)^(1/40)
# tr$ENTRANCES_AVG <- (tr$ENTRANCES_AVG)^(1/3)
# tr$FLOORSMAX_AVG <- (tr$FLOORSMAX_AVG)^(1/2.5)
# tr$FLOORSMIN_AVG <- (tr$FLOORSMIN_AVG)^(1/2.2)
# tr$LANDAREA_VG <- (tr$LANDAREA_AVG)^(1/5)
# tr$LIVINGAPRTMENTS_AVG <- (tr$LIVINGAPARTMENTS_AVG)^(1/3)
# tr$LIVINGAREA_AVG <- (tr$LIVINGAREA_AVG)^(1/3.5)
# tr$NONLIVINGAPARTMENTS_AVG <- (tr$NONLIVINGAPARTMENTS_AVG)^(1/7)
# tr$NONLIVINGAREA_AVG <- (tr$NONLIVINGAREA_AVG)^(1/5)
# tr$TOTALAREA_MODE <- (tr$TOTALAREA_MODE)^(1/3)
# tr$OBS_30_CNT_SOCIAL_CIRCLE <- (tr$OBS_30_CNT_SOCIAL_CIRCLE)^(1/7)
# tr$DEF_30_CNT_SOCIAL_CIRCLE <- (tr$DEF_30_CNT_SOCIAL_CIRCLE)^(1/7)
# tr$OBS_60_CNT_SOCIAL_CIRCLE <- (tr$OBS_60_CNT_SOCIAL_CIRCLE)^(1/7)
# tr$DEF_60_CNT_SOCIAL_CIRCLE <- (tr$DEF_60_CNT_SOCIAL_CIRCLE)^(1/7)
# #tr$DAYS_LAST_PHONE_CHANGE <- (abs(tr$DAYS_LAST_PHONE_CHANGE))^(1/2)
# 
# #te$AMT_INCOME_TOTAL <- log1p(te$AMT_INCOME_TOTAL)
# #te$AMT_CREDIT <- log1p(te$AMT_CREDIT)
# #te$AMT_ANNUITY <- log1p(te$AMT_ANNUITY)
# #te$AMT_GOODS_PRICE <- log1p(te$AMT_GOODS_PRICE)
# te$REGION_POPULATION_RELATIVE <- sqrt(te$REGION_POPULATION_RELATIVE)
# #te$DAYS_BIRTH <- sqrt(abs(te$DAYS_BIRTH))
# #te$DAYS_EMPLOYED <- sqrt(abs(te$DAYS_EMPLOYED))
# te$DAYS_REGISTRATION <- sqrt(abs(te$DAYS_REGISTRATION))
# #te$OWN_CAR_AGE <- sqrt(abs(te$OWN_CAR_AGE))
# te$APARTMENTS_AVG <- log1p(50*te$APARTMENTS_AVG)
# te$YEARS_BEGINEXPLUATATION_AVG <- (te$YEARS_BEGINEXPLUATATION_AVG)^30
# te$YEARS_BUILD_AVG <- (te$YEARS_BUILD_AVG)^3
# te$COMMONAREA_AVG <- (te$COMMONAREA_AVG)^(-1/200)
# te$ELEVATORS_AVG <- (te$ELEVATORS_AVG)^(1/40)
# te$ENTRANCES_AVG <- (te$ENTRANCES_AVG)^(1/3)
# te$FLOORSMAX_AVG <- (te$FLOORSMAX_AVG)^(1/2.5)
# te$FLOORSMIN_AVG <- (te$FLOORSMIN_AVG)^(1/2.2)
# te$LANDAREA_VG <- (te$LANDAREA_AVG)^(1/5)
# te$LIVINGAPRTMENTS_AVG <- (te$LIVINGAPARTMENTS_AVG)^(1/3)
# te$LIVINGAREA_AVG <- (te$LIVINGAREA_AVG)^(1/3.5)
# te$NONLIVINGAPARTMENTS_AVG <- (te$NONLIVINGAPARTMENTS_AVG)^(1/7)
# te$NONLIVINGAREA_AVG <- (te$NONLIVINGAREA_AVG)^(1/5)
# te$TOTALAREA_MODE <- (te$TOTALAREA_MODE)^(1/3)
# te$OBS_30_CNT_SOCIAL_CIRCLE <- (te$OBS_30_CNT_SOCIAL_CIRCLE)^(1/7)
# te$DEF_30_CNT_SOCIAL_CIRCLE <- (te$DEF_30_CNT_SOCIAL_CIRCLE)^(1/7)
# te$OBS_60_CNT_SOCIAL_CIRCLE <- (te$OBS_60_CNT_SOCIAL_CIRCLE)^(1/7)
# te$DEF_60_CNT_SOCIAL_CIRCLE <- (te$DEF_60_CNT_SOCIAL_CIRCLE)^(1/7)
# #te$DAYS_LAST_PHONE_CHANGE <- (abs(te$DAYS_LAST_PHONE_CHANGE))^(1/2)


fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

sum_bbalance <- bbalance %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(fn) 
# rm(bbalance); gc()

sum_bureau <- bureau %>% 
  left_join(sum_bbalance, by = "SK_ID_BUREAU") %>% 
  select(-SK_ID_BUREAU) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
# rm(bureau, sum_bbalance); gc()

sum_cc_balance <- cc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
# rm(cc_balance); gc()

sum_payments <- payments %>% 
  select(-SK_ID_PREV) %>% 
  mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
         PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
         DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
         DPD = ifelse(DPD > 0, DPD, 0),
         DBD = ifelse(DBD > 0, DBD, 0)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
# rm(payments); gc()

sum_pc_balance <- pc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
# rm(pc_balance); gc()

sum_prev <- prev %>%
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
         APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
# rm(prev); gc()

tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>% 
  select(-TARGET) %>% 
  bind_rows(te) %>%
  left_join(sum_bureau, by = "SK_ID_CURR") %>% 
  left_join(sum_cc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_payments, by = "SK_ID_CURR") %>% 
  left_join(sum_pc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_prev, by = "SK_ID_CURR") %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(na = apply(., 1, function(x) sum(is.na(x))),
         DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
         DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
         INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
         INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
         ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
         LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
         ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
         CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS, 
         CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
         INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
         SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
         CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
         CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
         PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
         PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED) 

docs <- str_subset(names(tr), "FLAG_DOC")
live <- str_subset(names(tr), "(?!NFLAG_)(?!FLAG_DOC)(?!_FLAG_)FLAG_")
inc_by_org <- tr_te %>% 
  group_by(ORGANIZATION_TYPE) %>% 
  summarise(m = median(AMT_INCOME_TOTAL)) %$% 
  setNames(as.list(m), ORGANIZATION_TYPE)

# rm(tr, te, fn, sum_bureau, sum_cc_balance, 
#    sum_payments, sum_pc_balance, sum_prev); gc()

tr_te %<>% 
  mutate(DOC_IND_KURT = apply(tr_te[, docs], 1, moments::kurtosis),
         LIVE_IND_SUM = apply(tr_te[, live], 1, sum),
         NEW_INC_BY_ORG = recode(tr_te$ORGANIZATION_TYPE, !!!inc_by_org),
         NEW_EXT_SOURCES_MEAN = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
         NEW_SCORES_STD = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd))%>%
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) 
# select(features %>% filter(Gain > 0.003) %>% pull(Feature)) %>%  # less variable scored less
  # data.matrix()

#---------------------------
cat("Preparing data...\n")
test <- tr_te[-tri, ]
train <- tr_te[tri, ] 
train %<>%
  mutate(TARGET = fct_recode(as.factor(y), defaulter = "1", repayer = "0")) %>% 

feature_importance <- read_csv("feature_importance.csv")

train %>%
  select(feature_importance %>% filter(Gain > 0.003) %>% pull(Feature)) %>% 
  select(train %>%  sapply(., FUN = function(x) sum(is.na(x))/length(x)) %>% .[.<0.5] %>% names) %>% 
  as.data.frame() %>% 
  DMwR::knnImputation()








  
index <- createDataPartition(train$TARGET, p=0.75, list=FALSE)
train_set <- train[index,]
test_set <- train[-index,]

fit_ctrl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)


#Training the random forest model
model_rf <- caret::train(train_set %>% select(-TARGET), train_set$TARGET,method='rf', metric = "ROC", trControl = fit_ctrl)


#h2o random forest
library(h2o)
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "16G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

tr_h2o <- as.h2o(train_set)
te_h2o <- as.h2o(test_set)

predictors <- feature_importance %>% filter(Gain > 0.003) %>% pull(Feature)


model_rf <- h2o::h2o.randomForest(x = predictors,
                                  y = "TARGET", 
                                  training_frame = tr_h2o, 
                                  validation_frame = te_h2o,
                                  ntrees = 500
                                  )

summary(model_rf)

model_rf@model$validation_metrics 





gbm1 <- h2o.gbm(
  x = predictors,
  y = "TARGET", 
  training_frame = tr_h2o, 
  validation_frame = te_h2o,   
  ntrees = 500,                ## decrease the trees, mostly to allow for run time
  learn_rate = 0.2,           ## increase the learning rate (from 0.1)
  max_depth = 10,             ## increase the depth (from 5)
  seed = 2000000)                ## Set the random seed for reproducability

summary(gbm1)

finalRf_predictions <- h2o.predict(
  object = gbm1,
  newdata = as.h2o(test))


#Predicting using random forest model
testSet$pred_rf <- predict(object = model_rf,testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$Loan_Status,testSet$pred_rf)





tri <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = tr_te[tri, ], label = y[tri])
dval <- xgb.DMatrix(data = tr_te[-tri, ], label = y[-tri])
cols <- colnames(tr_te)



#---------------------------
read_csv("input/sample_submission.csv.zip") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb, dtest)) %>%
  write_csv(paste0("tidy_xgb_less_vars_", round(m_xgb$best_score, 5), ".csv"))


read_csv("input/sample_submission.csv.zip") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = as.data.frame(finalRf_predictions) %>% pull(defaulter)) %>%
  write_csv(paste0("h2o_gbm_", round(gbm1@model$validation_metrics@metrics$AUC, 5), ".csv"))




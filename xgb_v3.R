library(tidyverse)
library(xgboost)
library(magrittr)
library(futile.logger)
library(caret)
set.seed(0)

#---------------------------
futile.logger::flog.info("Loading data...")

bbalance <- read_csv("input/bureau_balance.csv.zip") 
bureau <- read_csv("input/bureau.csv.zip")
cc_balance <- read_csv("input/credit_card_balance.csv.zip")
payments <- read_csv("input/installments_payments.csv.zip") 
pc_balance <- read_csv("input/POS_CASH_balance.csv.zip")
prev <- read_csv("input/previous_application.csv.zip")
tr <- read_csv("input/application_train.csv.zip") 
te <- read_csv("input/application_test.csv.zip")

winsor <- function (x, multiple = 3){
  
  if (length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  med <- median(x, na.rm = T)
  y <- x - med
  sc <- mad(y, center = 0) * multiple
  y[y > sc] <- sc
  y[y < -sc] <- -sc
  y + med
}



#---------------------------
futile.logger::flog.info("Preprocessing and feature engineering...")

# 
# tr %<>% 
#   bind_rows(tr %>% filter(TARGET == 1))

#I commented out ones that kxx used for creating new variables (e.g. AMT_CREDIT / AMT_ANNUITY)
#I am forced to do this because the score is poor if I don't.
# 
# tr$AMT_INCOME_TOTAL <- winsor(tr$AMT_INCOME_TOTAL)
# tr$AMT_CREDIT <- winsor(tr$AMT_CREDIT)
# #tr$AMT_ANNUITY <- log1p(tr$AMT_ANNUITY)
# #tr$AMT_GOODS_PRICE <- log1p(tr$AMT_GOODS_PRICE)
# tr$REGION_POPULATION_RELATIVE <- sqrt(tr$REGION_POPULATION_RELATIVE)
# #tr$DAYS_BIRTH <- sqrt(abs(tr$DAYS_BIRTH))
# tr$DAYS_EMPLOYED <- winsor(tr$DAYS_EMPLOYED)
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
# te$AMT_INCOME_TOTAL <- winsor(te$AMT_INCOME_TOTAL)
# te$AMT_CREDIT <- winsor(te$AMT_CREDIT)
# #te$AMT_ANNUITY <- log1p(te$AMT_ANNUITY)
# #te$AMT_GOODS_PRICE <- log1p(te$AMT_GOODS_PRICE)
# te$REGION_POPULATION_RELATIVE <- sqrt(te$REGION_POPULATION_RELATIVE)
# #te$DAYS_BIRTH <- sqrt(abs(te$DAYS_BIRTH))
# te$DAYS_EMPLOYED <- winsor(te$DAYS_EMPLOYED)
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
         DBD = ifelse(DBD > 0, DBD, 0),
         NUM_LESS_PAYMENT = ifelse(PAYMENT_DIFF>=0, 1,0)
         ) %>% 
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
         APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT
         
         ) %>% 
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
         PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED
         ) 

docs <- str_subset(names(tr), "FLAG_DOC")
live <- str_subset(names(tr), "(?!NFLAG_)(?!FLAG_DOC)(?!_FLAG_)FLAG_")
inc_by_org <- tr_te %>% 
  group_by(ORGANIZATION_TYPE) %>% 
  summarise(m = median(AMT_INCOME_TOTAL)) %$% 
  setNames(as.list(m), ORGANIZATION_TYPE)

# rm(tr, te, fn, sum_bureau, sum_cc_balance, 
#    sum_payments, sum_pc_balance, sum_prev); gc()

# tr_te_to_imp <- tr_te %>% 
#   select(EXT_SOURCE_1, EXT_SOURCE_2,EXT_SOURCE_3) 
# 
# knn_imp <- caret::preProcess(tr_te_to_imp, method = c("knnImpute"))
# 
# 
# tr_te_imp <- predict(xxx, tr_te_to_imp)
# tr_te_imp_knn <- predict(knn_imp, tr_te_to_imp)
# 
# df <- bind_rows(tr_te_imp, tr_te_to_imp %>% 
#             filter(is.na(EXT_SOURCE_1),is.na(EXT_SOURCE_2),is.na(EXT_SOURCE_3))
# )
# 
# Sys.setenv("PKG_CXXFLAGS"="-std=c++0x")
# # devtools::install_github("alexwhitworth/imputation")
# 
# library("imputation")
# t <- Sys.time()
# tr_te_imp <- imputation::kNN_impute(tr_te_to_imp %>%
#                                       top_n(100000),
#                                     k = 5, 
#                                     q= 2,
#                                     verbose= FALSE,
#                                     parallel= FALSE,
#                                     n_canopies= 20)
# Sys.time() - t
# bind_cols(tr_te_to_imp %>% select(ID), tr_te_imp)


tr_te %<>%
  # bind_cols(tr_te_imp) %>% 
  mutate(DOC_IND_KURT = apply(tr_te[, docs], 1, moments::kurtosis),
         LIVE_IND_SUM = apply(tr_te[, live], 1, sum),
         NEW_INC_BY_ORG = recode(tr_te$ORGANIZATION_TYPE, !!!inc_by_org),
         NEW_EXT_SOURCES_MEAN = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
         NEW_SCORES_STD = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd),
         EXT2X3 = EXT_SOURCE_2*EXT_SOURCE_3,
         EXT1X2X3 = EXT_SOURCE_1*EXT_SOURCE_2*EXT_SOURCE_3,
         EXT2X3XD = EXT_SOURCE_1 * EXT_SOURCE_2 *DAYS_BIRTH,
         EXT22X3 = EXT_SOURCE_2^2 *EXT_SOURCE_3,
         EXT2X32 = EXT_SOURCE_2*EXT_SOURCE_3^2,
         EXT1X2 = EXT_SOURCE_1*EXT_SOURCE_2,
         EXT1X3 = EXT_SOURCE_1*EXT_SOURCE_3,
         EXT2XD = EXT_SOURCE_2*DAYS_BIRTH,
         EXT1X22 = EXT_SOURCE_1*EXT_SOURCE_2^2,
         DAYS_BIRTH_2 = DAYS_BIRTH^2,
         DAYS_BIRTH_3 = DAYS_BIRTH^3,
         CREDIT_INCOME_PERCENT = AMT_CREDIT/AMT_INCOME_TOTAL,
         CREDIT_TERM = AMT_CREDIT/AMT_ANNUITY
  )%>%
  mutate(
    NEW_EXT_SOURCES_MEAN_sqrt = sqrt(NEW_EXT_SOURCES_MEAN),SOURCES_PROD_LOG =log(SOURCES_PROD/mean(SOURCES_PROD, na.rm = T)+0.1),EXT1X2X3_LOG =log(EXT1X2X3/mean(EXT1X2X3, na.rm = T)+0.1),EXT1X3_LOG =log(EXT1X3/mean(EXT1X3, na.rm = T)+0.1),SOURCES_PROD_sqrt = sqrt(SOURCES_PROD),EXT1X2X3_sqrt = sqrt(EXT1X2X3),EXT2X32_LOG =log(EXT2X32/mean(EXT2X32, na.rm = T)+0.1),EXT2X3_sqrt = sqrt(EXT2X3),NEW_EXT_SOURCES_MEAN_sq = NEW_EXT_SOURCES_MEAN^2,EXT1X3_sqrt = sqrt(EXT1X3),EXT22X3_LOG =log(EXT22X3/mean(EXT22X3, na.rm = T)+0.1),EXT2X32_sqrt = sqrt(EXT2X32),EXT22X3_sqrt = sqrt(EXT22X3),NEW_EXT_SOURCES_MEAN_cube = NEW_EXT_SOURCES_MEAN^3,EXT1X2_sqrt = sqrt(EXT1X2),EXT1X22_LOG =log(EXT1X22/mean(EXT1X22, na.rm = T)+0.1),EXT_SOURCE_3_sqrt = sqrt(EXT_SOURCE_3),EXT1X22_sqrt = sqrt(EXT1X22),EXT2X3XD_sqrt = sqrt(EXT2X3XD),EXT2XD_sqrt = sqrt(EXT2XD),EXT_SOURCE_2_sqrt = sqrt(EXT_SOURCE_2),EXT2X3_sq = EXT2X3^2,EXT_SOURCE_1_sqrt = sqrt(EXT_SOURCE_1),EXT_SOURCE_3_sq = EXT_SOURCE_3^2,EXT_SOURCE_2_sq = EXT_SOURCE_2^2,EXT1X3_sq = EXT1X3^2,EXT1X2_sq = EXT1X2^2,EXT_SOURCE_3_cube = EXT_SOURCE_3^3,EXT22X3_sq = EXT22X3^2,EXT_SOURCE_2_cube = EXT_SOURCE_2^3,EXT_SOURCE_1_sq = EXT_SOURCE_1^2,EXT2X3_cube = EXT2X3^3,SOURCES_PROD_sq = SOURCES_PROD^2,EXT1X2X3_sq = EXT1X2X3^2,EXT2X32_sq = EXT2X32^2,EXT2XD_sq = EXT2XD^2,EXT1X22_sq = EXT1X22^2,CNT_DRAWINGS_CURRENT_mean_LOG =log(CNT_DRAWINGS_CURRENT_mean/mean(CNT_DRAWINGS_CURRENT_mean, na.rm = T)+0.1),EXT_SOURCE_1_cube = EXT_SOURCE_1^3,CNT_DRAWINGS_CURRENT_sd_sqrt = sqrt(CNT_DRAWINGS_CURRENT_sd),CNT_DRAWINGS_CURRENT_sd_LOG =log(CNT_DRAWINGS_CURRENT_sd/mean(CNT_DRAWINGS_CURRENT_sd, na.rm = T)+0.1),EXT1X2_cube = EXT1X2^3,EXT1X3_cube = EXT1X3^3,CNT_DRAWINGS_ATM_CURRENT_mean_sqrt = sqrt(CNT_DRAWINGS_ATM_CURRENT_mean),CNT_DRAWINGS_CURRENT_mean_sqrt = sqrt(CNT_DRAWINGS_CURRENT_mean),EXT22X3_cube = EXT22X3^3,CNT_DRAWINGS_CURRENT_max_sqrt = sqrt(CNT_DRAWINGS_CURRENT_max),EXT2XD_cube = EXT2XD^3,CNT_DRAWINGS_ATM_CURRENT_mean_LOG =log(CNT_DRAWINGS_ATM_CURRENT_mean/mean(CNT_DRAWINGS_ATM_CURRENT_mean, na.rm = T)+0.1),EXT2X3XD_sq = EXT2X3XD^2,CNT_DRAWINGS_CURRENT_max_LOG =log(CNT_DRAWINGS_CURRENT_max/mean(CNT_DRAWINGS_CURRENT_max, na.rm = T)+0.1),EXT2X32_cube = EXT2X32^3,SOURCES_PROD_cube = SOURCES_PROD^3,EXT1X2X3_cube = EXT1X2X3^3,AMT_DRAWINGS_CURRENT_mean_LOG =log(AMT_DRAWINGS_CURRENT_mean/mean(AMT_DRAWINGS_CURRENT_mean, na.rm = T)+0.1),EXT1X22_cube = EXT1X22^3,AMT_BALANCE_mean_LOG =log(AMT_BALANCE_mean/mean(AMT_BALANCE_mean, na.rm = T)+0.1),AMT_TOTAL_RECEIVABLE_mean_LOG =log(AMT_TOTAL_RECEIVABLE_mean/mean(AMT_TOTAL_RECEIVABLE_mean, na.rm = T)+0.1),AMT_RECIVABLE_mean_LOG =log(AMT_RECIVABLE_mean/mean(AMT_RECIVABLE_mean, na.rm = T)+0.1),AMT_RECEIVABLE_PRINCIPAL_mean_LOG =log(AMT_RECEIVABLE_PRINCIPAL_mean/mean(AMT_RECEIVABLE_PRINCIPAL_mean, na.rm = T)+0.1),AMT_BALANCE_mean_sqrt = sqrt(AMT_BALANCE_mean),AMT_TOTAL_RECEIVABLE_mean_sqrt = sqrt(AMT_TOTAL_RECEIVABLE_mean),AMT_RECIVABLE_mean_sqrt = sqrt(AMT_RECIVABLE_mean),AMT_RECEIVABLE_PRINCIPAL_mean_sqrt = sqrt(AMT_RECEIVABLE_PRINCIPAL_mean),CNT_DRAWINGS_ATM_CURRENT_sd_sqrt = sqrt(CNT_DRAWINGS_ATM_CURRENT_sd)
    ) %>% 
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
  select(read_csv("feature_importance_2.csv") %>% filter(Gain > 0.0003) %>% pull(Feature)) %>% 
  data.matrix()


#---------------------------
futile.logger::flog.info("Preparing data for model (separate train and test, etc)...")

dtest <- xgb.DMatrix(data = tr_te[-tri, ])
tr_te <- tr_te[tri, ]
tri <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = tr_te[tri, ], label = y[tri])
dval <- xgb.DMatrix(data = tr_te[-tri, ], label = y[-tri])
cols <- colnames(tr_te)



#---------------------------
cat("Training model...\n")
p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 14,
          eta = 0.025,
          max_depth = 7,
          min_child_weight = 10,
          gamma = 0,
          subsample = 0.85,
          colsample_bytree = 0.7,
          colsample_bylevel = 0.6,
          alpha = 0,
          lambda = 0,
          #scale_pos_weight = 2,
          nrounds = 2000)

set.seed(0)
m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 300)

# xgb.importance(model=xgb_v2_0.78779) %>% write_csv(.,path = "feature_importance_2.csv")

saveRDS(m_xgb, paste0("xgb_v3_", round(m_xgb$best_score, 5), ".RDS"))

#---------------------------
read_csv("input/sample_submission.csv.zip") %>%
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb, dtest)) %>%
  write_csv(paste0("sub_xgb_v3_", round(m_xgb$best_score, 5), ".csv"))






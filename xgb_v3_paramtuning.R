library(tidyverse)
library(xgboost)
library(magrittr)
library(futile.logger)
library(caret)
set.seed(0)

#---------------------------
futile.logger::flog.info("Loading data...")
# SaveRDS(tr_te, "tr_te.RDS")
tr <- read_csv("input/application_train.csv.zip") 
tri <- 1:nrow(tr)
y <- tr$TARGET
tr_te <- readRDS("tr_te.RDS")
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
          min_child_weight = 15,
          gamma = 0,
          subsample = 0.85,
          colsample_bytree = 0.6,
          colsample_bylevel = 0.6,
          alpha = 0,
          lambda = 0,
          # scale_pos_weight = 1.2,
          nrounds = 2000)

set.seed(0)


cv <- xgb.cv(params = p, data = dtrain, nrounds = p$nrounds,nfold = 5,verbose = T)




m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 300)

# xgb.importance(model=xgb_v2_0.78779) %>% write_csv(.,path = "feature_importance_2.csv")

saveRDS(m_xgb, paste0("xgb_v3_", round(m_xgb$best_score, 5), ".RDS"))

#---------------------------
read_csv("input/sample_submission.csv.zip") %>%
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb, dtest)) %>%
  write_csv(paste0("sub_xgb_v3_", round(m_xgb$best_score, 5), ".csv"))






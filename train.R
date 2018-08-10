library(tidyverse)
library(xgboost)
library(magrittr)
library(futile.logger)
set.seed(0)

#---------------------------
tr <- read_csv("input/application_train.csv.zip") 
te <- read_csv("input/application_test.csv.zip")

bureau <- read_csv("input/bureau.csv.zip") %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

cred_card_bal <-  read_csv("input/credit_card_balance.csv.zip") %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

pos_cash_bal <- read_csv("input/POS_CASH_balance.csv.zip") %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

prev <- read_csv("input/previous_application.csv.zip") %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 




# EDA ---------------------------------------------------------------------

bureau %>% filter(SK_ID_CURR == 100002) %>% View()

ggplot(tr,  aes(DAYS_BIRTH, group = TARGET, color = TARGET)) + 
  geom_density()


ggplot(tr,  aes(FLAG_OWN_CAR, group = TARGET, color = TARGET)) + 
  geom_density()

cor(tr$EXT_SOURCE_1, tr$TARGET)

ggplot(tr,  aes(CODE_GENDER, group = TARGET, color = TARGET)) + 
  geom_histogram(stat = "count", position = "dodge")

#---------------------------
cat("Preprocessing...\n")

avg_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(buro_count = bureau %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(card_count = cred_card_bal %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(pos_count = pos_cash_bal %>%  
           group_by(SK_ID_PREV, SK_ID_CURR) %>% 
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(nb_app = prev %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>% 
  select(-TARGET) %>% 
  bind_rows(te) %>%
  left_join(avg_bureau, by = "SK_ID_CURR") %>% 
  left_join(avg_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(avg_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(avg_prev, by = "SK_ID_CURR") %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  data.matrix()

# rm(tr, te, prev, avg_prev, bureau, avg_bureau, cred_card_bal, 
   # avg_cred_card_bal, pos_cash_bal, avg_pos_cash_bal)
gc()

#---------------------------
cat("Preparing data...\n")
dtest <- xgb.DMatrix(data = tr_te[-tri, ])
tr_te <- tr_te[tri, ]
tri <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()
dtrain <- xgb.DMatrix(data = tr_te[tri, ], label = y[tri])
dval <- xgb.DMatrix(data = tr_te[-tri, ], label = y[-tri])
cols <- colnames(tr_te)

rm(tr_te, y, tri); gc()

#---------------------------
cat("Training model...\n")
p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 16,
          eta = 0.025,
          max_depth = 6,
          min_child_weight = 19,
          gamma = 0,
          subsample = 0.8,
          colsample_bytree = 0.632,
          alpha = 0,
          lambda = 0.05,
          nrounds = 2000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 200)

xgb.importance(cols, model=m_xgb) %>% 
  xgb.plot.importance(top_n = 30)



#---------------------------
max_auc  <-  0
gridsearch_params <- list(max_depth = c(6:10), min_child_weight = c(15:20))


for(i in gridsearch_params$max_depth) {
  for(j in gridsearch_params$min_child_weight){
    
    flog.info(paste0("max_depth: ", i," min_child_weight: ", j))
    
    p <- list(objective = "binary:logistic",
              booster = "gbtree",
              eval_metric = "auc",
              nthread = 16,
              eta = 0.025,
              max_depth = i,
              min_child_weight = j,
              gamma = 0,
              subsample = 0.8,
              colsample_bytree = 0.632,
              alpha = 0,
              lambda = 0.05,
              nrounds = 2000)
    
    m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), verbose = 0, early_stopping_rounds = 200)
    
    flog.info(m_xgb$best_score)
    
    
  }
}




#---------------------------
read_csv("input/sample_submission.csv.zip") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb, dtest)) %>%
  write_csv(paste0("tidy_xgb_", round(m_xgb$best_score, 4), ".csv"))

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

install_pmt <- read_csv("input/installments_payments.csv.zip") %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

# tr_clean <- tr %>% 
# select(EXT_SOURCE_1,EXT_SOURCE_2, EXT_SOURCE_3,DAYS_BIRTH, 
#        DAYS_EMPLOYED, AMT_GOODS_PRICE, DAYS_ID_PUBLISH, AMT_ANNUITY,
#        AMT_CREDIT, DAYS_REGISTRATION, DAYS_EMPLOYED, AMT_CREDIT,
#        AMT_INCOME_TOTAL)


# EDA ---------------------------------------------------------------------

# CREDIT_TERM, EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3, DAYS_BIRTH, DAYS_EMPLOYED, 
# AMT_GOOD_PRICE, DAYS_ID_PUBLISH, AMT_ANNUITY, AMT_CREDIT, DAYS_REGISTRATION,
# DAYS_EMPLOYED_PERCENT, AMT_CREDIT, CNT_INSTALMENT_FUTURE, AMT_CREDIT_MAX_OVERDUE,AMT_INCOME_TOTAL,SK_ID_BUREAU
# AMT_DOWN_PAYMENT, family size? defaulter has higher annual income?  Loan annuity/amt ratio should be important?


# thoughts: family status? outside financial market status, if payment is on time
# ditch the revolving loans completely?

# bureau %>% filter(SK_ID_CURR == 100002) %>% View()
# 
# ggplot(tr,  aes(DAYS_BIRTH, group = TARGET, color = TARGET)) + 
#   geom_density()
# 
# 
# ggplot(tr,  aes(FLAG_OWN_CAR, group = TARGET, color = TARGET)) + 
#   geom_density()
# 
# cor(tr$EXT_SOURCE_1, tr$TARGET)
# 
# ggplot(tr,  aes(CODE_GENDER, group = TARGET, color = TARGET)) + 
#   geom_histogram(stat = "count", position = "dodge")
# 
# tr %>% filter(TARGET == 1,
#               AMT_INCOME_TOTAL <= 500000
#               ) %>% 
#   ggplot(aes(AMT_INCOME_TOTAL)) +
#   geom_histogram()

#---------------------------
cat("Preprocessing...\n")

avg_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE), max, min)) %>% 
  mutate(buro_count = bureau %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE), max, min)) %>% 
  mutate(card_count = cred_card_bal %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE), max, min)) %>% 
  mutate(pos_count = pos_cash_bal %>%  
           group_by(SK_ID_PREV, SK_ID_CURR) %>% 
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE), max, min)) %>% 
  mutate(nb_app = prev %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_install_pmt <- install_pmt %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE), max, min)) %>% 
  mutate(pos_count = install_pmt %>%  
           group_by(SK_ID_PREV, SK_ID_CURR) %>% 
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
  left_join(avg_install_pmt, by = "SK_ID_CURR") %>% 
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
          max_depth = 5,
          min_child_weight = 10,
          gamma = 0,
          subsample = 0.8,
          colsample_bytree = 0.8,
          alpha = 0,
          lambda = 0.05,
          nrounds = 2000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 200)

xgb.importance(cols, model=m_xgb) %>% 
  xgb.plot.importance(top_n = 30)

#---------------------------
read_csv("input/sample_submission.csv.zip") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb, dtest)) %>%
  write_csv(paste0("tidy_xgb_morefeature", round(m_xgb$best_score, 4), ".csv"))

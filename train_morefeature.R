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


tr_te %<>% 
  mutate(DOC_IND_KURT = apply(tr_te[, docs], 1, moments::kurtosis),
         LIVE_IND_SUM = apply(tr_te[, live], 1, sum),
         NEW_INC_BY_ORG = recode(tr_te$ORGANIZATION_TYPE, !!!inc_by_org),
         NEW_EXT_SOURCES_MEAN = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
         NEW_SCORES_STD = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd))%>%
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
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
          min_child_weight = 20,
          gamma = 0,
          subsample = 0.8,
          colsample_bytree = 0.7,
          alpha = 0,
          lambda = 0.05,
          scale_pos_weight = 1,
          nrounds = 2000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 200)

xgb.importance(cols, model=m_xgb) %>% 
  xgb.plot.importance(top_n = 30)

#---------------------------
read_csv("input/sample_submission.csv.zip") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_xgb, dtest)) %>%
  write_csv(paste0("tidy_xgb_morefeature", round(m_xgb$best_score, 4), ".csv"))

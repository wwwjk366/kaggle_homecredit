
out <- data.frame(
  feature = character(),
  # formula = character(),
  corr=numeric()) 



for (i in 1:ncol(df)) {
  if(i %% 50 == 0) print(i)
  names_df <- names(df)
  res <- abs(cor(df[,i]^2, y, use = "complete.obs"))
  out <- add_row(out, 
                 feature = paste0(names_df[i], "_sq = ", names_df[i], "^2"), 
                 corr = res)
  res <- abs(cor(df[,i]^3, y, use = "complete.obs"))
  out <- add_row(out, 
                 feature = paste0(names_df[i], "_cube = ", names_df[i],"^3"), 
                 corr = res)
  res <- abs(cor(sqrt(abs(df[,i])), y, use = "complete.obs"))
  out <- add_row(out, 
                 feature = paste0(names_df[i], "_sqrt = ", "sqrt(",names_df[i], ")"), corr = res)
  
  if(!is.nan(moments::skewness(df[,i], na.rm = T)) & moments::skewness(df[,i], na.rm = T)>0.5) {
    res <- abs(cor(log(df[,i]/mean(df[,i], na.rm = T)+0.1), y, use = "complete.obs"))
    out <- add_row(out, 
                   feature = paste0(names_df[i], "_LOG =", "log(",names_df[i],"/mean(", names_df[i], ", na.rm = T)+0.1)"), 
                   corr = res)
  }
  
}

out %>% arrange(-corr) %>% filter(corr > 0.1) %>% pull(feature) %>% paste(collapse=',')



# parallel ----------------------------------------------------------------

library(foreach)

cl <- parallel::makeCluster(10)
doParallel::registerDoParallel(cl)

foreach(i = 1:ncol(df), .combine = 'rbind') %dopar% {
  
  out <- data.frame(
    feature = character(),
    # formula = character(),
    corr=numeric()) 
  
  names_df <- names(df)
  res <- abs(cor(df[,i]^2, y, use = "complete.obs"))
  out <- data.frame(out, 
                 feature = paste0(names_df[i], "_sq = ", names_df[i], "^2"), 
                 corr = res)
  res <- abs(cor(df[,i]^3, y, use = "complete.obs"))
  out <- add_row(out, 
                 feature = paste0(names_df[i], "_cube = ", names_df[i],"^3"), 
                 corr = res)
  res <- abs(cor(sqrt(abs(df[,i])), y, use = "complete.obs"))
  out <- add_row(out, 
                 feature = paste0(names_df[i], "_sqrt = ", "sqrt(",names_df[i], ")"), corr = res)
  
  if(!is.nan(moments::skewness(df[,i], na.rm = T)) & moments::skewness(df[,i], na.rm = T)>0.5) {
    res <- abs(cor(log(df[,i]/mean(df[,i], na.rm = T)+0.1), y, use = "complete.obs"))
    out <- add_row(out, 
                   feature = paste0(names_df[i], "_LOG =", "log(",names_df[i],")/mean(", names_df[i], ")+0.1)"), 
                   corr = res)
    out
  }
  
}

parallel::stopCluster(cl)

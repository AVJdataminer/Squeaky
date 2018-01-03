Createsubset<-function(df,response,percent,pathout){
  #example input:test2=Createsubset(test1,'TOBACCO_RATING_APPLIED_N',.3,path)
  in_train <- caret::createDataPartition(df[['response']], p = percent, list = FALSE)
  out=df[in_train,]
  setwd(pathout)
  write.csv(out, paste("df_subset.csv",sep =""), row.names = F)
  return(out)
}

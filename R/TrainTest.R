#requires the response variable be named response
Train_test1<-function(modelpath,df,percent){
  modeldata=df
  setwd(modelpath)
  in_train <- caret::createDataPartition(modeldata[['response']], p = percent, list = FALSE)
  if(percent==1){train=modeldata} else{train=modeldata[in_train,]}
  if(percent==1){test=dplyr::sample_frac(modeldata,0.1,replace=T)}else{test=modeldata[-in_train,]}
  write.csv(train, "training.csv", row.names = F)
  write.csv(test, "testing.csv",row.names = F)  
}
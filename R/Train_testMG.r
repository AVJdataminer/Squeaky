#requires the response variable be named response
Train_testMG<-function(modelpath,percent){
  setwd(paste(modelpath,"/model_ready_data",sep=""))
  modeldata=read.csv(list.files(pattern=".csv"))
  setwd(modelpath)
  in_train <- caret::createDataPartition(modeldata[['response']], p = percent, list = FALSE)
  if(percent==1){train=modeldata} else{train=modeldata[in_train,]}
  if(percent==1){test=dplyr::sample_frac(modeldata,0.1,replace=T)}else{test=modeldata[-in_train,]}
  write.csv(train, "train.csv", row.names = F)
  write.csv(test, "test.csv",row.names = F)  
}
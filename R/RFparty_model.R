RFparty_model<-function(modelpath){
  setwd(modelpath)
  setwd("H:/modeling_data")
  train=read.csv("training.csv")
  train=dplyr::select(train, -ADV.Sub.Channel)
  test=read.csv("testing.csv")
  require(party)
  rfp=ctree(code~.,data=train)
  plot(rfp, type="simple")
  
}
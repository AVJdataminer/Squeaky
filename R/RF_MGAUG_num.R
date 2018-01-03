RF_MGAUG_num=function(modelpath, codein){
start.time <- Sys.time()
	set.seed(1234)
  setwd(modelpath)
  train=read.csv("train.csv")
	train=train[sample(nrow(train),nrow(train)*.25),]
  test=read.csv("test.csv")


start.time <- Sys.time()

  st=format(Sys.time(), "%d_%b_%Y_%H.%M")

 ##add regular RF fit to the data-------------------------------------------
  
  rfmodel=randomForest(response~.,ntree=50,data=train)

  #output figures
  setwd(paste(modelpath,"/figures",sep=""))
  st=format(Sys.time(), "%Y_%m_%d_%H.%M")
  png(paste("GINI Variable Importance RF Model.png", sep =""))
  varImpPlot(rfmodel)
  dev.off()
  
  vi=rfmodel$importance
  var_importance <- data.frame(variable=row.names(vi),
                               importance=as.vector(vi))
  var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
  vari=dplyr::arrange(var_importance, desc(importance))

  p <- ggplot(vari, aes(x=reorder(variable,importance), weight=importance, fill=variable))
  p <- p + geom_bar() +coord_flip()+ ggtitle("Variable Importance from Random Forest Fit MG")
  p <- p + xlab("Variable") + ylab("Variable Importance")
  p <- p + scale_fill_discrete(name="Variable Name")
  p=p + theme(axis.text.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title=element_text(size=16),
            plot.title=element_text(size=18))
  p=p +theme_bw()+theme(legend.position = "none")
  
  ggsave(p,filename=paste("Variable Importance RF_",st,".png", sep = ""))
  
  png(paste("RF Model_",st,".png", sep =""))
  plot(rfmodel)
  dev.off()

  cm=rfmodel$confusion
  setwd(paste(modelpath,"/output",sep=""))
  write.csv(cm,paste("Confusion matrix RF OOB_",st,".csv", sep=""))
  write.csv(vari,paste("var Importance MG_", st,".csv", sep = ""))
  
  #predict test subjects based on RF for comparison
  setwd(modelpath)
  test$pred.rf=predict(rfmodel,test)
  train$pred.rf=predict(rfmodel,train)
  write.csv(test, paste(modelpath,"/output/RF test.csv", sep =""),row.names=F)
  write.csv(train, paste(modelpath,"/output/RF train.csv", sep =""),row.names=F)
 
  
  #confusion matrix

  dz=caret::confusionMatrix(test$pred.rf,test$response)
  write.csv(dz$table, paste(modelpath,"/output/Confusion matrix for RF Test.csv",sep = ""), row.names = F)

  #create variable use counts data
  vu=varUsed(rfmodel, by.tree=F, count=T)
  vuc<-dplyr::arrange(data.frame(variable=names(dplyr::select(test,-response,-pred.rf)),usage=as.vector(vu)),desc(usage))
  write.csv(vuc, paste(modelpath,"/output/Variable usage RF model.csv",sep = ""), row.names = F)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


 setwd('H:/MoneyGuard Cross Sell/model_ready_data')
pn=read.csv(list.files(pattern='pn'))
indata=read.csv(list.files(pattern='indata.csv'))
alldata=cbind(indata,pn)
predicted=predict(rfmodel,alldata)
head(predicted)
alldata$predicted=predicted
setwd(modelpath)
write.csv(alldata, "all_pred_w_pn.csv")
dz=caret::confusionMatrix(alldata$predicted,alldata$response)
dz
}

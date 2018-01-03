RF_model=function(modelpath){
  setwd(modelpath)
  train=read.csv("training.csv")
  train=select(train, -Contract.Number)
  test=read.csv("testing.csv")
  test=select(test, -Contract.Number)
  #train$code=ifelse(train$code =="MG",1,0)
  #test$code=ifelse(test$code =="MG",1,0)


  ds=splitmix(train)
  vard=apply(ds$X.quali, 2, function(x)length(unique(x)))
  #remove variables with more than 53 catergories, limit of RF
  f1=Filter(function(x) x>53, vard)#select those variable names
  '%ni%'=Negate('%in%')
  train=subset(train,select=names(train) %ni% names(f1))
  test=subset(test,select=names(test) %ni% names(f1))

  #mtry <- tuneRF(dplyr::select(test,-code),test$code, stepFactor=0.5)
  #print(mtry)
  #best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]


  rfmodel=randomForest(code~.,ntree=50,data=train)

  #output figures
  setwd(paste(modelpath,"/figures",sep=""))
  st=format(Sys.time(), "%Y_%m_%d_%H.%M")
  png(paste("GINI Variable Importance RF Model_",st,".png", sep =""))
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
  test1=read.csv("testing.csv")
  #test1$code=ifelse(test1$code =="MG",1,0)


  test1$pred.rf=predict(rfmodel,test)
  write.csv(test1, paste(modelpath,"/output/random forest test.csv", sep =""),row.names=F)

  train1=read.csv("training.csv")
  #train1$code=ifelse(train1$code =="MG",1,0)
  train1$pred.rf=predict(rfmodel,train1)
  write.csv(train1, paste(modelpath,"/output/random forest train.csv", sep =""),row.names=F)

  #predict <- ifelse(test1$pred.rf >0.5,1,0)
  predict <- test1$pred.rf 
  
  #confusion matrix
  cm1=data.frame(table(predict,test1$code))
  #develop confusion matrix
  c1=rbind(cm1[1,3],cm1[3,3],sum(cm1[1,3],cm1[3,3]))
  c2=rbind(cm1[2,3],cm1[4,3],sum(cm1[2,3],cm1[4,3]))
  c3=rbind(sum(c1[1,1],c2[1,1]),sum(c1[2,1],c2[2,1]),sum(c1[3,1],c2[3,1]))
  d1=cbind(c1,c2,c3)
  c4=rbind(round(d1[1,1]/d1[1,3],2),"NEG.Pred",round(d1[2,2]/d1[2,3],2))
  d2=cbind(d1,c4)
  r4=rbind(d2,cbind(round(as.numeric(d2[1,1])/as.numeric(d2[3,1]),2),round(as.numeric(d2[2,2])/as.numeric(d2[2,3]),2),"-","-"))
  c01=cbind(rbind("MG","NONMG","Total", "Actual"),r4)
  r5=cbind("-","Sensitivity","Specificity","Total.Accuracy",round(sum(d1[1,1],d1[2,2])/d1[3,3],2))
  dz=data.frame(rbind(c01,r5))
  names(dz)=c("Model", "MG", "NONMG","Total", "Pos.Pred")
  dz
  #work on building an rf error matrix to match
  write.csv(dz, paste(modelpath,"/output/Confusion matrix for RF Test",st,".csv",sep = ""), row.names = F)

  #create variable use counts data
  vu=varUsed(rfmodel, by.tree=F, count=T)
  vuc<-dplyr::arrange(data.frame(variable=names(dplyr::select(test,-code)),usage=as.vector(vu)),desc(usage))
  write.csv(vuc, paste(modelpath,"/output/Variable usage RF model",st,".csv",sep = ""), row.names = F)

#could build partial dependce plots for variables high on usage and save as png
} 
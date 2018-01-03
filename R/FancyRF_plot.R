FancyRF_plot=function(modelpath){
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


# Make initial tree
	form <- as.formula(code ~ .)
	tree.1 <- rpart(form,data=train,control=rpart.control(minsplit=1,cp=0))

  setwd(paste(modelpath,"/output",sep=""))
  cpt=data.frame(tree.1$cptable) #complexity error table
  minEcp=min(cpt$CP[cpt$xerror==min(cpt$xerror)])
  #Bestcp=min(cpt$CP[(cpt$rel.error+cpt$xstd)< cpt$xerror])
  write.csv(cpt,paste("Complexity Error tree.1_", st,".csv", sep = ""))

#-------------------------------------------------------------------
#update to auto tune then output only for improved or best model perhaps
 tree.2 <- rpart(form,train,control=rpart.control(minsplit=1,cp=minEcp))			

#output figures
  setwd(paste(modelpath,"/figures",sep=""))
  st=format(Sys.time(), "%Y_%m_%d_%H.%M")
  png(paste("Big Tree RF Model_",st,".png", sep =""))
  prp(tree.2,varlen=3)
  dev.off()

  png(paste("Complexity Error Rpart Model_",st,".png", sep =""))
  plotcp(tree.2)
  dev.off()

  png(paste("Fancy Tree Plot_",st,".png", sep =""))
  rattle::fancyRpartPlot(tree.2)	
  dev.off()

  

  #variable Importance
  vi=varImp(tree.2)
  var_importance <- data.frame(variable=row.names(vi),importance=as.vector(vi$Overall))
  #var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
  vari=dplyr::arrange(var_importance, desc(importance))

  write.csv(vari, paste(modelpath,"/output/Variable Importance Rpart model",st,".csv",sep = ""), row.names = F)


  setwd(modelpath)
  test1=read.csv("testing.csv")
  #test1$code=ifelse(test1$code =="MG",1,0)

  pred.rpart=predict(tree.2,test1)
  test1=data.frame(test1,pred.rpart)
  test1$pred.rpart <- ifelse(test1$MG > 0.5,"MG","NONMG")
  test1$OMIS=ifelse(test1$code=="NONMG",ifelse(test1$pred.rpart=="MG",1,0),0)

 
  train1=read.csv("training.csv")
  #train1$code=ifelse(train1$code =="MG",1,0)
  pred.rpart=predict(tree.2,train1)
  train1=data.frame(train1,pred.rpart)
  train1$pred.rpart=ifelse(train1$MG > 0.5,"MG","NONMG")
  train1$OMIS=ifelse(train1$code=="NONMG",ifelse(train1$pred.rpart=="MG",1,0),0)
  
  #predict <- test1$pred.rpart.MG

  #confusion matrix
  cm1=data.frame(table(test1$pred.rpart,test1$code))
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
  write.csv(dz, paste(modelpath,"/output/Confusion matrix for Rpart Test",st,".csv",sep = ""), row.names=F)

 ##add regular RF fit to the data-------------------------------------------
  
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
  test1$pred.rf=predict(rfmodel,test)
  train1$pred.rf=predict(rfmodel,train)
  write.csv(test1, paste(modelpath,"/output/rpart and RF test.csv", sep =""),row.names=F)
  write.csv(train1, paste(modelpath,"/output/rpart and RF train.csv", sep =""),row.names=F)
 
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


 
}

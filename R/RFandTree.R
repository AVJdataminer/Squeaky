RFandTree=function(modelpath){
	set.seed(1234)
  setwd(modelpath)
  train=read.csv("train.csv")
  test=read.csv("test.csv")
###need to add level balancing just before modeling-------------
#writing out to csv removes the level balancing
require(tidyverse)
  	require(purrr)
  	require(dplyr)
#split df1 into factor and non-factor
	df1=train
	df2=test
	Name1='train'
	Name2='test'

out=df1%>%
    	map_df(function(x) is.factor(x))%>%
    	gather(feature,num_nulls)%>%
    	filter(num_nulls==TRUE)%>%
	print(n=50)
	list1=out$feature
#select these vars from df2 using dots
#store as new df, feed to same function rewritten to set levels for each variable
	out2=df2%>%
	select_(.dots=list1)
	out3=df1%>%
	select_(.dots=list1)
#merge the two dfs together then set levels to match merged set
	totdf=rbind(out2,out3)
	for(i in list1){
	levels(out2[[i]])<-levels(totdf[[i]])
	levels(out3[[i]])<-levels(totdf[[i]])}
#recreate non-factor data from search list
	nf=df1%>%
    	map_df(function(x) is.factor(x))%>%
    	gather(feature,num_nulls)%>%
    	filter(num_nulls==FALSE)
	list2=nf$feature
	nf2=df2%>%
	select_(.dots=list2)
	nf3=df1%>%
	select_(.dots=list2)
# put pieces back togther for output dfs
train=data.frame(nf3,out3)
test=data.frame(nf2,out2)

##Remove the more than 53 level vars------------------
  ds=splitmix(train)
  vard=apply(ds$X.quali, 2, function(x)length(unique(x)))
  f1=Filter(function(x) x>53, vard)#select those variable names
  '%ni%'=Negate('%in%')
  train=subset(train,select=names(train) %ni% names(f1))
  test=subset(test,select=names(test) %ni% names(f1))
  st=format(Sys.time(), "%d_%b_%Y_%H.%M")
 
# Make initial tree
	form <- as.formula(response ~ .)
	tree.1 <- rpart(form,data=train,control=rpart.control(minsplit=1,cp=0))

  setwd(paste(modelpath,"/output",sep=""))
  write.csv(f1,"vars_deleted_53.csv", row.names=F)
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

  setwd(modelpath)
  test1=test

  pred.rpart=predict(tree.2,test1)
  test1=data.frame(test1,pred.rpart)

  train1=train
 
  pred.rpart=predict(tree.2,train1)
  train1=data.frame(train1,pred.rpart)


  #confusion matrix
  #dz=caret::confusionMatrix(test1$pred.rpart,test1$response)
  #write.csv(dz, paste(modelpath,"/output/Confusion matrix for Rpart Test",st,".csv",sep = ""), row.names=F)

 ##add regular RF fit to the data-------------------------------------------
  
  rfmodel=randomForest(response~.,ntree=50,data=train)

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
 
  
  #confusion matrix
  #dz=caret::confusionMatrix(test1$pred.rf,test1$response)
  #write.csv(dz, paste(modelpath,"/output/Confusion matrix for RF Test",st,".csv",sep = ""), row.names = F)

  #create variable use counts data
  vu=varUsed(rfmodel, by.tree=F, count=T)
  vuc<-dplyr::arrange(data.frame(variable=names(dplyr::select(test,-response)),usage=as.vector(vu)),desc(usage))
  write.csv(vuc, paste(modelpath,"/output/Variable usage RF model",st,".csv",sep = ""), row.names = F)


 
}

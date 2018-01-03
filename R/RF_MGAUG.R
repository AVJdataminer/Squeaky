RF_MGAUG=function(modelpath, codein){
start.time <- Sys.time()
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

#matching levels introduces NA values--------------------------
#need to rerun NA fill process
setwd(codein)
source("Fill_NA.R")
train=Fill_NA(train,modelpath)
test=Fill_NA(test,modelpath)

start.time <- Sys.time()

##Remove the more than 53 level vars------------------
  ds=splitmix(train)
  vard=apply(ds$X.quali, 2, function(x)length(unique(x)))
  f1=Filter(function(x) x>53, vard)#select those variable names
  '%ni%'=Negate('%in%')
  train=subset(train,select=names(train) %ni% names(f1))
  test=subset(test,select=names(test) %ni% names(f1))
  st=format(Sys.time(), "%d_%b_%Y_%H.%M")

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
  test$pred.rf=predict(rfmodel,test)
  train$pred.rf=predict(rfmodel,train)
  write.csv(test, paste(modelpath,"/output/RF test.csv", sep =""),row.names=F)
  write.csv(train, paste(modelpath,"/output/RF train.csv", sep =""),row.names=F)
 
  
  #confusion matrix
	test$predicted=ifelse(test$pred.rf<.5,0,1)
  dz=caret::confusionMatrix(test$predicted,test$response)
  write.csv(dz$table, paste(modelpath,"/output/Confusion matrix for RF Test",st,".csv",sep = ""), row.names = F)

  #create variable use counts data
  vu=varUsed(rfmodel, by.tree=F, count=T)
  vuc<-dplyr::arrange(data.frame(variable=names(dplyr::select(test,-response)),usage=as.vector(vu)),desc(usage))
  write.csv(vuc, paste(modelpath,"/output/Variable usage RF model",st,".csv",sep = ""), row.names = F)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

library(twilio)
Sys.setenv(TWILIO_SID = "AC8ea39bd8323a436434562a263437d7ed")
Sys.setenv(TWILIO_TOKEN = "5892e7675cc89cb93439baefcd852db8")
my_phone_number <- "14062021470"
twilios_phone_number <- "14062047208"
# 
body=paste("R is done!", time.taken, sep="")
# # Now we can send away!
#tw_send_message(from = twilios_phone_number, to = my_phone_number, 
 #               body =body )

 setwd('H:/MoneyGuard Cross Sell/model_ready_data')
pn=read.csv(list.files(pattern='pn'))
indata=read.csv(list.files(pattern='indata.csv'))

x=indata
setwd(modelpath)	
  #split into numeric and non-numeric
  ds=PCAmixdata::splitmix(x)
 
  impute.med <- function(x) {
    z <- median(x, na.rm = TRUE)
    x[is.na(x)] <- z
    return(x)
  }
  
  #treat numeric NA's
  dat2 <- data.frame(sapply(ds$X.quanti, function(x){
    if(is.numeric(x) & any(is.na(x))){
      impute.med(x)
    } else {
      x
    } 
  }
  ))
  #treat factor or character Na's
  df1=lapply(ds$X.quali,forcats::fct_explicit_na)
  #paste two df's back together
  out1=cbind(dat2,df1)
alldata=cbind(out1,pn)
predicted=predict(rfmodel,alldata)
head(predicted)
alldata$predicted=predicted
setwd(modelpath)
write.csv(alldata, "all_pred_w_pn.csv")

}

Logistic_model<-function(modelpath, formula){
  setwd(modelpath)
  train=read.csv("training.csv")
trans=read.csv("datawpca.csv")
  test=read.csv("testing.csv")
train$code=ifelse(train$code =="MG",1,0)
test$code=ifelse(test$code =="MG",1,0)

#train=dplyr::select(train,code,dim1,ADV.Sub.Channel,dim4,dim5,m50inv,married,GENERS,mf50,INSURANCE_AGE,ZIP)

#test=dplyr::select(test,code,dim1,ADV.Sub.Channel,dim4,dim5,m50inv,married,GENERS,mf50,INSURANCE_AGE,ZIP)

#check for output directory and create if needed
len=nchar(getwd())
subDir=paste(substr(getwd(),1,len),"/output",sep="")
if (file.exists(subDir)){
  setwd(subDir)
} else {
  dir.create(file.path(subDir))
  setwd(subDir)
}

ptm=proc.time()

lr2=glm(formula,data=train, family=binomial)
proc.time()-ptm

#install.packages("devtools")
#require(devtools)
#install.packages("mnormt")
#install_github("dgrtwo/broom")
#library(broom)
tidyfit=broom::tidy(lr2)
st=format(Sys.time(), "%Y_%m_%d_%H.%M")
filename=paste("Logistic best_", st,".csv", sep="")

write.csv(tidyfit,paste("regression fit", st,".csv", sep =""),row.names = F)
#step <- stepAIC(lr2, direction="both")
#step$anova # display results

train$predicted.lr=ifelse(predict(lr2, newdata=train, type = 'response') >0.5,1,0)
train$probs.lr=predict(lr2,newdata=train, type='response')
st=format(Sys.time(), "%Y_%m_%d_%H.%M")
write.csv(train, paste("trainwpredicted_",st,".csv",sep=""), row.names=FALSE)
test$predicted.lr=ifelse(predict(lr2, newdata=test, type = 'response') >0.5,1,0)
test$probs.lr=predict(lr2,newdata=test, type='response')
write.csv(train, paste("testwpredicted_",st,".csv",sep =""), row.names=FALSE)

predict <- predict(lr2, newdata=test, type = 'response')

#confusion matrix
cm1=data.frame(table(predict > 0.5,test$code))
#develop confusion matrix
r1=rbind(cm1[4,3],cm1[3,3],sum(cm1[4,3],cm1[3,3]),cm1[4,3]/sum(cm1[4,3],cm1[3,3]))
r2=rbind(cm1[2,3],cm1[1,3],sum(cm1[2,3],cm1[1,3]),cm1[1,3]/sum(cm1[2,3],cm1[1,3]))
r3=rbind(sum(cm1[4,3],cm1[2,3]),sum(cm1[3,3],cm1[1,3]),sum(sum(cm1[4,3],cm1[2,3]),sum(cm1[3,3],cm1[1,3])),0)
d1=cbind(r1,r2,r3)
r4=rbind("POS.Pred",d1[1,1]/d1[1,3],"NEG.Pred",d1[2,1]/d1[2,3])
d2=cbind(d1,r4)
r5=cbind("Sensitivity","Specificity","Total.Accuracy",(sum(d1[1,1],d1[2,2]))/d1[3,3])
d2=rbind(d2,r5)
d2
#work on building an rf error matrix to match
st=format(Sys.time(), "%Y_%m_%d_%H.%M")
write.csv(d2, paste("Confusion matrix Logistic Test_", st,".csv",sep =""))

#ROCR Curve

setwd(paste(modelpath,"/figures",sep=""))
test$pred=ifelse(predict >0.5,1,0)

ROCRpred <- prediction(test$predicted, test$code)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')

png(paste("ROC for Logistic",st,".png",sep=""))
  plot(ROCRperf, colorize = FALSE, text.adj = c(-0.2,1.7))
dev.off()

misClasificError <- mean(test$predicted != test$code)
print(paste('Accuracy',1-misClasificError))

}

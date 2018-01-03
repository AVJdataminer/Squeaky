#fit linear model
setwd(modelpath)
  train=read.csv("train.csv")
  test=read.csv("test.csv")
start.time <- Sys.time()

fit=lm(response~.,data=trainb)
summary(fit)
trainb=train[sample(nrow(train),nrow(train)*.25),]
fit=lm(response~.,data=trainb)
summary(fit)

sum(train$response)
#66565
nrow(train)
#274157
NM=dplyr::filter(train,response==0)
sam=NM[sample(nrow(NM),nrow(test),]
M=dplyr::filter(train,response==1)
trainb=rbind(sam,M)
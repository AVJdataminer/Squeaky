 #main file for processing pca and running models
home<-("H:/")#change as necassary for server machine

codein<-paste(home,"/Modeling_functions",sep="")
pathin<-paste(home,"/modeling_data/original_data",sep="")
modelpath<-paste(home,"/modeling_data",sep="")

outpath<- (paste(home,"/modeling_data/outputs", sep=""))

setwd(codein)
source("Install_Load_packs.r")#checks location of code and compiles it
Install_Load_packs()


#####Update the variables to exclude for each run
setwd(pathin)
names(read.csv(list.files(pattern=".csv")))#review for variable selection   

df=read.csv(list.files(path=pathin,pattern=".csv"))

df=dplyr::select(df,-X,-AGE_RANGE,-NAME_SEX,-NAME_MARITAL,-FE,-debt,-inc,-inv,-m50dincinv,-ADV.Sub.Channel)
#subset data for function testing
in_train <- createDataPartition(df$code, p = 0.01, list = FALSE)
training <- df[ in_train,]
#save data subset for function testing
write.csv(df,paste(modelpath,"/indata_file.csv",sep = ""),row.names = F)

# Fill NA Values ----------------------------------------------------------
setwd(codein)
source("Fill_NA.r")
Fill_NA(home,modelpath)


# Create Specific PCAs ----------------------------------------------------
setwd(codein)
source("pca_spec.R")
vars=c("FAMP","HHCOMP")
outputname="Fam_pos"
pca_spec(vars, modelpath, outputname)
#generationalshopping activity
vars=c("GENERS","DESTGRP")
outputname="GenSpend"
pca_spec(vars, modelpath, outputname)
#insurance offers
vars=c("INSFINEX","MEDSUP","IRIAUTO","INSPAFUN")
outputname="InsurIndex"
pca_spec(vars, modelpath, outputname)
#money
vars=c("ESTINV30","ESTDEB30","ESTINC30","NETW30")
outputname="Financial"
pca_spec(vars, modelpath, outputname)
#location
vars=c("ZIP", "NAME_STATE")
outputname="location"
pca_spec(vars, modelpath, outputname)
#RX lists
vars=c("DRUG_PRIORITY_CODE_1", "DRUG_PRIORITY_CODE_2", "DRUG_PRIORITY_CODE_3")
outputname="RX"
pca_spec(vars, modelpath, outputname)

file_names=(list.files( pattern="_pca.csv")
newdf=do.call(cbind,lapply(file_names, read.csv))

p1=read.csv("Fam_pos_pca.csv")
p2=read.csv(file_names[2])
dim(p2)
p3=read.csv(file_names[3])
dim(p2)


# Create PCA --------------------------------------------------------------
setwd(codein)
source("pca_create.r")
pca_create(modelpath)



# Create model training and testing data ----------------------------------
#decide if pca include(datawpca) or exclude(Na_filled)
usepca=1
percent=.7
setwd(codein)
source("Train_test.r")
Train_test(modelpath,usepca,percent)


# Run Logistic Model ------------------------------------------------------
setwd(codein)
source("Logistic_model.R")
Logistic_model(modelpath)

# Run Random Forest Model -------------------------------------------------
setwd(codein)
source("RF_model.R")
RF_model(modelpath)

library(mva) > set.seed(131) > crabs.prox <- randomForest(dslcrabs, + ntree = 1000, proximity = TRUE)$proximity > crabs.mds <- cmdscale(1 - crabs.prox) > 
  plot(crabs.mds, col = c("blue", + "orange")[codes(crabs$sp)], pch = c(1, + 16)[codes(crabs$sex)], xlab="", ylab="")


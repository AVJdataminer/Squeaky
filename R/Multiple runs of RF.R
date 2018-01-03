rm(list=ls())
#run rf thru multiple PCA's
home<-("D:/data")#change as necassary for server machine

codein<-paste(home,"/Modeling_functions",sep="")
st=format(Sys.time(), "%d_%b_%Y_%H.%M")
setwd(home)
len=nchar(getwd())
subDir=paste(substr(getwd(),1,len),"/modeling_data_",st,sep="")

if (file.exists(subDir)){
  setwd(subDir)
} else {
  dir.create(file.path(subDir))
  setwd(subDir)
}

pathin<-paste(subDir,"/original_data",sep="")
ifelse(file.exists(pathin),,dir.create(file.path(pathin)))
#create output folder
modelpath<-subDir
outpath=paste(subDir,"/output",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))
#create figures folder
modelpath<-subDir
outpath=paste(subDir,"/figures",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))

setwd(codein)
source("Install_Load_packs.r")#checks location of code and compiles it
Install_Load_packs()


#####Update the variables to exclude for each run
setwd(home)
#names(read.csv(list.files(pattern="Transition class with kbm vars.csv")))#review for variable selection
#df=read.csv(list.files(path=home,pattern="Transition class with kbm vars.csv"))


names(read.csv(list.files(pattern="mdn_po.csv")))#review for variable selection
df=read.csv(list.files(path=home,pattern="mdn_po.csv"))


df=dplyr::select(df,-Product.Subgroup.Code,-AGE_RANGE,-NAME_SEX,-NAME_MARITAL,-FE,-debt,-inc,-inv,-m50dincinv, -ADV.Sub.Channel)
df=dplyr::select(df, -DEDUPE_ID)
write.csv(df,paste(modelpath,"/indata_file.csv",sep = ""),row.names = F)
setwd(codein)


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

# Create PCA --------------------------------------------------------------
setwd(codein)
source("pca_create.r")
pca_create(modelpath)

#implement a dataset with policy numbers for id later

# Create model training and testing data ----------------------------------

usepca=0
percent=.7

financial=0
insur=1
Fam_pos=0
GenSpend=0
rx=0
location=0
setwd(codein)
source("Train_test.r")
Train_test(modelpath,usepca,percent,financial,insur,Fam_pos,GenSpend,rx,location)



# Run Random Forest Model -------------------------------------------------
setwd(codein)
source("RF_model.R")
RF_model(modelpath)

#########Fam_pos############################################
rm(list=ls())
home<-("D:/data")#change as necassary for server machine

codein<-paste(home,"/Modeling_functions",sep="")
st=format(Sys.time(), "%d_%b_%Y_%H.%M")
setwd(home)
len=nchar(getwd())
subDir=paste(substr(getwd(),1,len),"/modeling_data_",st,sep="")

if (file.exists(subDir)){
  setwd(subDir)
} else {
  dir.create(file.path(subDir))
  setwd(subDir)
}

pathin<-paste(subDir,"/original_data",sep="")
ifelse(file.exists(pathin),,dir.create(file.path(pathin)))
#create output folder
modelpath<-subDir
outpath=paste(subDir,"/output",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))
#create figures folder
modelpath<-subDir
outpath=paste(subDir,"/figures",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))

setwd(codein)
source("Install_Load_packs.r")#checks location of code and compiles it
Install_Load_packs()


#####Update the variables to exclude for each run
setwd(home)
#names(read.csv(list.files(pattern="Transition class with kbm vars.csv")))#review for variable selection
#df=read.csv(list.files(path=home,pattern="Transition class with kbm vars.csv"))


names(read.csv(list.files(pattern="mdn_po.csv")))#review for variable selection
df=read.csv(list.files(path=home,pattern="mdn_po.csv"))


df=dplyr::select(df,-Product.Subgroup.Code,-AGE_RANGE,-NAME_SEX,-NAME_MARITAL,-FE,-debt,-inc,-inv,-m50dincinv, -ADV.Sub.Channel)
df=dplyr::select(df, -DEDUPE_ID)
write.csv(df,paste(modelpath,"/indata_file.csv",sep = ""),row.names = F)
setwd(codein)


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

# Create PCA --------------------------------------------------------------
setwd(codein)
source("pca_create.r")
pca_create(modelpath)

#implement a dataset with policy numbers for id later

# Create model training and testing data ----------------------------------

usepca=0
percent=.7

financial=0
insur=0
Fam_pos=1
GenSpend=0
rx=0
location=0
setwd(codein)
source("Train_test.r")
Train_test(modelpath,usepca,percent,financial,insur,Fam_pos,GenSpend,rx,location)



# Run Random Forest Model -------------------------------------------------
setwd(codein)
source("RF_model.R")
RF_model(modelpath)

############GenSpend###########
rm(list=ls())
home<-("D:/data")#change as necassary for server machine

codein<-paste(home,"/Modeling_functions",sep="")
st=format(Sys.time(), "%d_%b_%Y_%H.%M")
setwd(home)
len=nchar(getwd())
subDir=paste(substr(getwd(),1,len),"/modeling_data_",st,sep="")

if (file.exists(subDir)){
  setwd(subDir)
} else {
  dir.create(file.path(subDir))
  setwd(subDir)
}

pathin<-paste(subDir,"/original_data",sep="")
ifelse(file.exists(pathin),,dir.create(file.path(pathin)))
#create output folder
modelpath<-subDir
outpath=paste(subDir,"/output",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))
#create figures folder
modelpath<-subDir
outpath=paste(subDir,"/figures",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))

setwd(codein)
source("Install_Load_packs.r")#checks location of code and compiles it
Install_Load_packs()


#####Update the variables to exclude for each run
setwd(home)
#names(read.csv(list.files(pattern="Transition class with kbm vars.csv")))#review for variable selection
#df=read.csv(list.files(path=home,pattern="Transition class with kbm vars.csv"))


names(read.csv(list.files(pattern="mdn_po.csv")))#review for variable selection
df=read.csv(list.files(path=home,pattern="mdn_po.csv"))


df=dplyr::select(df,-Product.Subgroup.Code,-AGE_RANGE,-NAME_SEX,-NAME_MARITAL,-FE,-debt,-inc,-inv,-m50dincinv, -ADV.Sub.Channel)
df=dplyr::select(df, -DEDUPE_ID)
write.csv(df,paste(modelpath,"/indata_file.csv",sep = ""),row.names = F)
setwd(codein)


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

# Create PCA --------------------------------------------------------------
setwd(codein)
source("pca_create.r")
pca_create(modelpath)

#implement a dataset with policy numbers for id later

# Create model training and testing data ----------------------------------

usepca=0
percent=.7

financial=0
insur=0
Fam_pos=0
GenSpend=1
rx=0
location=0
setwd(codein)
source("Train_test.r")
Train_test(modelpath,usepca,percent,financial,insur,Fam_pos,GenSpend,rx,location)



# Run Random Forest Model -------------------------------------------------
setwd(codein)
source("RF_model.R")
RF_model(modelpath)
#########################RX##########################
rm(list=ls())
home<-("D:/data")#change as necassary for server machine

codein<-paste(home,"/Modeling_functions",sep="")
st=format(Sys.time(), "%d_%b_%Y_%H.%M")
setwd(home)
len=nchar(getwd())
subDir=paste(substr(getwd(),1,len),"/modeling_data_",st,sep="")

if (file.exists(subDir)){
  setwd(subDir)
} else {
  dir.create(file.path(subDir))
  setwd(subDir)
}

pathin<-paste(subDir,"/original_data",sep="")
ifelse(file.exists(pathin),,dir.create(file.path(pathin)))
#create output folder
modelpath<-subDir
outpath=paste(subDir,"/output",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))
#create figures folder
modelpath<-subDir
outpath=paste(subDir,"/figures",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))

setwd(codein)
source("Install_Load_packs.r")#checks location of code and compiles it
Install_Load_packs()


#####Update the variables to exclude for each run
setwd(home)
#names(read.csv(list.files(pattern="Transition class with kbm vars.csv")))#review for variable selection
#df=read.csv(list.files(path=home,pattern="Transition class with kbm vars.csv"))


names(read.csv(list.files(pattern="mdn_po.csv")))#review for variable selection
df=read.csv(list.files(path=home,pattern="mdn_po.csv"))


df=dplyr::select(df,-Product.Subgroup.Code,-AGE_RANGE,-NAME_SEX,-NAME_MARITAL,-FE,-debt,-inc,-inv,-m50dincinv, -ADV.Sub.Channel)
df=dplyr::select(df, -DEDUPE_ID)
write.csv(df,paste(modelpath,"/indata_file.csv",sep = ""),row.names = F)
setwd(codein)


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

# Create PCA --------------------------------------------------------------
setwd(codein)
source("pca_create.r")
pca_create(modelpath)

#implement a dataset with policy numbers for id later

# Create model training and testing data ----------------------------------

usepca=0
percent=.7

financial=0
insur=0
Fam_pos=0
GenSpend=0
rx=1
location=0
setwd(codein)
source("Train_test.r")
Train_test(modelpath,usepca,percent,financial,insur,Fam_pos,GenSpend,rx,location)



# Run Random Forest Model -------------------------------------------------
setwd(codein)
source("RF_model.R")
RF_model(modelpath)
#############################location###################
rm(list=ls())
home<-("D:/data")#change as necassary for server machine

codein<-paste(home,"/Modeling_functions",sep="")
st=format(Sys.time(), "%d_%b_%Y_%H.%M")
setwd(home)
len=nchar(getwd())
subDir=paste(substr(getwd(),1,len),"/modeling_data_",st,sep="")

if (file.exists(subDir)){
  setwd(subDir)
} else {
  dir.create(file.path(subDir))
  setwd(subDir)
}

pathin<-paste(subDir,"/original_data",sep="")
ifelse(file.exists(pathin),,dir.create(file.path(pathin)))
#create output folder
modelpath<-subDir
outpath=paste(subDir,"/output",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))
#create figures folder
modelpath<-subDir
outpath=paste(subDir,"/figures",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))

setwd(codein)
source("Install_Load_packs.r")#checks location of code and compiles it
Install_Load_packs()


#####Update the variables to exclude for each run
setwd(home)
#names(read.csv(list.files(pattern="Transition class with kbm vars.csv")))#review for variable selection
#df=read.csv(list.files(path=home,pattern="Transition class with kbm vars.csv"))


names(read.csv(list.files(pattern="mdn_po.csv")))#review for variable selection
df=read.csv(list.files(path=home,pattern="mdn_po.csv"))


df=dplyr::select(df,-Product.Subgroup.Code,-AGE_RANGE,-NAME_SEX,-NAME_MARITAL,-FE,-debt,-inc,-inv,-m50dincinv, -ADV.Sub.Channel)
df=dplyr::select(df, -DEDUPE_ID)
write.csv(df,paste(modelpath,"/indata_file.csv",sep = ""),row.names = F)
setwd(codein)


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

# Create PCA --------------------------------------------------------------
setwd(codein)
source("pca_create.r")
pca_create(modelpath)

#implement a dataset with policy numbers for id later

# Create model training and testing data ----------------------------------

usepca=0
percent=.7

financial=0
insur=0
Fam_pos=0
GenSpend=0
rx=0
location=1
setwd(codein)
source("Train_test.r")
Train_test(modelpath,usepca,percent,financial,insur,Fam_pos,GenSpend,rx,location)



# Run Random Forest Model -------------------------------------------------
setwd(codein)
source("RF_model.R")
RF_model(modelpath)

#######################run the PCA with Rx model on all the data##
rm(list=ls())
home<-("D:/data")#change as necassary for server machine

codein<-paste(home,"/Modeling_functions",sep="")
st=format(Sys.time(), "%d_%b_%Y_%H.%M")
setwd(home)
len=nchar(getwd())
subDir=paste(substr(getwd(),1,len),"/modeling_data_",st,sep="")

if (file.exists(subDir)){
  setwd(subDir)
} else {
  dir.create(file.path(subDir))
  setwd(subDir)
}

pathin<-paste(subDir,"/original_data",sep="")
ifelse(file.exists(pathin),,dir.create(file.path(pathin)))
#create output folder
modelpath<-subDir
outpath=paste(subDir,"/output",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))
#create figures folder
modelpath<-subDir
outpath=paste(subDir,"/figures",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))

setwd(codein)
source("Install_Load_packs.r")#checks location of code and compiles it
Install_Load_packs()


#####Update the variables to exclude for each run
setwd(home)
#names(read.csv(list.files(pattern="Transition class with kbm vars.csv")))#review for variable selection
#df=read.csv(list.files(path=home,pattern="Transition class with kbm vars.csv"))


names(read.csv(list.files(pattern="mdn_po.csv")))#review for variable selection
df=read.csv(list.files(path=home,pattern="mdn_po.csv"))


df=dplyr::select(df,-Product.Subgroup.Code,-AGE_RANGE,-NAME_SEX,-NAME_MARITAL,-FE,-debt,-inc,-inv,-m50dincinv, -ADV.Sub.Channel)
df=dplyr::select(df, -DEDUPE_ID)
write.csv(df,paste(modelpath,"/indata_file.csv",sep = ""),row.names = F)
setwd(codein)


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

# Create PCA --------------------------------------------------------------
setwd(codein)
source("pca_create.r")
pca_create(modelpath)

#implement a dataset with policy numbers for id later

# Create model training and testing data ----------------------------------

usepca=0
percent=1

financial=0
insur=0
Fam_pos=0
GenSpend=0
rx=1
location=0
setwd(codein)
source("Train_test.r")
Train_test(modelpath,usepca,percent,financial,insur,Fam_pos,GenSpend,rx,location)



# Run Random Forest Model -------------------------------------------------
setwd(codein)
source("RF_model.R")
RF_model(modelpath)
setwd(codein)
source("FancyRF_plot.R")
FancyRF_plot(modelpath)

####financial PCA#############################
rm(list=ls())
#run rf thru multiple PCA's
home<-("D:/data")#change as necassary for server machine

codein<-paste(home,"/Modeling_functions",sep="")
st=format(Sys.time(), "%d_%b_%Y_%H.%M")
setwd(home)
len=nchar(getwd())
subDir=paste(substr(getwd(),1,len),"/modeling_data_",st,sep="")

if (file.exists(subDir)){
  setwd(subDir)
} else {
  dir.create(file.path(subDir))
  setwd(subDir)
}

pathin<-paste(subDir,"/original_data",sep="")
ifelse(file.exists(pathin),,dir.create(file.path(pathin)))
#create output folder
modelpath<-subDir
outpath=paste(subDir,"/output",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))
#create figures folder
modelpath<-subDir
outpath=paste(subDir,"/figures",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))

setwd(codein)
source("Install_Load_packs.r")#checks location of code and compiles it
Install_Load_packs()


#####Update the variables to exclude for each run
setwd(home)
#names(read.csv(list.files(pattern="Transition class with kbm vars.csv")))#review for variable selection
#df=read.csv(list.files(path=home,pattern="Transition class with kbm vars.csv"))


names(read.csv(list.files(pattern="mdn_po.csv")))#review for variable selection
df=read.csv(list.files(path=home,pattern="mdn_po.csv"))


df=dplyr::select(df,-Product.Subgroup.Code,-AGE_RANGE,-NAME_SEX,-NAME_MARITAL,-FE,-debt,-inc,-inv,-m50dincinv, -ADV.Sub.Channel)
df=dplyr::select(df, -DEDUPE_ID)
write.csv(df,paste(modelpath,"/indata_file.csv",sep = ""),row.names = F)
setwd(codein)


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

# Create PCA --------------------------------------------------------------
setwd(codein)
source("pca_create.r")
pca_create(modelpath)

#implement a dataset with policy numbers for id later

# Create model training and testing data ----------------------------------

usepca=0
percent=.7

financial=1
insur=0
Fam_pos=0
GenSpend=0
rx=0
location=0
setwd(codein)
source("Train_test.r")
Train_test(modelpath,usepca,percent,financial,insur,Fam_pos,GenSpend,rx,location)



# Run Random Forest Model -------------------------------------------------
setwd(codein)
source("RF_model.R")
RF_model(modelpath)


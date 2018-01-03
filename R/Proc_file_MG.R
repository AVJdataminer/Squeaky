#main file for processing pca and running models
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


df=dplyr::select(df,-Product.Subgroup.Code,-AGE_RANGE,-NAME_SEX,-NAME_MARITAL,-FE,-debt,
		-inc,-inv,-m50dincinv, -ADV.Sub.Channel,-DRUG_PRIORITY_CODE_1,-DRUG_PRIORITY_CODE_2,-DRUG_PRIORITY_CODE_3)

write.csv(df,paste(modelpath,"/indata_file.csv",sep = ""),row.names = F)
setwd(codein)


setwd(codein)
source("Fill_NA.r")
Fill_NA(home,modelpath)
#implement a dataset with policy numbers for id later

# Create model training and testing data ----------------------------------
percent=1
setwd(codein)
source("Train_testMG.r")
Train_testMG(modelpath)

# Run Random Forest Model  and Tree modelss--------------------------------
setwd(codein)
set.seed(999)
source("FancyRF_plot.R")
FancyRF_plot(modelpath)

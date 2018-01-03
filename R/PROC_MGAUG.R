#proc_suw
#for modeling with clean ready data
rm(list=ls())#clear ws
#home<-"D:/data/MG_AUG"#change for each machine
home<-'H:/MoneyGuard Cross Sell'
codein<-"H:/Rcode/Modeling_Functions" 
setwd(codein)

#create a new dir for this modeling instance----------------
source("MakePath.R")
modelpath=MakePath(home,codein)

#Load packages----------------------------------------------
setwd(codein)
source("Install_Load_packs.r")#checks 
Install_Load_packs()

#fix some variables---------------------------------------
#setwd(paste(home,"/model_ready_data",sep=""))
#df=read.csv('indata.csv')
#indata=dplyr::select(df, -NAME_AGE,-CA)
#write.csv(indata,'indata.csv', row.names=F)

#move data to new directory---------------------------------
setwd(codein)
source("MoveR.R")
fromp=paste(home,"/model_ready_data",sep="")
top=paste(modelpath,"/model_data",sep="")
MoveR(fromp,"indata",top)

#create train and test data subsets-------------------------
setwd(codein)
source("Train_test1.R")
Train_test1(modelpath,.7)

start.time <- Sys.time()
#run the random forest and tree model -----------------------
setwd(codein)
source("RF_MGAUG.R")
RF_MGAUG(modelpath)

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
tw_send_message(from = twilios_phone_number, to = my_phone_number, 
                body =body )





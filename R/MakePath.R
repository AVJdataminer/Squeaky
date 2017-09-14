MakePath<-function(home){
#set function = modelpath
st=format(Sys.time(), "%d_%b_%Y_%H.%M")
setwd(home)
len=nchar(getwd())
subDir=paste(substr(getwd(),1,len),"/modeling_work_",st,sep="")
#check if directory exists, if not create it
if (file.exists(subDir)){
  setwd(subDir)
} else {
  dir.create(file.path(subDir))
  setwd(subDir)
}

pathin<-paste(subDir,"/model_data",sep="")
ifelse(file.exists(pathin),,dir.create(file.path(pathin)))

#create output folder
outpath=paste(subDir,"/output",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))

#create figures folder
figures=paste(subDir,"/figures",sep="")
ifelse(file.exists(figures),,dir.create(file.path(figures)))

#create reporting folder
figures=paste(subDir,"/reporting",sep="")
ifelse(file.exists(reporting),,dir.create(file.path(reporting)))
return(subDir)
}

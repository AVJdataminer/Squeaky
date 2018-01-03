MoveR<-function(frompath,infilename,topath){
  setwd(frompath)
  file=paste(infilename,".csv", sep="")
  df=read.csv(file)
  setwd(topath)
  write.csv(df,paste(infilename,"_data.csv",sep=""), row.names=F)
  print(list.files())
}
#copies a csv file from one location to another
MoveR<-function(frompath,infilename,topath){
  setwd(frompath)
  file=infilename
  df=read.csv(file)
  setwd(topath)
  write.csv(df,paste("in_data.csv",sep=""), row.names=F)
  print(list.files())
}

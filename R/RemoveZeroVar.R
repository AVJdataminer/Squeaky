RemoveZeroVar<-function(df,present){
  nzv=caret::nearZeroVar(df)
  df=df[,-nzv]
#write out the names of deleted vars
outd=df[,nzv]
write.csv(names(outd),paste(present,"/NZV_deleted.csv",sep=""))
  return(df)
}
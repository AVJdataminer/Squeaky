AddId<-function(df, modelpath){
#function to add row id and write data out to modelpath
nr=nrow(df)
df$A.ID=1:nr
write.csv(df,paste(modelpath, '/indata.id.csv', sep=""), row.names=FALSE)
return(df)
}

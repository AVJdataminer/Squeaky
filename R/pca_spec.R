pca_spec<-function(vars,pathin,outputname){
	setwd(modelpath)
  df=read.csv("Na_filled.csv")
  #df=dplyr::select(df,-Contract.Number)
  df=dplyr::select_(df, .dots=vars) #selects based on names in vars!!
  ds=splitmix(df)
  pca<-PCAmix(ds$X.quanti,ds$X.quali,rename.level =T,graph=FALSE)
	#may need to update for new filename convention
  pcdf=data.frame(pca$scores)
  nomin=names(pcdf)
  names(pcdf)=paste(outputname,".",nomin, sep="")
  write.csv(pcdf, paste(outputname,"_pca.csv", sep=""), row.names=F)
}
                   
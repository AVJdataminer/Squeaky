pca_create<-function(pathin){
  setwd(pathin)
  df=read.csv("Na_filled.csv")
  df=dplyr::select(df,-Contract.Number)
  #df=dplyr::select(df, -code, -class)
  ds=splitmix(df)
  pca<-PCAmix(ds$X.quanti,ds$X.quali,rename.level =T,graph=FALSE)
  #check for outputs directory and create if missing
  
  summary(pca) #output to data files
  df=read.csv("Na_filled.csv")
  pcadf=cbind(df,pca$scores)
  write.csv(pcadf,"datawpca.csv",row.names = F)
  
  #check if directory exists for figures, if not create on outputs location
  len=nchar(getwd())
  subDir=paste(substr(getwd(),1,len),"/figures",sep="")
  if (file.exists(subDir)){
    setwd(subDir)
  } else {
    dir.create(file.path(subDir))
    setwd(subDir)
  }
  #plot(pca,choice="ind")
  png("PCA correlation plot.png")
  plot(pca, choice="cor")
  dev.off()
  png("PCA loadings plot.png")
  plot(pca,choice="sqload",coloring.var="type")
  dev.off()
  pe=data.frame(pca$eig)
  pe$comp=1:nrow(pe)
  png("PCA Proportion of variance.png") 
  plot(pe$comp,pe$Proportion) 
  dev.off() #plot of variance by component
  png("PCA Cummulative variance.png")
  plot(pe$comp,pe$Cumulative)
  dev.off() #plot of cummulative variation
  
}
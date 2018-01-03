#function to create PCA from scaled data
#could add check function for non scaled data
df=select(outdf, -A.ID)
PCAmix<-function(df,modelpath){
  require(PCamixdata)
  
  split<-splitmix(df)
  X1 <- split$X.quanti 
  pca=prcomp(X1)
  X2 <- split$X.quali 
  PC<-data.frame(pca$x,variable=as.factor(df$response))
  PC<-data.frame(pca$x,variable=df$HP)
  ggplot(PC,aes(x=PC1,y=PC2,col=variable))+
    geom_point(size=3,alpha=0.5)+ #Size and alpha just for fun
    theme_classic()
  #which pieces we want to add to the data

  
  
}
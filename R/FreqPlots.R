#function to create freq plots for variables in data frame. Each plot is saved as a png in the specified folder or path.
# I suggest seperating factors and only using this for numeric cases. Later I will add an if statement to use the appropriate
#code for factor variable bins.

#FreqPlots(df,folder)
ds=PCAmixdata::splitmix(df)
#define:
#response=response
#theme_new
# require(ggplot2)
 FreqPlots<-function(df, folder){
   require(ggplot2)
   plotFunc <- function(x, na.rm = TRUE){
    nm <- names(x)
     for (i in seq_along(nm)) {
       plots <-ggplot(x,aes_string(x = nm[i]), fill=response) +facet_grid(~response)+
        geom_histogram(fill="darkred", color="lightgrey", position="dodge")+theme_new()+
         theme(legend.position = "none")+ ggtitle(paste(nm[i]))+
         theme(axis.text.x = element_text(angle = 0, hjust = 1))
       ggsave(filename=paste(folder,"/",nm[i],".png", sep=""),plot=plots)
     }}
  plotFunc(df)
 }
 FreqPlots(ds$X.quali,getwd())
 

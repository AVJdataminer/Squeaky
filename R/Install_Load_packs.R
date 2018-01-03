#1. check if packages in list are installed
#2. install missing packages
#3. load packages into library

Install_Load_packs<-function(path){

# List of packages for session
.packages = c("ggplot2",  "PCAmixdata","randomForest","forcats",
	"caret","rpart","mlbench","ROCR","MASS","dplyr","devtools",
	"mnormt","broom","mvtnorm","party","rattle","rpart.plot",
	"RColorBrewer","partykit","caret","tidyverse","purrr", "ReporteRs")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

}
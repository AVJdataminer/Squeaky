Train_test<-function(modelpath,usepca,percent,npca,financial,insur,Fam_pos,GenSpend,rx,location){
  setwd(modelpath)
  if(financial==1){theta=dplyr::select(data.frame(read.csv("NA_filled.csv"),read.csv("Financial_pca.csv")),
	-ESTINV30,-ESTDEB30,-ESTINC30,-NETW30)
		} 

  if(insur==1){theta=dplyr::select(data.frame(read.csv("NA_filled.csv"),read.csv("InsurIndex_pca.csv")),
	-INSFINEX,-MEDSUP,-IRIAUTO,-INSPAFUN)
		} 

  if(Fam_pos==1){theta=dplyr::select(data.frame(read.csv("NA_filled.csv"),read.csv("Fam_pos_pca.csv")),
	-FAMP,-HHCOMP)
		} 

  if(GenSpend==1){theta=dplyr::select(data.frame(read.csv("NA_filled.csv"),read.csv("GenSpend_pca.csv")),
	-GENERS, -DESTGRP)
 		}

  if(rx==1){theta=dplyr::select(data.frame(read.csv("NA_filled.csv"),read.csv("RX_pca.csv")),
	-DRUG_PRIORITY_CODE_1,-DRUG_PRIORITY_CODE_2,-DRUG_PRIORITY_CODE_3)
		} 

  if(location==1){theta=dplyr::select(data.frame(read.csv("NA_filled.csv"),read.csv("location_pca.csv")),
	-ZIP, -NAME_STATE)
		} 

  if(usepca==1){theta=read.csv("datawpca.csv")}
  if(npca==1){theta=read.csv("NA_filled.csv")}  

  modeldata=theta

  #modeldata=select(modeldata,-Contract.Number)
  in_train <- createDataPartition(modeldata$code, p = percent, list = FALSE)
  if(percent==1){train=modeldata} else{train=modeldata[in_train,]}
  if(percent==1){test=dplyr::sample_frac(modeldata,0.1,replace=T)}else{test=modeldata[-in_train,]}
  write.csv(train, "training.csv", row.names = F)
  write.csv(test, "testing.csv",row.names = F)
  
}
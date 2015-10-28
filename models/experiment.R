setwd("/Users/mark/Documents/rain2/input/")
library(data.table)
library(Metrics)
library(gbm)

## Either load the function and calculate, or load a prior version
#source("../loadFeatures.R")
#loadFeatures()  ## set to output x.csv and xTest.csv

x<-fread("x.csv")
xTest<-fread("xTest.csv")
myS<-fread("sample_solution.csv")

experiment<-function(parameterList,i,newId,x,xTest){
  library(gbm)
  library(Metrics)
  v_trees<-ifelse(is.na(parameterList[i,"trees"]),50,parameterList[i,"trees"])
  v_learn<-ifelse(is.na(parameterList[i,"learn"]),0.05,parameterList[i,"learn"])
  v_depth<-ifelse(is.na(parameterList[i,"depth"]),5,parameterList[i,"depth"])
  v_minObs<-ifelse(is.na(parameterList[i,"minObs"]),10,parameterList[i,"minObs"])
  v_rowSample<-ifelse(is.na(parameterList[i,"rowSample"]),0.7,parameterList[i,"rowSample"])
  v_colSample<-ifelse(is.na(parameterList[i,"colSample"]),0.7,parameterList[i,"colSample"])
  v_distribution<-ifelse(is.na(parameterList[i,"distribution"]),"laplace",parameterList[i,"distribution"])
  #_cvStrategy<-ifelse(is.na(parameterList[i,"distribution"]),50,parameterList[i,"distribution"])
  a<-1; b<-round(nrow(x)*0.9); c<-b+1; d<-nrow(x)
  g<-gbm.fit(x[a:b,4:ncol(x)],x$Expected[a:b],
             distribution=v_distribution,
             n.trees = v_trees,
             bag.fraction = v_rowSample,
             interaction.depth = v_depth,
             n.minobsinnode = v_minObs,
             shrinkage = v_learn)
  

  #summary(pR1)  ## want to export the importance values by runId
  v_pmin<-70  ## put these in config file
  v_pmax<-0.01  ## put these in config file

  pTest<-predict(g,xTest[,4:ncol(x)],v_trees)
  pVal<-as.data.frame(cbind(x$Expected[c:d],predict(g,x[c:d,4:ncol(x)],v_trees)))
  colnames(pVal)<-c("Expected","Predicted")
  pVal[,2]<-pmin(v_pmin,pmax(v_pmax,pVal[,2]))
  valLoss<-mae(pVal$Expected,pVal$Predicted)
  print(paste("Val loss: ",valLoss))
  parameterList[i,"val"]<-valLoss
  write.csv(pVal,paste0("cv_Id_",newId,".csv"),row.names=F)

  pTest<-as.data.frame(cbind(xTest$Id,predict(g,xTest[,4:ncol(x)],v_trees)))
  colnames(pVal)<-c("Id","Expected")
  pTest[,2]<-pmin(v_pmin,pmax(v_pmax,pTest[,2]))
  write.csv(pTest,paste0("../submission_Id_",newId,".csv"),row.names=F)
  write.csv(parameterList[i,],paste0("../parameters_",newId,".csv"))
  returnDf<-as.data.frame(cbind(i,valLoss))
  colnames(returnDf)<-c("i","validationLoss")
  return(returnDf)
}


## this needs to be some kind of loop; expect that the CSV will be continually updated
##  also want to check that it doesn't get stuck, so lastI nextI check or something

nCores<-6
library(doSNOW)
cl <- makeCluster(rep("localhost",each=nCores))  ##replace with more processes if desired
registerDoSNOW(cl)

## Loop: read the file, get the next batch of work, run it, update data in memory, overwrite file
parameterList<-read.csv("../parameters.csv",stringsAsFactors = F)
idList<-which(is.na(parameterList$val))[1:nCores]
idList<-idList[!is.na(idList)]
a<-foreach(i=idList, .combine=rbind) %dopar% experiment(parameterList,i,i,as.data.frame(x),as.data.frame(xTest))
parameterList[idList,"val"]<-a$validationLoss
write.csv(parameterList,"../parameters.csv",row.names=F)

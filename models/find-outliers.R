### The objective of this script is to predict outliers within the test set. ###

# set up
setwd("D:/Kaggles/Rain")
library(data.table)
library(readr)
library(dplyr)
library(caret)
set.seed(333)

### SET VARIABLES AND FUNCTIONS ###

# set columns to import
selection <- c("Id" 
  , "Expected"
  , "minutes_past"             
  , "radardist_km"                                                       
  , "Ref"
)

# add a flag to identify the target group
bigflag <- function(dt) {
  bigset <- filter(dt, Expected>4000 & Expected<5000)   # 2200-3500 is also a range of interest
  bigids <- unique(bigset[, Id])
  dt[, bigflag := 0]
  dt[Id %in% bigids, bigflag := 1]
}

# identify IDs with negative Ref values
negflag <- function(dt) {
  negset <- filter(dt, Ref<0)  # other fields here?
  negids <- unique(negset[, Id])
  dt[, negflag := 0]
  dt[Id %in% negids, negflag := 1]
}

# aggregate by Id and generate features
collapsify2 <- function(dt) { 
  dt[, .(expected = mean(Expected, na.rm = T)
    , bigflag = mean(bigflag, na.rm = T)
#     , negflag = mean(negflag, na.rm = T)
    , records = .N    
    , rd = mean(radardist_km, na.rm = T)
    , refmissratio = sum(is.na(Ref))/.N
  ), Id]
}


### PROCESS THE DATA ####

# get train 
trraw <- fread("train.csv", select = selection)
bigflag(trraw) 
negflag(trraw)
timeparse(trraw)
trainbigs0 <- collapsify2(trraw)
trainbigs1 <- trainbigs0[refmissratio != 1]
write_csv(trainbigs1, "trainset.csv")

# downsample to create more balanced classes
bigsidx <- which(trainbigs1$bigflag == 1)
smallsidx <- which(trainbigs1$bigflag == 0)
dssmallsidx <- sample(1:length(smallsidx), length(bigsidx)*4)
trainbigs <- rbind(trainbigs1[bigsidx, ], trainbigs1[dssmallsidx, ])

# create a validation set                        
trainidx <- createDataPartition(trainbigs[, bigflag], p=.6, list=FALSE, times=1)
val <- trainbigs[-trainidx, ] 
setDF(val)
trainbigs <- trainbigs[trainidx, ] 
setDF(trainbigs)



### BUILD OUTLIER MODEL ###

# prep and set params
library(xgboost)
library(Metrics)
library(ROCR)
bestround=20

xgbtrainbigs <- xgb.DMatrix(as.matrix(trainbigs[, -c(1:3)]), label=trainbigs$bigflag, missing=NA)

param0 <- list(objective  = "binary:logistic" 
               , eval_metric = "auc"               
               , eta = 0.4
               , subsample = 1              
               , min_child_weight = 1
               , colsample_bytree = 1
               , max_depth = 6
)

# run cv model
numrounds <- 20
set.seed(333)
xgbfirstbigs <- xgb.cv(data=xgbtrainbigs
                       , params=param0
                       , nrounds=numrounds                                                         
                       , nfold=10
                       , verbose=1
                       , early.stop.round=NULL
) 
plot(xgbfirstbigs[, test.auc.mean], xlim=c(0,numrounds), ylim=c(0.85,1))
par(new=T)
plot(xgbfirstbigs[, train.auc.mean], xlim=c(0,numrounds), ylim=c(0.85,1), xlab='', ylab='',axes=F)
bestround <- which.max(xgbfirstbigs[,test.auc.mean]) 

# train the model
watched <- list(eval=xgbtrainbigs)
xgbmodbigs  <- xgb.train(data=xgbtrainbigs 
                         , params=param0
                         , nrounds = bestround
                         , verbose = 1
                         , watchlist = watched
)
xgbfactors <- xgb.importance(colnames(trainbigs[-c(1:3)]), model=xgbmodbigs)   
xgb.plot.importance(xgbfactors)

# validate and evaluate outlier model
xgbval <- xgb.DMatrix(as.matrix(val[, -c(1:3)]), missing=NA)
xgbpred2  <- predict(xgbmodbigs, xgbval)

predxgb <- prediction(xgbpred2, val$bigflag)
roccurve <- performance(predxgb, "tpr", "fpr")
predzeros <- prediction(rep(0,nrow(val)), val$bigflag)
baseline <- performance(predzeros, "tpr", "fpr")
plot(roccurve, col="blue")
plot(baseline, col="red", add=TRUE)
score <- performance(predxgb, "auc")
cat("AUC:", "\t", paste(score@y.values), "\n")

checkset <- data.frame(Id=val$Id, Expected=val$expected, PredBig=xgbpred2, BigFlag=val$bigflag)
write_csv(checkset, "checkset.csv") 


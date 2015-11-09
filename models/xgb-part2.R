### The objective of this script is to build a model or models, predict response variables, and check performance. ###

# set up and load
setwd("D:/Kaggles/Rain")
load("input.RData")
library(xgboost)
library(Metrics)
library(ROCR)
library(readr)

### BUILD MAIN PREDICTIVE MODEL ###

# prep
bestround <- 150L           # used if skipping the cv stage
xgbtrainmain <- xgb.DMatrix(as.matrix(trainmain[, -c(1:3)]), label=trainmain$target, missing=NA)

# set MAE eval function and model params 
meanabserr <- function(preds, xgbtrainmain) {
  labels <- getinfo(xgbtrainmain, "label")
  err <- mae(expm1(as.numeric(labels)), expm1(as.numeric(preds)))       
  return(list(metric = "MAE", value = err))
}  
param0 <- list(objective  = "reg:linear" 
  , eta = 0.4         
  , subsample = 1                               # model is not tuned
  , min_child_weight = 1
  , colsample_bytree = 0.7
  , max_depth = 5
)

# run model
numrounds <- 150
xgbfirstmain <- xgb.cv(data=xgbtrainmain
 , params=param0
 , feval = meanabserr                           # could try RMSE as eval metric?
 , nrounds=numrounds                                                         
 , nfold=5
 , verbose=1
 , early.stop.round=NULL
) 
plot(xgbfirstmain[, test.MAE.mean], xlim=c(0,numrounds), ylim=c(1.6,2.1))
par(new=T)
plot(xgbfirstmain[, train.MAE.mean], xlim=c(0,numrounds), ylim=c(1.6,2.1), xlab='', ylab='',axes=F)
bestround <- which.min(xgbfirstmain[,test.MAE.mean]) 

watched <- list(eval=xgbtrainmain)
set.seed(333)
xgbmodmain  <- xgb.train(data=xgbtrainmain 
  , params=param0
  , feval = meanabserr
  , nrounds = bestround
  , verbose = 1
  , watchlist = watched
)
xgbfactorsmain <- xgb.importance(colnames(trainmain[-c(1:3)]), model=xgbmodmain)   
xgb.plot.importance(xgbfactorsmain)

# validate and evaluate main model
xgbval <- xgb.DMatrix(as.matrix(val[, -c(1:3)]), missing=NA)
xgbpred  <- predict(xgbmodmain, xgbval)
zeroz <- vector(mode="numeric", length = nrow(val))    # correct for any negative values
predicted <- pmax(zeroz, expm1(xgbpred))
mae(expm1(val$target), predicted)


### BUILD OUTLIER MODEL ###

# prep and set params
bestround=150L 
xgbtrainbigs <- xgb.DMatrix(as.matrix(trainbigs[, -c(1:3)]), label=trainbigs$bigflag, missing=NA)

param0 <- list(objective  = "binary:logistic" 
               , eval_metric = "auc"               
               , eta = 0.3         
               , subsample = 1              
               , min_child_weight = 1
               , colsample_bytree = 0.7
               , max_depth = 6
)

# run model
numrounds <- 150
set.seed(333)
xgbfirstbigs <- xgb.cv(data=xgbtrainbigs
                   , params=param0
                   , nrounds=numrounds                                                         
                   , nfold=5
                   , verbose=1
                   , early.stop.round=NULL
) 
plot(xgbfirstbigs[, test.auc.mean], xlim=c(0,numrounds), ylim=c(0.65,1))
par(new=T)
plot(xgbfirstbigs[, train.auc.mean], xlim=c(0,numrounds), ylim=c(0.65,1), xlab='', ylab='',axes=F)
bestround <- which.max(xgbfirstbigs[,test.auc.mean]) 

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


### COMBINE THE MODELS AND APPLY TO HOLDOUT SET

xgbhold <- xgb.DMatrix(as.matrix(hold[, -c(1:3)]), missing = NA)
predmains  <- predict(xgbmodmain, xgbhold) 
predbigs  <- predict(xgbmodbigs, xgbhold) 

checkset <- data.table(Id=hold$Id, PredRain=expm1(predmains), PredBig=predbigs)
checkset[, Expected := PredRain]
checkset[PredBig>0.5, Expected := 2500]    # values set manually
setDF(checkset)

mae(expm1(hold$target), checkset$PredRain)
mae(expm1(hold$target), checkset$Expected)

write_csv(checkset, "checkset.csv") 

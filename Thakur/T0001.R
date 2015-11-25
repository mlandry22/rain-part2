library(data.table)
library(Metrics)
library(gbm)
library(xgboost)
library(readr)
library(h2o)
library(caret)

train <- fread("../Data/train1.csv")
test <- fread("../Data/test1.csv")

h2o.init(nthreads = -1,max_mem_size = '16G')
train1.hex <- as.h2o(train[,3:24,with=FALSE],destination_frame = "train.hex")
test1.hex <- as.h2o(test[,3:24,with=FALSE],destination_frame = "test.hex")

split <- createFolds(train$Expected,5)
error1 <- c()
for(i in 1:5){
  keep <- split[[i]]
  x1 <- train1.hex[-keep,]
  x2 <- train1.hex[keep,]
  model <- h2o.deeplearning(
  x=2:22,
  y=1,
  training_frame = x1,
  activation = "RectifierWithDropout",
  hidden = c(10,10),
  input_dropout_ratio = 0.1,
  hidden_dropout_ratio = c(0.1,0.1),
  loss = "Absolute",
  epochs = 10
  )
  pred <- as.data.frame(h2o.predict(model,x2))
  error <- sum(abs(train$Expected[keep]-(exp(pred$predict)+1)))/length(train$Expected[keep])
  error1 <- c(error1,error)
  print(error1)
  if(i==1) predT <- (exp(as.data.frame(h2o.predict(model,test1.hex))$predict) - 1) else predT <- predT + (exp(as.data.frame(h2o.predict(model,test1.hex))$predict) - 1)
  }
mean(error1)

T0001 <- data.frame(Id=test$Id,Expected=predT/5)




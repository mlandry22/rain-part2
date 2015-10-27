## This starts with the main process I keep reusing, but then shows several modeling attempts,
##  all jumbled in one script.
## I am working on a simple JSON interface to allow for easy hands-off experimentation, which
##   will start out just grid search with logging. When that happens, this will need to be broken
##   out into features and modeling.

setwd("/Users/mark/Documents/rain2")
library(data.table)
library(Metrics)
library(gbm)


t<-fread("train.csv",)
t[1:2,]
x<-t[,.(
  Expected=max(Expected),
  logExpected=log1p(max(Expected)),
  nTtl=.N,
  dist=max(radardist_km),
  nMissing=sum(is.na(Ref)),
  maxSet10=
    max(ifelse(minutes_past>=0 & minutes_past<10,1,0))+
    max(ifelse(minutes_past>=10 & minutes_past<20,1,0))+
    max(ifelse(minutes_past>=20 & minutes_past<30,1,0))+
    max(ifelse(minutes_past>=30 & minutes_past<40,1,0))+
    max(ifelse(minutes_past>=40 & minutes_past<50,1,0))+
    max(ifelse(minutes_past>=50 & minutes_past<60,1,0))
    ,
  maxValidSet10=
    max(ifelse(minutes_past>=0 & minutes_past<10 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=10 & minutes_past<20 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=20 & minutes_past<30 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=30 & minutes_past<40 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=40 & minutes_past<50 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=50 & minutes_past<60 & is.na(Ref)==F,1,0))
    ,
  maxSumRefSet10=
    max(ifelse(minutes_past>=0 & minutes_past<10 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=10 & minutes_past<20 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=20 & minutes_past<30 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=30 & minutes_past<40 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=40 & minutes_past<50 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=50 & minutes_past<60 & is.na(Ref)==F,Ref^2,0))
  ,
  meanRef=mean(Ref,na.rm=T),
  meanRef50=mean(Ref_5x5_50th,na.rm=T),
  meanSqRef10=mean(Ref_5x5_10th^2,na.rm=T),
  meanSqRef90=mean(Ref_5x5_90th^2,na.rm=T),
  meanRefComposite=mean(RefComposite,na.rm=T),
  meanRefComposite50=mean(RefComposite_5x5_50th,na.rm=T),
  meanRhoHV=mean(RhoHV,na.rm=T),
  meanRhoHV50=mean(RhoHV_5x5_50th,na.rm=T),
  meanZdr=mean(Zdr,na.rm=T),
  meanZdr50=mean(Zdr_5x5_50th,na.rm=T),
  meanKdp=mean(Kdp,na.rm=T),
  meanKdp50=mean(Kdp_5x5_50th,na.rm=T),
  
  meanSqRef=mean(Ref^2,na.rm=T),
  meanSqRef50=mean(Ref_5x5_50th^2,na.rm=T),
  meanSqRefComposite=mean(RefComposite^2,na.rm=T),
  meanSqRefComposite50=mean(RefComposite_5x5_50th^2,na.rm=T),
  meanSqRhoHV=mean(RhoHV^2,na.rm=T),
  meanSqRhoHV50=mean(RhoHV_5x5_50th^2,na.rm=T),
  meanSqZdr=mean(Zdr^2,na.rm=T),
  meanSqZdr50=mean(Zdr_5x5_50th^2,na.rm=T),
  meanSqKdp=mean(Kdp^2,na.rm=T),
  meanSqKdp50=mean(Kdp_5x5_50th^2,na.rm=T)
),Id]

#x<-as.data.table(x)
x[,missingRateTtl:=round(nMissing/nTtl,1)]
x[,validRateSet10:=round(maxValidSet10/maxSet10,1)]
x[,maxSumRefSet10:=maxSumRefSet10/(maxValidSet10+1)]

## Convert to data frame and turn NAN into NA
x<-as.data.frame(x)
for(i in 1:ncol(x)){x[is.nan(x[,i]),i]<-NA}

#############
### Test data
#############

tTest<-fread("test.csv")
tTest[,Expected:=0]  ## helps keep calculations identical for copy/paste or modular

xTest<-tTest[,.(
  Expected=max(Expected),
  logExpected=log1p(max(Expected)),
  nTtl=.N,
  dist=max(radardist_km),
  nMissing=sum(is.na(Ref)),
  maxSet10=
    max(ifelse(minutes_past>=0 & minutes_past<10,1,0))+
    max(ifelse(minutes_past>=10 & minutes_past<20,1,0))+
    max(ifelse(minutes_past>=20 & minutes_past<30,1,0))+
    max(ifelse(minutes_past>=30 & minutes_past<40,1,0))+
    max(ifelse(minutes_past>=40 & minutes_past<50,1,0))+
    max(ifelse(minutes_past>=50 & minutes_past<60,1,0))
  ,
  maxValidSet10=
    max(ifelse(minutes_past>=0 & minutes_past<10 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=10 & minutes_past<20 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=20 & minutes_past<30 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=30 & minutes_past<40 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=40 & minutes_past<50 & is.na(Ref)==F,1,0))+
    max(ifelse(minutes_past>=50 & minutes_past<60 & is.na(Ref)==F,1,0))
  ,
  maxSumRefSet10=
    max(ifelse(minutes_past>=0 & minutes_past<10 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=10 & minutes_past<20 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=20 & minutes_past<30 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=30 & minutes_past<40 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=40 & minutes_past<50 & is.na(Ref)==F,Ref^2,0))+
    max(ifelse(minutes_past>=50 & minutes_past<60 & is.na(Ref)==F,Ref^2,0))
  ,
  meanRef=mean(Ref,na.rm=T),
  meanRef50=mean(Ref_5x5_50th,na.rm=T),
  meanSqRef10=mean(Ref_5x5_10th^2,na.rm=T),
  meanSqRef90=mean(Ref_5x5_90th^2,na.rm=T),
  meanRefComposite=mean(RefComposite,na.rm=T),
  meanRefComposite50=mean(RefComposite_5x5_50th,na.rm=T),
  meanRhoHV=mean(RhoHV,na.rm=T),
  meanRhoHV50=mean(RhoHV_5x5_50th,na.rm=T),
  meanZdr=mean(Zdr,na.rm=T),
  meanZdr50=mean(Zdr_5x5_50th,na.rm=T),
  meanKdp=mean(Kdp,na.rm=T),
  meanKdp50=mean(Kdp_5x5_50th,na.rm=T),
  
  meanSqRef=mean(Ref^2,na.rm=T),
  meanSqRef50=mean(Ref_5x5_50th^2,na.rm=T),
  meanSqRefComposite=mean(RefComposite^2,na.rm=T),
  meanSqRefComposite50=mean(RefComposite_5x5_50th^2,na.rm=T),
  meanSqRhoHV=mean(RhoHV^2,na.rm=T),
  meanSqRhoHV50=mean(RhoHV_5x5_50th^2,na.rm=T),
  meanSqZdr=mean(Zdr^2,na.rm=T),
  meanSqZdr50=mean(Zdr_5x5_50th^2,na.rm=T),
  meanSqKdp=mean(Kdp^2,na.rm=T),
  meanSqKdp50=mean(Kdp_5x5_50th^2,na.rm=T)
),Id]

xTest[,missingRateTtl:=round(nMissing/nTtl,1)]
xTest[,validRateSet10:=round(maxValidSet10/maxSet10,1)]
xTest[,maxSumRefSet10:=maxSumRefSet10/(maxValidSet10+1)]

xTest<-as.data.frame(xTest)
for(i in 1:ncol(xTest)){xTest[is.nan(xTest[,i]),i]<-NA}

#####################
### Modeling
#####################

a<-1; b<-round(nrow(x2)*0.9); c<-b+1; d<-nrow(x2)
y<-x$Expected

## First leaderboard model
rg2<-gbm.fit(x2[a:b,3:24],x2$Expected[a:b],distribution = "laplace",n.trees = 100,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.05)
mae(y[c:d],predict(rg2,x2[c:d,3:24],100))  ## 137.8622
mae(y[c:d],median(y[a:b]))  ## 138.3613
summary(rg2)

## Day 2, in progress. Added set10's, missing counts, missing rates, Ref10 Ref90
rg3<-gbm.fit(x[a:b,4:ncol(x)],x$Expected[a:b],distribution = "laplace",n.trees = 100,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
summary(rg3)
mae(y[c:d],predict(rg3,x[c:d,4:ncol(x)],100))  ## 137.4258  ##137.6859  ## 137.8094  ## 137.8406    (median 138.3613)
mae(y[c:d],predict(rg3,x[c:d,4:ncol(x)],70))  ## 137.6859  ## 137.8094  ## 137.8406    (median 138.3613)
mae(y[c:d],predict(rg3,x[c:d,4:ncol(x)],80))  ## 137.6859  ## 137.8094  ## 137.8406    (median 138.3613)
mae(y[c:d],predict(rg3,x[c:d,4:ncol(x)],90))  ## 137.6859  ## 137.8094  ## 137.8406    (median 138.3613)

##############
## Submissions
##############

myS<-fread("sample_solution.csv")

#Refit 90/10 model to all data
rg2Full<-gbm.fit(x2[,3:24],x2$Expected,distribution = "laplace",n.trees = 100,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.05)
pR<-predict(rg2Full,xTest2[,2:23],100)
myS$Expected<-pR
write.csv(myS[,1:2,with=F],"submission2.csv",row.names=F)  ## got 127.10308

rg3Full<-gbm.fit(x[a:d,4:ncol(x)],x$Expected[a:d],distribution = "laplace",n.trees = 100,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
pR<-predict(rg3Full,xTest[,4:ncol(x)],100)
myS$Expected<-pR
write.csv(myS[,1:2,with=F],"submission4.csv",row.names=F)  ## error: put sum(maxSumRefSet10) instead of mean()

rg3Full<-gbm.fit(x[a:d,4:ncol(x)],x$Expected[a:d],distribution = "laplace",n.trees = 100,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
pR<-predict(rg3Full,xTest[,4:ncol(x)],100)
myS$Expected<-pR
write.csv(myS[,1:2,with=F],"submission5.csv",row.names=F)  ## got 126.64628

###########
####### H2O
###########

library(h2o)
h2o.init(nthreads = -1,max_mem_size = '8G')
xHex<-as.h2o(x,destination_frame = "x.hex")
xTestHex<-as.h2o(xTest,destination_frame = "xTest.hex")

pTest<-as.data.frame(h2o.getFrame("prediction-ac5fba79-cec7-4691-a054-cbfcdaf8736c"))
pTest[1:5,]

myS$Expected<-expm1(pTest[,1])
s[1:10,]; myS[1:10,]
myS$correction<-ifelse(s$Expected*10<myS$Expected,myS$Expected^ifelse(myS$Expected>1,0.5,2),myS$Expected)
summary(s$Expected)
#summary(myS$Expected)
summary(myS$correction)
summary(pR)
myS$Expected<-myS$correction

write.csv(myS[,1:2,with=F],"submission1.csv",row.names=F)  ## got 127.38101

## Try some transformations
## idea: transform, fit mse R model, untransform, test mae against 10% and compare; for fast iteraton, try 20 trees
maeModel<-

mae(y[c:d],predict(rg3,x[c:d,4:ncol(x)],10))  ## 138.1903
mae(y[c:d],predict(rg3,x[c:d,4:ncol(x)],20))  ## 138.1492    (median 138.3613)
mae(y[c:d],predict(rg3,x[c:d,4:ncol(x)],40))  ## 137.6507

x[1:2,]
## accidentally ran first with laplace, but against logged targets anyway; not useful
rMse<-gbm.fit(x[a:b,4:ncol(x)],x$logExpected[a:b],distribution = "laplace",n.trees = 40,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
summary(rMse)
mae(y[c:d],expm1(predict(rMse,x[c:d,4:ncol(x)],10)))  ## 138.2239   138.1903
mae(y[c:d],expm1(predict(rMse,x[c:d,4:ncol(x)],20)))  ## 138.1683   138.1492    (median 138.3613)
mae(y[c:d],expm1(predict(rMse,x[c:d,4:ncol(x)],40)))  ## 138.0754   137.6507

## actual MSE on logTarget
rMse<-gbm.fit(x[a:b,4:ncol(x)],x$logExpected[a:b],distribution = "gaussian",n.trees = 40,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
summary(rMse)
mae(y[c:d],expm1(predict(rMse,x[c:d,4:ncol(x)],10)))  ## 138.6125
mae(y[c:d],expm1(predict(rMse,x[c:d,4:ncol(x)],20)))  ## 138.4073    (median 138.3613)
mae(y[c:d],expm1(predict(rMse,x[c:d,4:ncol(x)],40)))  ## 138.1675

## fourth root; quite a bad idea
rMse<-gbm.fit(x[a:b,4:ncol(x)],(x$Expected[a:b]+1)^0.25,distribution = "gaussian",n.trees = 40,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
summary(rMse)
mae(y[c:d],((predict(rMse,x[c:d,4:ncol(x)],10)^4)-1))  ## 139.8754
mae(y[c:d],((predict(rMse,x[c:d,4:ncol(x)],20)^4)-1))  ## 139.4294    (median 138.3613)
mae(y[c:d],((predict(rMse,x[c:d,4:ncol(x)],40)^4)-1))  ## 139.1144

## MSE on logTarget with caps
rMse<-gbm.fit(x[a:b,4:ncol(x)],pmin(4,x$logExpected[a:b]),distribution = "gaussian",n.trees = 40,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
summary(rMse)
mae(y[c:d],expm1(predict(rMse,x[c:d,4:ncol(x)],10)))  ## 138.6125
mae(y[c:d],expm1(predict(rMse,x[c:d,4:ncol(x)],20)))  ## 138.4073    (median 138.3613)
mae(y[c:d],expm1(predict(rMse,x[c:d,4:ncol(x)],40)))  ## 138.1675

## alpha 0.45
rMse<-gbm.fit(x[a:b,4:ncol(x)],x$Expected[a:b],distribution = list(name="quantile",alpha=0.45),n.trees = 40,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],10))  ## 138.2322
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],20))  ## 137.8904    (median 138.3613)
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],40))  ## 137.704

## alpha 0.55: does better on this holdout. Could experiment with alphas against test set.
rMse<-gbm.fit(x[a:b,4:ncol(x)],x$Expected[a:b],distribution = list(name="quantile",alpha=0.55),n.trees = 40,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],10))  ## 138.24
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],20))  ## 138.0795    (median 138.3613)
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],40))  ## 136.9727

## alpha 0.55 with 50mm cap
rMse<-gbm.fit(x[a:b,4:ncol(x)],pmin(50,x$Expected[a:b]),distribution = list(name="quantile",alpha=0.55),n.trees = 40,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],10))  ## 138.2173
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],20))  ## 138.133    (median 138.3613)
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],40))  ## 137.9281

## alpha 0.5 with 100mm cap
rMse<-gbm.fit(x[a:b,4:ncol(x)],pmin(100,x$Expected[a:b]),distribution = list(name="quantile",alpha=0.5),n.trees = 40,interaction.depth = 6,n.minobsinnode = 20,shrinkage = 0.1)
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],10))  ## 138.2133
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],20))  ## 138.1254    (median 138.3613)
mae(y[c:d],predict(rMse,x[c:d,4:ncol(x)],40))  ## 137.9923

summary(expm1(predict(rMse,x[c:d,4:ncol(x)],40)))
summary(predict(rMse,x[c:d,4:ncol(x)],40))
summary(predict(rg3,x[c:d,4:ncol(x)],40))

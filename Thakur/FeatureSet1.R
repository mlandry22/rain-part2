library(data.table)
library(Metrics)
library(gbm)
library(xgboost)
library(readr)

t<-fread("../Data/train.csv",)
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
for(i in 1:ncol(x)){x[is.nan(x[,i]),i] <- NA}

write.csv(x,file="../Data/train1.csv",row.names=FALSE)
#############
### Test data
#############

tTest<-fread("../Data/test.csv")
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

write.csv(xTest,file="../Data/test1.csv",row.names=FALSE)
### The objective of this script is to bring in data,   ###
### generate features, and format inputs for models.     ###

# set up
setwd("D:/Kaggles/Rain")
library(data.table)
library(readr)
library(dplyr)
set.seed(333)

### SET VARIABLES AND FUNCTIONS ###

# set columns to import
selection <- c(
  "Id", 
  "Expected",
  "minutes_past",             
  "radardist_km",                                                       
  "Ref", 
  "Ref_5x5_10th",
  "Ref_5x5_50th",
  "Ref_5x5_90th",
  "RefComposite", 
  "RefComposite_5x5_10th",
  "RefComposite_5x5_50th",
  "RefComposite_5x5_90th",
  "RhoHV",
#   "RhoHV_5x5_10th",
#   "RhoHV_5x5_50th",
#   "RhoHV_5x5_90th",
#   "Zdr",
#   "Zdr_5x5_10th",
  "Zdr_5x5_50th",
# "Zdr_5x5_90th",
#   "Kdp",
#   "Kdp_5x5_10th",
  "Kdp_5x5_50th"
#   "Kdp_5x5_90th"  
)

# add a flag to identify the target group
bigflag <- function(dt) {
  bigset <- filter(dt, Expected>2390 & Expected<4020)
  bigids <- unique(bigset[, Id])
  dt[, bigflag := 0]
  dt[Id %in% bigids, bigflag := 1]
}

# create time spans between measurements for each Id
timeparse <- function(dt){
  minutes <- dt[, minutes_past]
  n <- length(minutes)
  timepassed <- vector(mode="numeric", length = n) 
  timepassed[1] <- minutes[1]
  timepassed[-1] <- diff(minutes)
  timepassed[n] <- 60-minutes[n-1]
  for (i in seq(2, n-1)) {                          # still some corner cases that may miscalculate
    if (minutes[i] < minutes[i-1]) {                # 34 IDs with single record plus other unique cases
      timepassed[i] <- minutes[i]  
    }
    if (minutes[i] > minutes[i+1]) {
      timepassed[i] <- 60-minutes[i-1]  
    }
  }
  dt[, timespans := timepassed/60] 
}

# replace negative values
posrefs <- function(dt) {
  dt[Ref<0, Ref := NA]
  dt[Ref_5x5_50th<0, Ref_5x5_50th := NA]
  dt[Ref_5x5_90th<0, Ref_5x5_90th := NA]
  dt[RefComposite<0, RefComposite := NA]
  dt[RefComposite_5x5_50th<0, RefComposite_5x5_50th := NA]
  dt[RefComposite_5x5_90th<0, RefComposite_5x5_90th := NA]  
}

# replace some missing values for Ref               
replacenas <- function(dt)  { 
  norefidx <- which(is.na(dt[, Ref]))
  norefcidx <- which(is.na(dt[, RefComposite]))
  noref5idx <- which(is.na(dt[, Ref_5x5_50th]))
  
  subcidx <- setdiff(norefidx, norefcidx) 
  sub5idx <- setdiff(norefidx, noref5idx) 
  sub5idx <- setdiff(sub5idx, subcidx) 
  
  refc <- as.numeric(dt[, RefComposite])
  refc <- refc * 0.9201 + .1694
  dt[subcidx, "Ref"] <- refc[subcidx]
  
  ref5 <- as.numeric(dt[, Ref_5x5_50th])
  ref5 <- ref5 * 0.9605 + 1.3533
  dt[sub5idx, "Ref"] <- ref5[sub5idx]
}

# estimate rainfall rates as a function of reflectivity
marshall_palmer <- function(dbz) ((10**(dbz/10))/200) ** (1/1.6)    # standard marshall-palmer function to find rainfall rate;
wsr_88d         <- function(dbz) ((10**(dbz/10))/300) ** (1/1.4)    # modified z-R function according to newer NWS standard;
nws_tropical    <- function(dbz) ((10**(dbz/10))/250) ** (1/1.2)    # profile for heavy rains and tropical storms
dorain <- function(dt) {                                             
  dt[, rate := wsr_88d(Ref)] 
  dt[, rateC := wsr_88d(RefComposite)]                              
}

# aggregate by Id and generate more features
collapsify <- function(dt) {
  dt[, .(
    target = log1p(mean(Expected, na.rm = T)),
    bigflag = mean(bigflag, na.rm = T),
    wref = mean(timespans * Ref, na.rm = T),         # look at slicing the timespans by midpoint instead of endpoint
    ref1 = mean(Ref_5x5_10th, na.rm = T),
    ref1sq = mean(Ref_5x5_10th^2, na.rm = T),        # any advantage to splitting the model in half after collapsing?  
    ref5 = mean(Ref_5x5_50th, na.rm = T),
    ref5sq = mean(Ref_5x5_50th^2, na.rm = T),
    ref9sq = mean(Ref_5x5_90th^2, na.rm = T),
    wrefc = mean(timespans * RefComposite, na.rm = T),
    refc1sq = mean(RefComposite_5x5_10th^2, na.rm = T),            
    refc9sq = mean(RefComposite_5x5_90th^2, na.rm = T),
    zdr5 = mean(Zdr_5x5_50th, na.rm = T),
    ratemax = max(rate, na.rm = T),         # max values cause warnings but will be purged with !is.na below
    refsd = sd(Ref, na.rm = T),
    ratesd = sd(rate, na.rm = T), 
    refcsd = sd(RefComposite, na.rm = T), 
    precip = sum(timespans * rate, na.rm = T),   
    precipC = sum(timespans * rateC, na.rm = T),
    refdiff = mean(Ref_5x5_50th-Ref, na.rm = T),
    refratio2 = mean((Ref_5x5_90th-Ref_5x5_10th)/Ref, na.rm = T), 
    refcratio2 = mean((RefComposite_5x5_90th-RefComposite_5x5_10th)/RefComposite, na.rm = T),
    rd = mean(radardist_km, na.rm = T),    
    rdxref = mean(radardist_km * Ref, na.rm = T),
    refdivrd = mean(Ref / radardist_km, na.rm = T),
    rdxrefc = mean(radardist_km * RefComposite, na.rm = T),
    refcdivrd = mean(RefComposite / radardist_km, na.rm = T),
    records = .N,
    refmissratio = sum(is.na(Ref))/.N
  ), Id]
}

### PROCESS DATA ####

# get train 
trraw <- fread("train.csv", select = selection)

# create a holdout set
idnumsh <- unique(trraw[, Id])
holdidx <-sample(1:length(idnumsh), length(idnumsh)/5)                     
holdraw <- trraw[Id %in% holdidx, ] 
trraw <- trraw[!Id %in% holdidx, ]  

# start processing train
bigflag(trraw) 
timeparse(trraw)
posrefs(trraw)
replacenas(trraw)
dorain(trraw)

# create an interim validation set                          
idnumsv <- unique(trraw[, Id])
validx <-sample(1:length(idnumsv), length(idnumsv)/5)                     
valraw <- trraw[Id %in% validx, ] 
trraw <- trraw[!Id %in% validx, ] 
val <- collapsify(valraw)
val <- val[!is.na(val[, wref]), ]      
setDF(val)

# finish processing for train to be used in outlier prediction
trainbigs <- collapsify(trraw) 
trainbigs <- trainbigs[!is.na(trainbigs[, wref]), ]
setDF(trainbigs)

# create filtered set for the main predictive model 
trrawmain <- subset(trraw, Expected <= 50)  
legit_reads <- 0.254 * 1:500                                      
trrawmain <- trrawmain[round(Expected, 4) %in% legit_reads]     
trainmain <- collapsify(trrawmain) 
trainmain <- trainmain[!is.na(trainmain[, wref]), ]
setDF(trainmain)  

# process the holdout set
timeparse(holdraw)
posrefs(holdraw)
replacenas(holdraw)
dorain(holdraw)
hold <- collapsify(holdraw) 
hold <- hold[!is.na(hold[, wref]), ]       # these types are not scored for the test set, so pulling them here also
setDF(hold)

# get and process the test set
teraw <- fread("test.csv", select = selection)
timeparse(teraw)
posrefs(teraw)
replacenas(teraw)
dorain(teraw)
teraw[, c("Expected", "bigflag") := 0]
test <- collapsify(teraw)
test[, c("target", "bigflag") := NULL]
setDF(test)

# clean up and save
keepers <- c("hold", "trainbigs", "trainmain", "val", "test")
rm(list= ls()[!(ls() %in% keepers)])
save.image("input.RData")

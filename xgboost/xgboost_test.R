rm(list=ls())
library(xgboost)


setwd('/Users/lululu/Documents/WYY/Graduate2019Spring/STAT602/DMC')
train.dat = read.csv('DMC_2019_task/train.csv',header=TRUE,sep='|')
test.dat = read.csv('DMC_2019_task/test.csv',header=TRUE,sep='|')
names(train.dat)
#train.dat$trustLevel <- as.factor(train.dat$trustLevel)
#train.dat$fraud <- as.factor(train.dat$fraud)
#test.dat$trustLevel <- as.factor(test.dat$trustLevel)

train.dat = cbind(train.dat, total.item=train.dat$totalScanTimeInSeconds*train.dat$scannedLineItemsPerSecond)
train.xg = list(data = as.matrix(train.dat[-10]),label = train.dat$fraud)
bst1 = xgboost(data = train.xg$data,label = train.xg$label,
             max_depth = 5,nrounds = 3,objective = 'binary:logistic')
test.fit = predict(bst1, newdata=as.matrix(test.dat))
xgb.dump(bst1)


eval_loss <- function(preds,truth){
  # false positive
  nfp <- sum(preds == 1 & truth == 0)
  # false negative
  nfn <- sum(preds == 0 & truth == 1)
  nbonus <- sum(preds == 1 & truth == 1)
  loss <- 25*nfp + 5*nfn - 5*nbonus
  return(list(metric = "error", value = loss))
}

CV.xgb <- function(xg.dat,nfold,thres){
  Nobs = length(xg.dat$label)
  fsize = floor(Nobs/nfold)
  ind.all = sample(1:Nobs,size = Nobs,replace = FALSE)
  Prob.all = numeric(length = Nobs)
  for (i in 1:nfold){
    if (i < nfold){
      ind.test = ind.all[(1:fsize)+(i-1)*fsize]
    }else if (i  == nfold){
      ind.test = ind.all[((i-1)*fsize+1):Nobs]
    }
    train.foldi = list(data  =  as.matrix(xg.dat$dat[-ind.test,]),
                     label  =  xg.dat$label[-ind.test])
    test.foldi = list(data = as.matrix(xg.dat$dat[ind.test,]),
                    label = xg.dat$label[ind.test])
    bst.foldi = xgboost(data = train.foldi$data,label = train.foldi$label,
                      max_depth = 5,nrounds = 20,objective = 'binary:logistic')
    pred.foldi = predict(bst.foldi,test.foldi$data)
    Prob.all[ind.test] = pred.foldi
  }
  Pred.all = as.numeric(Prob.all>thres)
  loss_01 = sum(Pred.all != xg.dat$label)   # 0-1 loss
  loss_fraud = eval_loss(Pred.all,xg.dat$label)
  return(list(Prob.all=Prob.all,Pred.all=Pred.all,loss_01=loss_01,loss_fraud=loss_fraud))
}


thres = 5/7
CV.fit= CV.xgb(train.xg,10,thres)
CV.fit$loss_fraud
CV.fit$loss_01

rep.cv = replicate(100,CV.xgb(xg.dat=train.xg,nfold=10,thres=5/7))
mean(unlist(lapply(rep.cv[4,],'[[',2)))






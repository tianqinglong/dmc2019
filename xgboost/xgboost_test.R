rm(list=ls())
library(xgboost)


#setwd('/Users/lululu/Documents/WYY/Graduate2019Spring/STAT602/DMC')
train.dat = read.csv('DMC_2019_task/train.csv',header=TRUE,sep='|')
test.dat = read.csv('DMC_2019_task/test.csv',header=TRUE,sep='|')
names(train.dat)
#train.dat$trustLevel <- as.factor(train.dat$trustLevel)
#train.dat$fraud <- as.factor(train.dat$fraud)
#test.dat$trustLevel <- as.factor(test.dat$trustLevel)

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

CV.xgb <- function(xg.dat,nfold){
  Nobs = length(xg.dat$label)
  fsize = floor(Nobs/nfold)
  ind.all = sample(1:Nobs,size = Nobs,replace = FALSE)
  Pred.all = numeric(length = Nobs)
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
                      max_depth = 5,nrounds = 3,objective = 'binary:logistic')
    pred.foldi = predict(bst.foldi,test.foldi$data)
    Pred.all[ind.test] = pred.foldi
  }
  return(Pred.all)
}

CV.prob = CV.xgb(train.xg,5)
thres = 5/7
CV.pred = as.numeric(CV.prob>thres)
sum(CV.pred != train.xg$label)   # 0-1 loss
eval_loss(CV.pred,train.xg$label)



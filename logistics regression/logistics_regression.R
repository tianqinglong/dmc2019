require(MuMIn)
require(ggplot2)
require(caret)
dat<-read.csv('/Users/lijinzhang/Documents/isu/602/DMC_2019_task/train.csv',header=T,sep='|')
n.dat <- nrow(dat)

final.cost.100 <- NULL
final.cost.avg <- matrix(rep(0,3*(2^9)),ncol=2^9)
for (i in 1:100){
  ind.train <- unlist(createDataPartition(dat$fraud,times=1, p=0.8))
  dat.train <- dat[ind.train,]
  dat.test <- dat[-ind.train,] 

  
  g <- glm(fraud ~ trustLevel+totalScanTimeInSeconds +grandTotal+lineItemVoids +
             scansWithoutRegistration+quantityModifications+scannedLineItemsPerSecond
           +valuePerSecond+lineItemVoidsPerPosition,
           data=dat.train, family=binomial(),na.action = na.fail)
  models <- lapply(dredge(g, evaluate = FALSE),eval)
  pred.fits <- predict(models,dat.test,type='response')
  
  
  threshold <- c(0.7,0.75,0.8)
  
  final.cost.allcombo <- NULL
  for (i in 1:(2^9)){
    final.cost<-NULL
   for (t in threshold){
      pred <- ifelse(unlist(pred.fits[i])>t,1,0)
      res <- data.frame(cbind(dat.test$fraud,pred))
      colnames(res) <- c('true','pred')
    
      cost <- sum(res$true==1&res$pred==0)*-5+
        sum(res$true==1&res$pred==1)*5+
        sum(res$true==0&res$pred==1)*-25
      final.cost <- c(final.cost,cost)
    
   }
    names(final.cost)<-threshold
    final.cost.allcombo <- cbind(final.cost.allcombo,final.cost)
    
}
  final.cost.avg <- final.cost.avg + final.cost.allcombo/100
  final.cost.100 <- rbind(final.cost.100, final.cost.allcombo)
  colnames(final.cost.100)<-c(1:(2^9))
  colnames(final.cost.avg) <- c(1:(2^9))
  
}

a <- stack(as.data.frame(final.cost.avg))
a$cutoff <- threshold
qplot(cutoff, values, data = a, group = ind, colour = ind, geom = "line")

matplot(x=threshold,y = final.cost.avg, type = 'l', lty = 1)

#ap <- stack(as.data.frame(final.cost.avg[,]))
#ap$cutoff <- threshold[10:21]
#qplot(cutoff, values, data = ap, group = ind, colour = ind, geom = "line")

save(final.cost.100.all,
     file="/Users/lijinzhang/Documents/isu/602/DMC_2019_task/logistics regression/final_cost_100_all.Rdata")

save(final.cost.avg.all,
     file="/Users/lijinzhang/Documents/isu/602/DMC_2019_task/logistics regression/final_cost_avg_all.Rdata")





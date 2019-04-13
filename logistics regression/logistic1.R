library(ggplot2)

Train0 <- read.csv(file = "/Users/apple/Desktop/ISU 2019 spring/DMC2019/DMC_2019_task/train.csv",
                  sep = "|")

names(Train0)

Train0$trustLevel <- as.factor(Train0$trustLevel)
TotalItem <- Train0$totalScanTimeInSeconds * Train0$scannedLineItemsPerSecond
AveValue <- Train0$grandTotal / TotalItem



Train1 <- data.frame(Train0, TotalItem = TotalItem)






################################## logistic regression ####################################
lossDMC <- function(true, pred){
  loss <- sum( (true==1) & (pred==0) ) * 5 +
    sum( (true==0) & (pred==1) ) * 25 + 
    sum( (true==1) & (pred==1) ) * (-5)
  return(loss)
}





cv_design <- function(n, fold = 10){
  m <- floor(n/fold)
  r <- n%%fold
  p1 <- rep(m, fold)
  p2 <- rep(0, fold)
  if (r>=1){
    p2[1:r] <- 1
  }
  p <- p1 + p2
  ub <- cumsum(p)
  lb <- ub - p + 1
  x <- sample(n)
  IND <- vector("list",fold)
  for (i in 1:fold){
    IND[[i]] <- x[(lb[i]):(ub[i])]
  }
  return(IND)
}




cv_logistic_probs <- function(D,formula, fold = 10){
  n <- length(D[,1])
  IND <- cv_design(n, fold)
  probs <- rep(0,n)
  options(warn=-1) 
  for (i in 1:fold){
    test_ind <- IND[[i]]
    Train <- D[-test_ind,] 
    Test <- D[test_ind,] 
    fit <- glm(formula=formula,data=Train,family=binomial(link=logit))
    py <- predict(fit,newdata=data.frame(Test), type = "response")
    probs[test_ind] <- py
  }
  return(probs)
}








################################## Loss results #################################

######## all 0 ##########
pred0 <- rep(0, length(Train1[,1]))
lossDMC(true = Train1$fraud, 
        pred = pred0 )
## result: 520
sum(pred0!=Train1$fraud)
## 104
#########################



######################################  Train1 ###################################
formula1 <- "fraud~."
loss1 <- rep(0, 100)
loss2 <- rep(0, 100)
for (i in 1:100){
  pred1 <- pred0 
  probs <- cv_logistic_probs(Train1, formula1, 10)
  pred1[ which(  probs > 5/7  ) ] <- 1
  loss1[i] <- lossDMC(true = Train1$fraud, 
                          pred = pred1)
  loss2[i] <- sum(pred1!=Train1$fraud)
}

mean(loss1)
## -251.85
mean(loss2)
## 16.42
###############################################################################################






######################################  Train0 ###################################
formula1 <- "fraud~."
loss1 <- rep(0, 100)
loss2 <- rep(0, 100)
for (i in 1:100){
  pred1 <- pred0 
  probs <- cv_logistic_probs(Train0, formula1, 10)
  pred1[ which(  probs > 5/7  ) ] <- 1
  loss1[i] <- lossDMC(true = Train0$fraud, 
                      pred = pred1)
  loss2[i] <- sum(pred1!=Train0$fraud)
}

mean(loss1)
## -87.8
mean(loss2)
## 32.9
###############################################################################################







fit_final <- glm(fraud~., data=Train1, family=binomial(link=logit))
summary(fit_final)
pred_final <- pred0
pred_final[which(fit_final$fitted.values>5/7)] <- 1

## train loss
lossDMC(true = Train1$fraud, 
        pred = pred_final)
sum(pred_final!=Train1$fraud)


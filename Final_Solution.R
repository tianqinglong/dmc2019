### import
train <- read.table("C:\\Users\\howard\\Desktop\\ISU courses\\STAT 602 Statistical Learning\\DMC\\DMC_2019_task\\train.csv"
                    ,header = T,sep='|')
test <- read.table("C:\\Users\\howard\\Desktop\\ISU courses\\STAT 602 Statistical Learning\\DMC\\DMC_2019_task\\test.csv"
                   ,header = T,sep='|')

### feature engineering
train$TotalItem <- train$totalScanTimeInSeconds * train$scannedLineItemsPerSecond
test$TotalItem <- test$totalScanTimeInSeconds * test$scannedLineItemsPerSecond
test$trustLevel <- as.factor(test$trustLevel)

d <- train
d$trustLevel <- as.factor(d$trustLevel)
#### formula
formula1 <- "fraud~trustLevel + TotalItem + lineItemVoids + scansWithoutRegistration + 
totalScanTimeInSeconds"

formula2 <- "fraud~trustLevel + TotalItem + lineItemVoids + scansWithoutRegistration + 
totalScanTimeInSeconds + I( totalScanTimeInSeconds * valuePerSecond ) +I( totalScanTimeInSeconds * valuePerSecond^(2) )"

formula3 <-  'fraud~trustLevel + TotalItem + lineItemVoids + scansWithoutRegistration + 
totalScanTimeInSeconds + I( totalScanTimeInSeconds * valuePerSecond ) +I( totalScanTimeInSeconds * valuePerSecond^(3.5) )'


####### predict testing
M1 <- glm(formula = formula1 , data=d, family=binomial(link=logit))
M2 <- glm(formula = formula2 , data=d, family=binomial(link=logit))
M3 <- glm(formula = formula3 , data=d, family=binomial(link=logit))
P1 <- predict(M1, test, type ="response")
P2 <- predict(M2, test, type ="response")
P3 <- predict(M3, test, type ="response")
P_M1 <- 0.4*P1 + 0.6*P2
P_M2 <- 0.4*P1 + 0.6*P3
P_M3 <- 0.3*P1 + 0.1*P2 + 0.6*P3

Pred1 <- ifelse(P_M1>5/7,1,0)
Pred2 <- ifelse(P_M2>5/7,1,0)
Pred3 <- ifelse(P_M3>5/7,1,0)
Pred_df <- data.frame(Pred1,Pred2,Pred3)
# Majority Vote
Pred_df$vote <- apply(Pred_df, 1, function(p){
  re <- ifelse(sum(p) > 1,1,0)
})

submit <- as.data.frame(Pred_df$vote)
colnames(submit) <- 'fraud'
write.csv(submit, file='Uni_State_Iowa_2.csv')
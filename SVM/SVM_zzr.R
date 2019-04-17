Train <- read.csv(file = "~/Documents/2019Spring/STAT602/DMC_2019_task/train.csv",
                  sep = "|")
Train$trustLevel <- as.factor(Train$trustLevel)
Train$fraud <- as.factor(Train$fraud)

set.seed(1234)
idx <- sample(1:1879, 1503, replace = FALSE)
training <- Train[idx,]
testing <- Train[-idx,]
library(caret)

## Default Benchmark
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_Linear <- train(fraud ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
test_pred <- predict(svm_Linear, newdata = testing)

confusionMatrix(test_pred, testing$fraud)

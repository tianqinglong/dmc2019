library(caret)
library(e1071)
library(tidyverse)

library(doParallel)
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

train <- read.csv("train.csv", sep = "|") %>%
  mutate(
    fraud = as.factor(fraud),
    totalItems = totalScanTimeInSeconds * scannedLineItemsPerSecond
    )

tag <- 'fraud'
features <- setdiff(names(train), tag)

trControl <- trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 100
)

search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
  )

nb.fit <- train(train[, features],
                train[, tag],
                'nb',
                trControl = trControl,
                tuneGrid = search_grid,
                preProc = c("BoxCox", 
                            "center", 
                            "scale")
                )

confusionMatrix(nb.fit)

save(nb.fit, file = "nbfit.RData")
stopCluster(cl)

load("nbfit.RData")

fit <- klaR::NaiveBayes(train[, features],
            train[, tag],
            usekernel = TRUE,
            fL = 0,
            adjust = 3)

tenFold <- createFolds(train$fraud, 10)

doNaiveBayes <- function(x, dat)
{
  test <- dat[x, ]
  train <- dat[-x, ]
  
  model <- NaiveBayes(train[, features],
                      train[, tag],
                      usekernel = TRUE,
                      fL = 0,
                      adjust = 3)
  
  pred <- predict(model, test)
  tb <- table(pred$posterior[,2] > 5/7, test[,10])
  
  return(-25*tb[2, 1]-
           5*tb[1, 2]+
           5*tb[2, 2])
}

loss <- replicate( 100, sum( sapply(createFolds(train$fraud, 10), doNaiveBayes, dat = train) ), simplify = T)

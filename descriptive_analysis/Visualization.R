library(ggplot2)
library(GGally)

train <- read.csv("train.csv", header = TRUE, sep = "|")

ggpairs(train)

q=c(0.0001, 0.005, 0.01, 0.05, 0.1, 0.9, 0.95, 0.99, 0.995, 0.999)
quantile(train$scannedLineItemsPerSecond, q)
quantile(train$valuePerSecond, q)

trunc.train=train[which(train$scannedLineItemsPerSecond < 1.01 & train$valuePerSecond < 3 ),]

ggpairs(trunc.train)


trans.train=train[which(train$lineItemVoidsPerPosition > 0),]
trans.train$scannedLineItemsPerSecond=log(trans.train$scannedLineItemsPerSecond)
trans.train$valuePerSecond=log(trans.train$valuePerSecond)
trans.train$lineItemVoidsPerPosition=log(trans.train$lineItemVoidsPerPosition)

ggpairs(trans.train)
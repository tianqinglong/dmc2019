library(tidyverse)
library(caret)
library(h2o)

train <- read.csv("train.csv", sep = "|")
train %>% mutate(
  fraud = as.factor(fraud),
  totalItems = totalScanTimeInSeconds * scannedLineItemsPerSecond
) -> train

h2o.no_progress()
h2o.init()

train.h2o <- train %>% as.h2o()
y <- "fraud"
x <- setdiff(names(train), y)

nb.h2o <- h2o.naiveBayes(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 10,
  laplace = 0
)
h2o.confusionMatrix(nb.h2o)

preprocess <- preProcess(train, method = c("BoxCox",
                                           "center",
                                           "scale"))
train_pp <- predict(preprocess, train)

train_pp.h2o <- 
  train_pp %>% 
  as.h2o()

y <- "fraud"
x <- setdiff(names(train), y)

hyper_params <- list(
  laplace = seq(0, 5, by = 0.5)
)

grid <- h2o.grid(
  algorithm = "naivebayes",
  grid_id = "nb_grid",
  x = x, 
  y = y, 
  training_frame = train_pp.h2o, 
  nfolds = 10,
  hyper_params = hyper_params
)

sorted_grid <- h2o.getGrid("nb_grid", sort_by = "accuracy", decreasing = TRUE)
sorted_grid

best_h2o_model <- sorted_grid@model_ids[[1]]
best_model <- h2o.getModel(best_h2o_model)
h2o.confusionMatrix(best_model)
# auc <- h2o.auc(best_model, xval = TRUE)
# fpr <- h2o.performance(best_model, xval = TRUE) %>% h2o.fpr() %>% .[['fpr']]
# tpr <- h2o.performance(best_model, xval = TRUE) %>% h2o.tpr() %>% .[['tpr']]
# data.frame(fpr = fpr, tpr = tpr) %>%
#   ggplot(aes(fpr, tpr) ) +
#   geom_line() + 
#   ggtitle( sprintf('AUC: %f', auc) )
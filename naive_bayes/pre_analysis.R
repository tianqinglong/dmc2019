library(tidyverse)
library(scales)

train <- read.csv("train.csv", sep = "|")

train %>% mutate(
  fraud = as.factor(fraud),
  totalItems = totalScanTimeInSeconds * scannedLineItemsPerSecond
) -> train

train %>%
  filter(fraud == 0) %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot(tl.cex = 0.6, tl.srt = 45, title = "Non Fraud", mar = c(0, 0, 0.65, 0))

train %>%
  filter(fraud == 1) %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot(tl.cex = 0.6, tl.srt = 45, title = "Fraud", mar = c(0, 0, 0.65, 0))

train %>%
  ggplot( aes(x = scannedLineItemsPerSecond, y = totalScanTimeInSeconds, col = fraud))+
  geom_point(alpha = 0.4)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

train %>% group_by(fraud) %>%
  ggplot( aes(x = totalItems, fill = fraud) ) + geom_density(alpha = 0.4)

train %>%
  ggplot( aes(x = lineItemVoidsPerPosition, y = lineItemVoids, col = fraud))+
  geom_point(alpha = 0.4, position = "jitter")+
  xlim(0, 1)

train %>%
  ggplot( aes(x = valuePerSecond, y = scannedLineItemsPerSecond, col = fraud))+
  geom_point(alpha = 0.4)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

train %>%
  ggplot( aes(x = exp(-2*valuePerSecond)^2, y = sqrt(grandTotal), col = fraud))+
  geom_point(alpha = 0.5)
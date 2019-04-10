library(tidyverse)
library(reshape2)

train <- read.csv("train.csv", header = TRUE, sep = "|")

draw_discrete <- function(var){
  train %>% 
    mutate( fraud = as.factor(fraud) ) %>% 
    group_by( fraud ) %>% 
    ggplot( aes(x = get(var), fill = fraud) )+
    geom_bar(alpha = 0.25)+
    labs( title = paste(var ,"by fraud"), x = var)
}

name_vector <- c("trustLevel",
                 "lineItemVoids",
                 "scansWithoutRegistration",
                 "quantityModifications")

pdf("supplementary_descriptive_analysis.pdf")

gg <- list()
for(i in 1:length(name_vector)){
    gg[[i]] <- draw_discrete(name_vector[i])
}

gridExtra::grid.arrange(gg[[1]],gg[[2]],gg[[3]],gg[[4]],
                        nrow = 2)
dev.off()
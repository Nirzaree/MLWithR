library(caret)

fMultipleClassificationModels <- function(Formula, TrainingData, TrainingControl, EvalMetric, TrainingModels) {
  AllModels <- lapply(TrainingModels, function(x) {
    set.seed(3)
    TrainedModel <- train(Formula,
                          TrainingData,
                          method = x,
                          metric = EvalMetric,
                          trControl = TrainingControl
                          # family = "binomial" #only for glm method
    )
    
    return(TrainedModel)
  })
  
  return(AllModels)
}

# fMultipleClassificationModels <- function(TrainingData, TrainingLabels, TrainingControl, EvalMetric, TrainingModels) {
#   AllModels <- lapply(TrainingModels, function(x) {
#     set.seed(3)
#     TrainedModel <- train(TrainingData,
#                           TrainingLabels,
#                           method = x,
#                           metric = EvalMetric,
#                           trControl = TrainingControl
#                           # family = "binomial" #only for glm method
#     )
#     
#     return(TrainedModel)
#   })
#   
#   return(AllModels)
# }


fMultipleClassificationModels <- function(TrainingData, TrainingLabels, TrainingControl, EvalMetric, TrainingModels) {
  AllModels <- lapply(TrainingModels, function(x) {
    set.seed(3)
    TrainedModel <- train(TrainingData,
                              TrainingLabels,
                              method = x,
                              metric = EvalMetric,
                              trControl = TrainingControl)
    
    return(TrainedModel)
  })
  
  return(AllModels)
}


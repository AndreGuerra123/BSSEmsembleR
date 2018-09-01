library("caret")
library("mlbench")
library("pROC")
library("rpart")
library("caretEnsemble")


set.seed(123)

in_training <- createDataPartition(iris$Species, p = 0.6, list = FALSE)
training <- iris[in_training, ]
testing <- iris[-in_training, ]

legacyControl <- trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  selectionFunction = "best",
  preProcOptions = list(p = 10))

legacyModel<- train(x = subset(training, select = -Species),
              y = training$Species,
              preProcess = c("center", "scale"),
              trControl = legacyControl,
              method = "nnet",
              metric = "accuracy",
              maximize=TRUE)

predictionsLegacy <- predict(legacyModel, testing)

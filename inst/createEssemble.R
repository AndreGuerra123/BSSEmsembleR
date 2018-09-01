library("caret")
library("mlbench")
library("pROC")
library("rpart")
library("caretEnsemble")
library("caTools")

data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
)

model_list <- caretList(
  Class~., data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
)
p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)
greedy_ensemble <- caretEnsemble(
      model_list,
       metric="ROC",
       trControl=trainControl(
            number=2,
            summaryFunction=twoClassSummary,
            classProbs=TRUE
         ))
summary(greedy_ensemble)

model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$Class)


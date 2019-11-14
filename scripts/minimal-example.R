library(caret)

library(doMC)
registerDoMC(cores = 1)

data("iris")

set.seed(825)

# To create a stratified repeated k-fold cross validation
multiIndexes<-createMultiFolds(y=iris$Species, k = 10, times = 1)

fitControl <- trainControl(index = multiIndexes, savePredictions = TRUE, search="grid", allowParallel= TRUE, 
                           summaryFunction = multiClassSummary, verboseIter = FALSE, classProbs=TRUE)
                
# methods that work ok: rf, AdaBoost.M1, AdaBag, bagFDA, C5.0, cforest, ctree, gbm, glmnet, pam, rpart, RRF, 
# sparseLDA, xgbLinear, xgbTree, JRip, J48, PART, LMT

# Para eliminar

# ordinalNet, glmboost, bartMachine, rpartCost, deepboost, ORFlog, ORFpls, ORFridge, ORFsvm, BstLm, bstSm, 
# PenalizedLDA, wsrf, rFerns, bagEarth, glmboost, nodeHarvest, ordinalNet, extraTrees

#execute the algorithm
modelFit <- train(Species ~ ., data = iris, 
                    method= "AdaBag",
                    metric = "AUC",
                    maximize = TRUE,
                    tuneLength = 2,
                    trControl = fitControl)
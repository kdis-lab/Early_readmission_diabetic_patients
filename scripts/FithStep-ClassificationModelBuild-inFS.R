library(caret)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/Diabetes-paper/")

source("Functions.R")

## All subsequent models are then run in parallel
#Not available in Windows
#library(doMC)
#registerDoMC(cores = 2)

#Required packages
mydata <- read.csv("diabetic_data-fourthStep-preproc.csv")

#It is necessary to install the libraries pcre, pcre-devel, libicu, libicu-dev, xz, xz-devel, xz-libs in the cluster RockOS

wants <- c("caret", "adabag", "gbm", "randomForest", "RWeka", "adabag", "plyr", "earth", "mda", "C50", "party", "Matrix", 
           "pamr", "rpart", "RRF", "sparseLDA", "xgboost", "ROCR", "Metrics", "RWeka")

# xgboost has problem in the cluster. The algorithms affected are xgbTree, xgbLinear

has   <- wants %in% rownames(installed.packages())

#if(any(!has)) install.packages(wants[!has])

algorithms <-c("rf", "AdaBoost.M1", "AdaBag", "bagFDA", "C5.0", "cforest", "gbm", "glmnet", "pam", "rpart", "RRF", 
               "sparseLDA", "xgbLinear", "xgbTree", "JRip", "J48", "PART", "LMT")

algorithms <- c("xgbLinear")

set.seed(825)

#we use an automatic grid by means of using the parameter tunelength
# see http://machinelearningmastery.com/tuning-machine-learning-models-using-the-caret-r-package/

fitControl <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE,
                    savePredictions = TRUE, search="grid", allowParallel= TRUE, 
                    summaryFunction = multiClassSummary, verboseIter = FALSE)

#execute the algorithms
for(algorithm in algorithms){

  modelFit <- train(readmitted ~ ., data = mydata, 
                    method=algorithm,
                    metric = "AUC",
                    maximize = TRUE,
                    tuneLength = 10,
                    trControl = fitControl)
  
  #Saving all the results
  write.table(modelFit$results, file= paste("results-classification/results-",algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE, na = "")
  
  #Saving the variable importance
  varImportance <- varImp(modelFit, scale=FALSE)
  
  write.table(varImportance$importance, file= paste("results-classification/varImportance-",algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")
  
  #Saving the model for graphing plots a posteriory.
  # save the model to disk
  saveRDS(modelFit, paste("results-classification/modelfit-",algorithm, ".rds", sep = ""))
}

library(caret)
library(ggplot2)
library(plotROC)
library(pROC)

path <- "D:/OneDrive - Universidad de Córdoba/workspace/Diabetes-paper/diabetic_data-fourthStep-preproc"

mydata <- read.csv(paste(path,".csv",sep = ""), sep = ",")

rfFuncs$summary <- multiClassSummary

set.seed(10)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv", 
                   repeats = 1,
                   verbose = FALSE)

subsets <- c(1:28)

rfProfile <- rfe(x= mydata[,-ncol(mydata)], y= mydata[,ncol(mydata)], sizes = subsets,
                 rfeControl =  ctrl, metric = "ROC")

rfProfile
plot(rfProfile)

#allowParallel = TRUE,
#xgbLinear
# use the modelLookup function to see which model parameters are available. 
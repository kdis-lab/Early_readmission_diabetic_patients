library(ggplot2)
library(caret)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/Diabetes-paper/")

output <- "results-classification/modelfit-"

algorithmNames <- c("bagFDA")
metrics <-c("AUC","Kappa","MCC")

for(algorithmName in algorithmNames){
  
  # load the model
  modelFit <- readRDS(paste(output,algorithmName, ".rds", sep = ""))
  
  ggplot(varImp(modelFit, scale=FALSE))
  
  ggsave(width = 8, height = 15, filename = paste(output,algorithmName,"-varImp.png",sep = ""), bg = "transparent")
  
  #for each metric
  for(metric in metrics){
    
    modelFit$metric <- metric
    
    ggplot(modelFit)
    
    ggsave(width = 8, height = 5, filename = paste(output,algorithmName,"-",metric,".png",sep = ""), bg = "transparent")
  }
}
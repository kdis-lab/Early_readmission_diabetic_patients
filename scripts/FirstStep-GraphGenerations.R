library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
library(ggplot2)
library(ggsignif)
library(plyr)
library(caret)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/Diabetes-paper/")

outputFolder <- "resultsv2/"

dataset <- read.csv("diabetic_data-fourthStep-preproc.csv", na.strings = "")

cnames <- colnames(dataset)

combinationsT <- list(c("<30",">30"),c("<30","NO"), c(">30","NO"))

cdatMeans <- ddply(dataset, "readmitted", summarise, 
                   num_lab_procedures=mean(num_lab_procedures), 
                   num_medications=mean(num_medications),
                   num_procedures=mean(num_procedures),
                   num_diagnoses=mean(num_diagnoses),
                   num_inpatient=mean(num_inpatient),
                   time_in_hospital=mean(time_in_hospital))

coordinatesList <- list(num_lab_procedures = c(3.2,4,3.2), num_medications=c(3,3.5,3), num_procedures= c(2,2.3,2), num_diagnoses=c(9.5,10.5,9.5),
                        num_outpatient= c(44,47,44), num_emergency=c(82,87,82), num_inpatient= c(3,3.5,3), time_in_hospital= c(2.3, 2.6, 2.3))

# Displaying graphs by type of var
for(c in cnames){
  
  #Create a bar plot for factor variables
  if(class(dataset[,c])=="factor"){
    
    ggplot(dataset, aes_string(x=c, fill="readmitted")) +
      geom_bar()
    
    ggsave(width = 10, height = 5, 
           filename = paste(outputFolder, c, "-barplots",".png",sep = ""), bg = "transparent")
  }
  
  # Box plots of each numeric variable
  # Density plots with means
  if(class(dataset[,c])== "integer" || class(dataset[,c])== "numeric"){
    
    ggplot(dataset, aes_string(x="readmitted", y= c)) + 
      stat_boxplot(geom = "errorbar", width = 0.5) + geom_boxplot() + 
      stat_summary(fun.y=mean, geom="point", shape=1, size=3)+
      geom_signif(comparisons = combinationsT, map_signif_level=TRUE, test = "wilcox.test", y_position = coordinatesList[[c]])
    
    ggsave(width = 5, height = 5, filename = paste(outputFolder, c,"-bloxpot",".png",sep = ""), bg = "transparent")
    
    ggplot(dataset, aes_string(x=c, colour="readmitted")) + geom_density()+ 
      geom_vline(data=cdatMeans, aes_string(xintercept= c, colour="readmitted"),
                 linetype="dashed", size=1)
    
    ggsave(width = 10, height = 5, filename = paste(outputFolder, c,"-densityplot",".png",sep = ""), bg = "transparent")
  }
}
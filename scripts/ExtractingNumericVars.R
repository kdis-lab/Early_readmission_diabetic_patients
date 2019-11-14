setwd("D:/OneDrive - Universidad de Córdoba/workspace/Diabetes-paper/")

mydata <- read.csv("diabetic_data-fourthStep-preproc.csv")

newData <- mydata[,c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "num_inpatient", 
                     "num_diagnoses", "readmitted")]

newData <- newData[!duplicated(newData[,1:(ncol(newData)-1)]),]

write.table(newData, file= "diabetic_data-only-numeric.csv",
            quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE, na = "")
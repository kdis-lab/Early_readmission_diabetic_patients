path <- "D:/OneDrive - Universidad de Córdoba/workspace/Diabetes-paper/"

classNames <- c("<30",">30","NO")

mydata <- read.csv(paste(path,"diabetic_data-firstStep.csv", sep = ""), sep = ",")

# For a given continuous variable, outliers are those observations that lie 
# outside 1.5 *IQR, where IQR, the 'Inter Quartile Range' is the difference between
# 75th and 25th quartiles. The outliers are removed with the median value of the group that they belong.

#function that takes a vector of data and a coefficient,
#returns boolean vector if a certain point is an outlier or not
remove_outliers <- function(varT, mydata, coef=1.5){
  
  vAll <- mydata[,varT]
  
  #for each class
  for(ClassType in classNames){
    
    vClass <- mydata[mydata$readmitted == ClassType, varT]
    
    quantilesG <- quantile(vClass,probs=c(0.25,0.75), na.rm = TRUE)
    
    IQRG <- quantilesG[2]-quantilesG[1]
    
    indexes <- (vAll < (quantilesG[1]-coef*IQRG) | vAll > (quantilesG[2]+coef*IQRG)) & mydata$readmitted == ClassType
    
    print(paste("Ouliers of var ", varT, "in class ", ClassType,": ", sum(indexes), sep = ""))
    
    mydata[indexes, varT] <- median(vClass, na.rm =TRUE)
    
  }
  
  return (mydata)
}

colNamesT <- colnames(mydata)[1:(ncol(mydata)-1)]

# Remove the outliers in each column
for(nameC in colNamesT){
  
  if(class(mydata[,nameC])=="integer" || class(mydata[,nameC])=="numeric")
  {
    mydata <- remove_outliers(nameC,mydata)
  }
}

#removing the atts num_emergency and num_outpatient because all their values are equal to 0 after removing outliers.
mydata$num_emergency <- NULL
mydata$num_outpatient <- NULL

#detecting inconsistent or dupplicated examples

mydata <- mydata[!duplicated(mydata[,1:(ncol(mydata)-1)]),]

ncol(mydata)
nrow(mydata)

write.table(mydata, file = paste(path,"diabetic_data-secondStep.csv",sep = ""),
            quote = FALSE,
            sep = "," ,
            row.names = FALSE, col.names = TRUE, na = "")
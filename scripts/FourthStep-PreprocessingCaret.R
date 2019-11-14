# La importancia del centrado se puede consultar en
# http://www.theanalysisfactor.com/center-on-the-mean/
# Robert et al. Centering, scaling, and transformations: improving 
#the biological information content of metabolomics data

library(caret)

#We use the center method (subtracts the mean of the predictor's data). We focused in the differences

#We use the scale method (divides by the standard deviation). All numerical predictors have the same importance

#we use the zv method (identifies numeric predictor columns with a single value 
#(i.e. having zero variance) and excludes them from further calculations.)

#We use the Yeo-Johnson transformation (it is similar to the Box-Cox model but can accommodate 
#predictors with zero and/or negative values (while the predictors values for the Box-Cox 
#transformation must be strictly positive.)

#The operations are applied in this order: zero-variance filter, Yeo-Johnson, centering, scaling
# load the database
path <- "D:/OneDrive - Universidad de Córdoba/workspace/Diabetes-paper/"

database <-read.csv(paste(path,"diabetic_data-secondStep.csv", sep = ""))

methods <- c("zv","YeoJohnson","center","scale")

preProcValues <- preProcess(database, method = methods, outcome = database$Class)

databaseTransformed <- predict(preProcValues, database)

write.table(databaseTransformed, file= paste(path,"diabetic_data-fourthStep-preproc.csv",sep = ""),
            quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE, na = "")
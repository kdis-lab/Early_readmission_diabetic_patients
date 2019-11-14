library(caret)

path <- "D:/OneDrive - Universidad de Córdoba/workspace/Diabetes-paper/"
dataset <- read.csv(paste(path,"diabetic_data-original.csv",sep = ""), na.strings = "")

nrow(dataset)
ncol(dataset)

summary(dataset)

# The encounter id and patient number are removed. They do not apport any useful information.
dataset$encounter_id <- NULL

dataset$patient_nbr<- NULL

# putting all NA values in race to a new level named Missing

vectLevels <- levels(dataset$race)

vectLevels <-c(vectLevels, "Missing")

levels(dataset$race) <- vectLevels

dataset[is.na(dataset$race),"race"]<- "Missing"

# delete the encounters with gender equal to "Unknown/Invalid"
dataset <- dataset[dataset$gender != "Unknown/Invalid",]

#Reducing the levels of variable age to three, as proposed in the original paper
vLevels <- levels(dataset$age)

levels(dataset$age) <- c(vLevels,"[0-30)","[30-60)","[60-100)")

dataset[dataset$age %in% c("[0-10)", "[10-20)", "[20-30)"),"age"]<- "[0-30)"
dataset[dataset$age %in% c("[30-40)", "[40-50)", "[50-60)"),"age"]<- "[30-60)"
dataset[dataset$age %in% c("[60-70)", "[70-80)", "[80-90)","[90-100)"),"age"]<- "[60-100)"

# remove the weight att, it is very sparce
dataset$weight <- NULL

# Reducing the levels of admission_type_id to eigth levels
dataset$admission_type_id <-as.factor(dataset$admission_type_id)

vLevels <- levels(dataset$admission_type_id)

levels(dataset$admission_type_id) <- c(vLevels,"Emergency","Urgent","Elective","Newborn","Not_Available","NULL","Trauma_Center","Not_Mapped")

dataset[dataset$admission_type_id==1,"admission_type_id"]<- "Emergency"
dataset[dataset$admission_type_id==2,"admission_type_id"]<- "Urgent"
dataset[dataset$admission_type_id==3,"admission_type_id"]<- "Elective"
dataset[dataset$admission_type_id==4,"admission_type_id"]<- "Newborn"
dataset[dataset$admission_type_id==5,"admission_type_id"]<- "Not_Available"
dataset[dataset$admission_type_id==6,"admission_type_id"]<- "NULL"
dataset[dataset$admission_type_id==7,"admission_type_id"]<- "Trauma_Center"
dataset[dataset$admission_type_id==8,"admission_type_id"]<- "Not_Mapped"

#Reducing the levels of discharge disposition to two levels
dataset$discharge_disposition_id <-as.factor(dataset$discharge_disposition_id)

vLevels <- levels(dataset$discharge_disposition_id)

levels(dataset$discharge_disposition_id) <- c(vLevels,"To_home","Otherwise")

#Remove all encounters that resulted in either discharge to a hospice or patient death. This operation was made in the original paper.
dataset <- dataset[!dataset$discharge_disposition_id %in% c(11,13,14,19:21),]

dataset[dataset$discharge_disposition_id %in% c(1,8),"discharge_disposition_id"]<- "To_home"

dataset[dataset$discharge_disposition_id %in% c(2:7,9,10,12,15:18,22:30),"discharge_disposition_id"]<- "Otherwise"

#Reducing the levels of admission_source_id to three levels

dataset$admission_source_id <-as.factor(dataset$admission_source_id)

vLevels <- levels(dataset$admission_source_id)

levels(dataset$admission_source_id) <- c(vLevels,"Emergency_Room","Physician/Clinical_Referal","Otherwise")

dataset[dataset$admission_source_id==7, "admission_source_id"] <- "Emergency_Room"
dataset[dataset$admission_source_id %in% c(1:3), "admission_source_id"] <- "Physician/Clinical_Referal"
dataset[dataset$admission_source_id %in% c(4:6,8:26), "admission_source_id"] <- "Otherwise"

# nothing to do with the att time_in_hospital

# remove the payer code, it does not give any useful information
dataset$payer_code <- NULL

# Reducing the levels of medical_specialty to 6 levels.

vectLevels <- levels(dataset$medical_specialty)

vectLevels <-c(vectLevels, "Surgery", "Missing", "Other")

levels(dataset$medical_specialty) <- vectLevels
  
#Putting all NA values in medical_specialty to a new level named Unknown
dataset[is.na(dataset$medical_specialty),"medical_specialty"]<- "Missing"

#The encounters of InternalMedicine and Family/GeneralPractice are not changed

#The encounters of Cardiology are joined
dataset[startsWith(as.character(dataset$medical_specialty),"Cardiology"),"medical_specialty"]<- "Cardiology"

#The encounters of Surgery are joined
dataset[startsWith(as.character(dataset$medical_specialty),"Surgery"),"medical_specialty"]<- "Surgery"

dataset[dataset$medical_specialty=="SurgicalSpecialty","medical_specialty"]<- "Surgery"

dataset[!dataset$medical_specialty %in% c("InternalMedicine","Family/GeneralPractice","Cardiology","Surgery","Missing"),"medical_specialty"]<- "Other"

#Nothing to do with the att num_lab_procedures, num_procedures, num_medications, num_outpatient, num_emergency, num_inpatient

# grouping diag_1 values in 9 groups defined in published paper

groupCirculatory<-as.factor(c(390:459,785))
groupResp<-as.factor(c(460:519,786))
groupDigest<-as.factor(c(520:579,783,787))
groupInj<-as.factor(c(800:999))
groupMusc<-as.factor(c(710:739))
groupNeo<-as.factor(c(140:239))
groupGeni<-as.factor(c(580:629,788))
groupOthers <- as.factor(c(780,781,784,789:799,240:249,251:279,680:709,782,1:139,290:319,280:289,320:359,630:679,360:389,365.44,740:759))

newLevels <- c("Circ.","Resp.","Diges.","Diab.","Inj.","Musc.","Geni.",
               "Neop.", "Other")

for(diag in c("diag_1","diag_2","diag_3")){
  
  oldLevels <- levels(dataset[,diag])  
  
  levels(dataset[,diag]) <- c(oldLevels,newLevels)
  
  # putting all NA values in diag to a new level named Unknown.
  
  dataset[is.na(dataset[,diag]),diag]<- "Other"
  
  dataset[dataset[,diag] %in% groupCirculatory, diag]<-"Circ."
  
  dataset[dataset[,diag] %in% groupResp, diag]<-"Resp."
  
  dataset[dataset[,diag] %in% groupDigest, diag]<-"Diges."
  
  dataset[startsWith(as.character(dataset[,diag]),"250."), diag] <- "Diab."
  
  dataset[dataset[,diag]==250,diag] <- "Diab."
  
  dataset[dataset[,diag] %in% groupInj, diag]<-"Inj."
  
  dataset[dataset[,diag] %in% groupMusc, diag]<-"Musc."
  
  dataset[dataset[,diag] %in% groupGeni, diag]<-"Geni."
  
  dataset[dataset[,diag] %in% groupNeo, diag]<-"Neop."
  
  dataset[dataset[,diag] %in% groupOthers, diag]<- "Other"
  
  dataset[startsWith(as.character(dataset[,diag]),"E"), diag]<-"Other"
  
  dataset[startsWith(as.character(dataset[,diag]),"V"), diag]<-"Other"
  
}

#nothing to do with the att num_diagnoses

#nothing to do with the atts max_glu_serum A1Cresult

#Removing drugs that were never or scarcely prescribed

dataset$acetohexamide <- NULL

dataset$tolbutamide <- NULL

dataset$troglitazone <- NULL

dataset$examide <- NULL

dataset$citoglipton <- NULL

dataset$glipizide.metformin <- NULL

dataset$glimepiride.pioglitazone <- NULL

dataset$metformin.rosiglitazone <- NULL

dataset$metformin.pioglitazone <- NULL

dataset$tolazamide <- NULL

dataset$miglitol <- NULL

dataset$acarbose<- NULL

dataset$glyburide.metformin<- NULL

dataset$chlorpropamide<- NULL

dataset$nateglinide<- NULL

#nothing to do with the att change

#nothing to do with the att diabetesMed

# nothing to do with the att readmitted

#After transforming, some encounters may be equal, therefore we should remove the duplicated instances

dataset <- dataset[!duplicated(dataset[,1:(ncol(dataset)-1)]),]

nrow(dataset)
ncol(dataset)

write.table(dataset, file = paste(path,"diabetic_data-firstStep.csv",sep = ""),
            quote = FALSE,
            sep = "," ,
            row.names = FALSE, col.names = TRUE, na = "")
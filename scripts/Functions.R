#Multi-Class Summary Function
#Based on caret:::twoClassSummary
require(compiler)

multiClassSummary <- cmpfun(function (data, lev = NULL, model = NULL){
  
  #Load Libraries
  require(Metrics)
  require(caret)
  
  allObs <- data[,  "obs"]
  obsLevels <- levels(allObs)
  
  #Check data
  if (!all(levels(data[, "pred"]) == obsLevels)) 
    stop("levels of observed and predicted data do not match")
  
  #Calculate custom one-vs-all stats for each class
  prob_stats <- lapply(levels(data[, "pred"]), function(class){
    
    #Grab one-vs-all data for the class
    pred <- ifelse(data[, "pred"] == class, 1, 0)
    obs  <- ifelse(data[,  "obs"] == class, 1, 0)
    prob <- data[,class]
    
    #Calculate one-vs-all logLoss and return
    cap_prob <- pmin(pmax(prob, .000001), .999999)
    prob_stats <- c(logLoss(obs, cap_prob))
    names(prob_stats) <- c('logLoss')
    return(prob_stats) 
  })
  
  prob_stats <- do.call(rbind, prob_stats)
  rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
  
  #Initialize variable
  aucs <- c()
  
  #Calculate pair-wise AUCs as described in Huang and Ling 2005, p. 305
  #based on Hand and Till 2001
  for(iClass in 1:length(obsLevels)) {
    class <- obsLevels[iClass]
    
    for (iInnerClass in 1:length(obsLevels)) {
      if (iInnerClass > iClass) {
        #Since AUCs are calculated pair-wise in every iteration, only the remaining classes 
        #which have not been compared yet are relevant
        innerClass <- obsLevels[iInnerClass]
        
        #In order to calculate pair-wise AUCs, use only observations with labels class and innerClass
        classFilter <- allObs == class | allObs == innerClass
        
        obsClass  <- ifelse(allObs[classFilter] == class, 1, 0)
        probClass <- data[classFilter, as.character(class)]
        aucClass <- Metrics::auc(obsClass, probClass)
        
        obsInner  <- ifelse(allObs[classFilter] == innerClass, 1, 0)
        probInner <- data[classFilter, as.character(innerClass)]
        aucInner <- Metrics::auc(obsInner, probInner)
        
        #print(paste("AUCs: ", aucClass, ", ", aucInner))
        aucs <- c(aucs, (aucClass + aucInner) / 2)
      }
    }
  }
  
  #Average AUC values for every pair-wise AUC value
  finalAUC <- sum(aucs) * 2 / (length(obsLevels) * (length(obsLevels) - 1))
  names(finalAUC) <- 'AUC'
  
  #Calculate confusion matrix-based statistics
  CM <- confusionMatrix(data[, "pred"], data[, "obs"])
  
  #Aggregate and average class-wise stats
  #Todo: add weights
  class_stats <- cbind(CM$byClass, prob_stats)
  class_stats <- colMeans(class_stats)
  
  #Aggregate overall stats
  overall_stats <- c(CM$overall)
  
  #Computing the Mathew Correlation coefficient  
  numerator <- 0
  denom1 <- 0
  denom2 <- 0
  
  rangeC <- c(1:length(obsLevels))
  
  for(k in rangeC){
    
    b1 <- 0
    b2 <- 0
    
    c1 <- 0
    c2 <- 0
    
    for(l in rangeC){
      
      b1 <- b1 + CM$table[k,l]
      b2 <- b2 + CM$table[l,k]
      
      for(m in rangeC){
        numerator <- numerator + CM$table[k,k]*CM$table[l,m]- CM$table[k,l]*CM$table[m,k]
      } 
    }
    
    for(f in rangeC[-k]){
      
      for(g in rangeC){
        
        c1 <- c1+ CM$table[f,g]
        c2 <- c2+ CM$table[g,f]
      }
    }
    
    denom1 <- denom1 + b1*c1
    denom2 <- denom2 + b2*c2
  }
  
  denominator <- sqrt(denom1)*sqrt(denom2)
  
  MCC <- NaN
  
  if(denominator > 0)
  {MCC <- numerator/denominator}
  
  names(MCC) <- 'MCC'
  
  #Combine overall with class-wise stats and remove some stats we don't want 
  stats <- c(overall_stats, class_stats, finalAUC, MCC)
  stats <- stats[! names(stats) %in% c('AccuracyNull', 
                                       'Prevalence', 'Detection Prevalence')]
  
  #Clean names and return
  names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
  
  print(class(stats))
  return(stats)
})
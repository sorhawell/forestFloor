importanceExportWrapper = function(rf, type = NULL,class = NULL, scale = NULL) {
  
  Prioritizedtype = c(
    "%IncMSE",                #regression1
    "IncNodePurity",          #regression2
    "MeanDecreaseAccuracy",    #classification1
    "MeanDecreaseGini"        #classification2
  )
  
  if(is.null(scale)) scale = TRUE #scale, has no null fix
  
  if(is.null(type)) {
    if( 
    !is.null(colnames(rf$importance)) && 
    any(Prioritizedtype[c(1,3)] %in% colnames(rf$importance))
  ) type = 1 else type = 2
  }
  

  #first try...
  impOut = randomForest::importance(rf,type=type, class=class, scale=scale)

  #check and a catch before returning
  
  #to many cols
  if(!is.null(impOut) && dim(impOut)[2]>1) {
    warning("found more than one type of importance, revert to fallback")
    impOut = NULL
  }
  
  #no cols
  if(!is.null(impOut) && dim(impOut)[2]<1) {
    warning("found nothing of importance, revert to fallback")
    impOut = NULL
  }
  
  #no length
  if(!length(impOut)) {
    warning("found nothing of importance, revert to fallback")
    impOut = NULL
  } 
  
  #... and the fallback
  if(is.null(impOut)) {
    
    #what to look for
    Prioritizedtype = c(
      "%IncMSE",                #regression1
      "IncNodePurity",          #regression2
      "MeanDecreaseAccuracy",    #classification1
      "MeanDecreaseGini"        #classification2
    )
    
    #fetch everything and pick the best
    allImportance = randomForest::importance(rf)
    bestOption = na.omit(match(Prioritizedtype,colnames(allImportance)))[1]
    bestOptionName = Prioritizedtype[Prioritizedtype %in% colnames(allImportance)][1]
    if(length(bestOption)!=1) stop("exporting any importance failed")
    bestAvailabletype = allImportance[,bestOption]
    cat("fallback, exported type of importance is: ", bestOptionName)
    impOut = bestAvailabletype
  }   
  
  return(impOut)
}

forestFloor = function(rf.fit,
                       X,
                       calc_np = FALSE,
                       binary_reg = FALSE,
                       ...) {
  Class = class(rf.fit)[1] #read only first class
  
  #convert randomForest.formula
  if(inherits(rf.fit,"randomForest.formula")){
    X <- as.data.frame(X)
    rn <- row.names(X)
    Terms <- delete.response(rf.fit$terms)
    X <- model.frame(Terms, X, na.action = na.fail)
  }
    
  #randomForest::randomForest or trimTrees::cinbag or rfPermute::rfPermute
  if(inherits(rf.fit,"randomForest")) {
    if(Class=="rfPermute") print("class 'rfPermute' supported as 'randomForest'")
    Type = rf.fit$type
    #changed classification to binary regression if requested and only two classes
    if(binary_reg) {
      if(!is.null(rf.fit$forest$nclass) && rf.fit$forest$nclass==2) {
        Type="regression"
      } else {
        warning("binary_reg=T is not possible for >2 classes. 
                Continue computation as multiClass")
        
      }
    }
  
    #dispatch either forestFloor_regression(and binary) or multiClassification
    switch(Type,
           regression =     return(forestFloor_randomForest_regression(rf.fit,
                                                                       X,
                                                                       calc_np,
                                                                       binary_reg,
                                                                       ...)),
           classification = return(forestFloor_randomForest_multiClass(rf.fit,
                                                                       X,
                                                                       calc_np,
                                                                       binary_reg,
                                                                       ...)),
           stop("type of randomForest object is neither 'regression' or 'classification', (RF.fit$type==?)"))
  }
  
  #other classes not supported...
  if(Class=="forestFloor_external") {
    print("forestFloor_external is a standardised treemodelfit which is not implemented yet")
    return("... cold emptyness")
  }
  
  #if not classses recognized
  stop("this class is not yet supported. Make me a request on email and we can talk about it")

}
   
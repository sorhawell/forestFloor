forestFloor = function(rf.fit,
                       X,
                       Xtest = NULL,
                       calc_np = FALSE,
                       binary_reg = FALSE,
                       bootstrapFC = FALSE,
                       ...) {
  
  #A very loose initial class check to avoid non-informatives error messsages
  if(!any(class(rf.fit) %in% c("randomForest","train","forestFloor_external","list"))) {
    stop("The rf.fit argument is not valid, should be 'randomForest' or 'train'")
  }

  #extract random model from caret object
  if(inherits(rf.fit,"train")) {
    message("This seems to be an caret object. Trying to extract 'randomForest' model")
    message("caret support is experimental, see help(forestFloor) for an example with caret")
    message("ask/complain freely at https://github.com/sorhawell/forestFloor/issues/27")

    #simple as that...
    rf.fit = rf.fit$finalModel
  }
  
  #convert randomForest.formula
  if(inherits(rf.fit,"randomForest.formula")){
    X <- as.data.frame(X)
    rn <- row.names(X)
    Terms <- delete.response(rf.fit$terms)
    X <- model.frame(Terms, X, na.action = na.fail)
    if(!is.null(Xtest)){
      Xtest <- model.frame(Terms, Xtest, na.action = na.fail)
    }
  }
  
  #warn and someday fix not-inbag/not-OOB problems
  if(any(is.na(rf.fit$predicted))) {
    warning("forestFloor: NA predictions in rf-object. Try to train with more trees(ntree)")
    message("forestFloor: NA predictions in rf-object. Try to train with more trees(ntree)")
    # is.na.ind = which(is.na(rf.fit$predicted))
    # rf$predicted = rf$predicted[-is.na.ind ]
    # rf$oob.times = rf$oob.times[-is.na.ind ]
    # rf$votes     = rf$votes    [-is.na.ind,]
    # X = X[-is.na.ind,]
    # #length(rf$y)
    # #sapply(rf,length)
  }
  
  #randomForest::randomForest or trimTrees::cinbag or rfPermute::rfPermute
  if(inherits(rf.fit,"randomForest")) {
    if(inherits(rf.fit,"rfPermute")) print("class 'rfPermute' supported as 'randomForest'")
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
           regression = return(
             forestFloor_randomForest_regression(
               rf.fit      = rf.fit,
               X           = X,
               Xtest       = Xtest,
               calc_np     = calc_np,
               binary_reg  = binary_reg,
               bootstrapFC = bootstrapFC,
               ...)
           ),
           classification = return(
             forestFloor_randomForest_multiClass(
               rf.fit      = rf.fit,
               X           = X,
               Xtest       = Xtest,
               calc_np     = calc_np,
               binary_reg  = binary_reg,
               bootstrapFC = bootstrapFC,
               ...)
           ),#majorityTerminal
           stop("type of randomForest object is neither 'regression' or 'classification', (RF.fit$type==?)"))
  }

  #other classes not supported...
  if(inherits(rf.fit,"forestFloor_external")) {
    print("forestFloor_external is a standardised treemodelfit which is not implemented yet")
    return("... cold emptyness")
  }

  #if not classses recognized
  stop("This class is not yet supported, is this a random forest model fit?")

}

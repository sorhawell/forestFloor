
forestFloor = function(rf.fit,
                       X,
                       calc_np = FALSE,
                       binary_reg = FALSE,
                       ...) {
  Class = class(rf.fit)
  #randomForest::randomForest or trimTrees::cinbag
  if(Class=="randomForest") {
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
  
  #other models...
  if(Class=="forestFloor_external") {
    print("forestFloor_external is a standardised treemodelfit which is not implemented yet")
    return("... cold emptyness")
  }
  
  #if not classses recognized
  stop("this class is not yet supported. Make me a request on email and we can talk about it")

}
   

##method to compute forestFloor_regression
forestFloor_randomForest_regression <- function(rf.fit,
                                                  X,
                                                  calc_np = FALSE,
                                                  binary_reg = FALSE,
                                                  ...) { 
  
  #check the rf.fitbject have a inbag
  if(is.null(rf.fit$inbag)) stop("input randomForest-object have no inbag, set keep.inbag=T,
                              try, randomForest(X,Y,keep.inbag=T) for regression where Y is numeric
                              and, cinbag(X,Y,keep.inbag=T,keep.forest=T) for binary-class where Y is factor
                              ..cinbag is from trimTrees package...
                              error condition: if(is.null(rf.fit$inbag))")
  
  #make node status a integer matrix
  ns = rf.fit$forest$nodestatus
  storage.mode(ns) = "integer"
  
  
  #translate binary classification RF-object, to regression mode
  if(rf.fit$type=="classification") {
    if(length(levels(rf.fit$y))!=2) stop("must be binary classification to use regression mode.
                                      error condition: if(length(levels(rf.fit$y))!=2")
    print("RF is classification, converting factors/categories to numeric 0 an 1")
    Y = as.numeric((rf.fit$y))-1
    cat(" defining",levels(rf.fit$y)[1]," as 0\n defining",levels(rf.fit$y)[2],"as 1")
    rf.fit$forest$leftDaughter  = rf.fit$forest$treemap[,1,] #translate daughter representation to regression mode
    rf.fit$forest$rightDaughter = rf.fit$forest$treemap[,2,] 
    ns[ns==1] = -3  ##translate nodestatus representation to regression mode
    if(is.null("rf.fit$inbagCount") && (is.null(rf.fit$call$replace) || rf.fit$call$replace)) {
stop("cannot compute classification forestFloor for
randomForest::randomForest when trained with replace=T.
Train forest with cinbag::trimTrees instead of randomForest().
Or set reaplace = FALSE.  The two functions are identical,
except cinbag() entails a more detailed inbag record, which is
needed to estimate binary node probabilities.")
    }
    if(!calc_np) stop("node predictions must be re-calculated for random forest of type classification, set calc_np=T)
                      error conditions: if(!calc_np && rf.fit$type='classification')")
    
    if(is.null(rf.fit$inbagCount)) inbag = rf.fit$inbag else inbag = rf.fit$inbagCount
  } else {
    Y=rf.fit$y
    inbag = rf.fit$inbag
  }
  
  
  #preparing data, indice-correction could be moved to C++
  #a - This should be fethed from RF-object, flat interface
  ld = rf.fit$forest$leftDaughter-1 #indice correction, first element is 0 in C++ and 1 in R.
  storage.mode(ld) = "integer"
  rd = rf.fit$forest$rightDaughter-1
  storage.mode(rd) = "integer"
  bv = rf.fit$forest$bestvar-1
  storage.mode(bv) = "integer"
  np = rf.fit$forest$nodepred
  storage.mode(np) = "double"
  bs = rf.fit$forest$xbestsplit
  storage.mode(bs) = "double"
  ib = inbag
  storage.mode(ib) = "integer"
  Yd = as.numeric(Y)
  storage.mode(Yd) = "double"
  ot  = rf.fit$oob.times
  storage.mode(ot) = "integer"
  
  
  ##recording types of variables
  xlevels = unlist(lapply(rf.fit$forest$xlevels,length),use.names=F)
  xl = xlevels
  storage.mode(xl) = "integer"
  varsToBeConverted = xlevels>1
  
  ##Converting X to Xd, all factors change to level numbers
  Xd=X
  for(i in 1:dim(Xd)[2]) {
    if(varsToBeConverted[i]) {
      Xd[,i] = as.numeric(Xd[,i])-1  
    }
  }  
  Xd=as.matrix(Xd)
  storage.mode(Xd) = "double"
  
  #outout variable
  localIncrements = Xd*0
  storage.mode(localIncrements) = "double"
  
  #should activities of nodes be reestimated(true) or reused from randomForest object(false)
  calculate_node_pred=calc_np
  
  # C++ function, recursively finding increments of all nodes of all trees
  # where OOB samples are present. vars, obs and ntree is "passed by number"
  # Anything else is passed by reference. Found increments are imediately
  # summed to localIncrements matrix.
  recTree(
    #passed by number
    vars=dim(X)[2], 
    obs=dim(X)[1],             
    ntree=rf.fit$ntree,
    calculate_node_pred=calculate_node_pred,
    #passed by reference
    X=Xd,  #training data, double matrix [obs,vars] 
    Y=Yd,
    leftDaughter = ld,  #row indices of left subnodes, integer matrix [nrnodes,ntree] 
    rightDaughter = rd, #...
    nodestatus = ns,    #weather node is terminal or not,      
    xbestsplit = bs,          
    nodepred = np,          
    bestvar = bv,
    inbag = ib,
    varLevels = xl,
    ot,  #oob.times
    localIncrements = localIncrements #output is written directly to localIncrements from C++
  )
  
  
  
  #writing out list
  imp = as.matrix(rf.fit$importance)[,1]
  out = list(X=X,Y=Y,
             importance = imp,
             imp_ind = sort(imp,decreasing=TRUE,index.return=TRUE)$ix,
             FCmatrix = localIncrements
  )
  class(out) = "forestFloor_regression"
  return(out)
}

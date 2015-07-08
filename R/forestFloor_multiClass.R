#test multiClass.cpp
forestFloor_multiClass = function(rfo,X) { 

  #check the RFobject have a inbag
  if(is.null(rfo$inbag)) stop("input randomForest-object have no inbag, set keep.inbag=T,
                              try, randomForest(X,Y,keep.inbag=T) for without replacement
                              and, cinbag(X,Y,keep.inbag=T,keep.forest=T) with replacement
                              ..cinbag is from trimTrees package...
                              error condition: if(is.null(rfo$inbag))")
  
  #make node status a integer matrix
  ns = rfo$forest$nodestatus
  storage.mode(ns) = "integer"
  
  
  #translate binary classification RF-object, to regression mode
  if(rfo$type=="classification") {
    #     if(length(levels(rfo$y))!=2) stop("no multiclass, must be binary classification.
    #                                       error condition: if(length(levels(rfo$y))!=2")
    print("RF is classification, converting factors/categories to numeric 0 an 1")
    
    rfo$forest$leftDaughter  = rfo$forest$treemap[,1,] #translate daughter representation to regression mode
    rfo$forest$rightDaughter = rfo$forest$treemap[,2,] 
    ns[ns==1] = -3  ##translate nodestatus representation to regression mode
    
    if(!is.null(rfo$inbagCount)) {
      inbag = rfo$inbagCount
    } else {
      if(!is.null(rfo$inbag)) {
        inbag = rfo$inbag
      } else {
        stop("error rfo$inbag or rfo$inbagCount is missing, 
             retrain forest with keep.inbag=TRUE")
      }
      }
    } else {
      stop("this function should only be run for classification")
  }
  
  
  #preparing data, indice-correction could be moved to C++
  #a - This should be fethed from RF-object, flat interface
  ld = rfo$forest$leftDaughter-1 #indice correction, first element is 0 in C++ and 1 in R.
  storage.mode(ld) = "integer"
  rd = rfo$forest$rightDaughter-1
  storage.mode(rd) = "integer"
  bv = rfo$forest$bestvar-1
  storage.mode(bv) = "integer"
  np = rfo$forest$nodepred
  storage.mode(np) = "double"
  bs = rfo$forest$xbestsplit
  storage.mode(bs) = "double"
  ib = inbag
  storage.mode(ib) = "integer"
  Yd = as.numeric(rfo$y)-1
  storage.mode(Yd) = "integer"
  ot  = rfo$oob.times
  storage.mode(ot) = "integer"
  
  
  ##recording types of variables
  xlevels = unlist(lapply(rfo$forest$xlevels,length),use.names=F)
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
  
  nClasses = as.integer(max(Yd))+1
  obs = length(Yd)
  vars=dim(X)[2]
  
  #outout variable
  localIncrements = rep(0.0,nClasses * obs * vars)
  storage.mode(localIncrements) = "double"
  
  
  # C++ function, recursively finding increments of all nodes of all trees
  # where OOB samples are present. vars, obs and ntree is "passed by number"
  # Anything else is passed by reference. Found increments are imediately
  # summed to localIncrements matrix.
  multiTree(
    #passed by number
    vars=vars, 
    obs=obs,             
    ntree=rfo$ntree,
    nClasses = nClasses,# changed from calculate node pred
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
  print("finished cpp")
  

  #restructure to feature contributions obs X var X class
  localIncrements = unlist(lapply(c(1:(nClasses-1),0),function(i) {
    localIncrements[(1:length(localIncrements))%%(nClasses)==i]
  }))
  
  
  #writing out list
  imp = as.matrix(rfo$importance)[,1]
  out = list(X=X,Y=rfo$y,
             importance = imp,
             imp_ind = sort(imp,decreasing=TRUE,index.return=TRUE)$ix,
             FCarray = array(localIncrements,dim=c(obs,vars,nClasses)),
             sumOfInbags = apply(rf$inbag,1,sum)
             #  all = mget(ls()) #export everything in list
  )

  class(out) = "forestFloor_multiClass"
  return(out)
}

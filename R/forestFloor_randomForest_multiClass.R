#test multiClass.cpp
forestFloor_randomForest_multiClass <- function(rf.fit,
                                     X,
                                     calc_np = FALSE,
                                     binary_reg = FALSE,
                                     majorityTerminal = TRUE
                                     ) { 

  #translate binary classification RF-object, to regression mode
  if(rf.fit$typ!="classification") stop("this function only handles type 'classification',
 but rf.fit$type!= 'classification'")
  
  #check the rf.fitbject have a inbag
  if(is.null(rf.fit$inbag)) stop("input randomForest-object have no inbag, set keep.inbag=T,
solution: randomForest(X,Y,keep.inbag=T).")
  
  #check if forest is saved
  if(is.null(rf.fit$forest)) stop("rf.fit$forest is null, try set keep.forest=TRUE during training")
  
  ##This line, allow legacy use of trimTrees::cinbag
  if(!is.null(rf.fit$inbagCount)) {
    rf.fit$inbag = rf.fit$inbagCount
    warning("rf.fit$inbagCount found. Are you still using trimTrees::cinbag?,
foerstFloor later than 1.8.9 supports classification w/wo replace directly, 
with randomForest")
  }
  
  #preparing data, indice-correction could be moved to C++
  #a - This should be fethed from RF-object, flat interface
  ns = rf.fit$forest$nodestatus
  storage.mode(ns) = "integer"
  ns[ns==1] = -3  ##translate nodestatus representation to regression mode
  
  rf.fit$forest$leftDaughter  = rf.fit$forest$treemap[,1,] #translate daughter representation to regression mode
  rf.fit$forest$rightDaughter = rf.fit$forest$treemap[,2,] 
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
  
  ib = rf.fit$inbag
  storage.mode(ib) = "integer"
  
  Yd = as.numeric(rf.fit$y)-1
  storage.mode(Yd) = "integer"
  
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
    ntree=rf.fit$ntree,
    nClasses = nClasses,# changed from calculate node pred
    #passed by reference
    X=Xd,  #training data, double matrix [obs,vars] 
    Y=Yd,
    majorityTerminal = majorityTerminal,
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
  

  #restructure to feature contributions obs X var X class
  localIncrements = unlist(lapply(c(1:(nClasses-1),0),function(i) {
    localIncrements[(1:length(localIncrements))%%(nClasses)==i]
  }))
  
  
  #writing out list
  imp = as.matrix(rf.fit$importance)[,1]
  out = list(X=as.data.frame(X), #cast as data.frame
             Y=rf.fit$y,
             importance = imp,
             imp_ind = sort(imp,decreasing=TRUE,index.return=TRUE)$ix,
             FCarray = array(localIncrements,dim=c(obs,vars,nClasses)),
             sumOfInbags = apply(rf.fit$inbag,1,sum)
             #  all = mget(ls()) #export everything in list
  )

  class(out) = "forestFloor_multiClass"
  return(out)
}



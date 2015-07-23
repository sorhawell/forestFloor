#test multiClass.cpp
forestFloor_randomForest_multiClass <- function(rf.fit,
                                     X,
                                     calc_np = FALSE,
                                     binary_reg = FALSE,
                                     ...) { 

  #check the rf.fitbject have a inbag
  if(is.null(rf.fit$inbag)) stop("input randomForest-object have no inbag, set keep.inbag=T,
                              try, randomForest(X,Y,keep.inbag=T) for without replacement
                              and, cinbag(X,Y,keep.inbag=T,keep.forest=T) with replacement
                              ..cinbag is from trimTrees package...
                              error condition: if(is.null(rf.fit$inbag))")
  
  #make node status a integer matrix
  ns = rf.fit$forest$nodestatus
  storage.mode(ns) = "integer"
  
  
  #translate binary classification RF-object, to regression mode
  if(rf.fit$type=="classification") {
    #     if(length(levels(rf.fit$y))!=2) stop("no multiclass, must be binary classification.
    #                                       error condition: if(length(levels(rf.fit$y))!=2")
    
    rf.fit$forest$leftDaughter  = rf.fit$forest$treemap[,1,] #translate daughter representation to regression mode
    rf.fit$forest$rightDaughter = rf.fit$forest$treemap[,2,] 
    ns[ns==1] = -3  ##translate nodestatus representation to regression mode
    
    if(!is.null(rf.fit$inbagCount)) {
      inbag = rf.fit$inbagCount
    } else {
      if(!is.null(rf.fit$inbag)) {
        inbag = rf.fit$inbag
      } else {
        stop("error rf.fit$inbag or rf.fit$inbagCount is missing, 
             retrain forest with keep.inbag=TRUE")
      }
      }
    } else {
      stop("this function should only be run for classification")
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
  imp = as.matrix(rf.fit$importance)[,1]
  out = list(X=X,Y=rf.fit$y,
             importance = imp,
             imp_ind = sort(imp,decreasing=TRUE,index.return=TRUE)$ix,
             FCarray = array(localIncrements,dim=c(obs,vars,nClasses)),
             sumOfInbags = apply(rf$inbag,1,sum)
             #  all = mget(ls()) #export everything in list
  )

  class(out) = "forestFloor_multiClass"
  return(out)
}


print.forestFloor_multiClass = function(x,...) {
  cat("this is a forestFloor_multiClass object \n
      this object can be plotted in 2D with plot(x), see help(plot.forestFloor) \n
      this object can be plotted in 3D with show3d(x), see help(show3d) \n
      \n
      x contains following internal elements: \n ",with(x,ls()))
}

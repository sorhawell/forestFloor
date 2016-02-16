##method to compute forestFloor_regression
forestFloor_randomForest_regression <- function(rf.fit,
                                                X,
                                                Xtest=NULL,
                                                calc_np = FALSE,
                                                binary_reg = FALSE,
                                                bootstrapFC = FALSE
                                                ) { 
  
  #args_List = list(...)#place extra arguments here
  
  #check the rf.fitbject have a inbag
  if(is.null(rf.fit$inbag)) stop("input randomForest-object have no inbag, set keep.inbag=T,
                                 try, randomForest(X,Y,keep.inbag=T) for regression where Y is numeric
                                 and, cinbag(X,Y,keep.inbag=T,keep.forest=T) for binary-class where Y is factor
                                 ..cinbag is from trimTrees package...
                                 error condition: if(is.null(rf.fit$inbag))")
  
  #merge X and Xtest if Xtest is provided
  if(!is.null(Xtest)) {
    isTrain       = c(rep(T,dim(X)[1]),rep(F,dim(Xtest)[1])) #mark OOB-CV FC and ext. test FC
    merged.list = Xtestmerger(X,Xtest,rf.fit$inbag,rf.fit$y) #test for compatability and merge
    X             = merged.list$bigX     #rbind X and Xtest (specific factor merging)
    rf.fit$inbag  = merged.list$bigInbag #correct inbag matrix
    rf.fit$y      = merged.list$bigy     #fill in dummy target values, not used as test is always OOB
    rf.fit$oob.times  = c(rf.fit$oob.times,rep(rf.fit$ntree,sum(!merged.list$isTrain)))
    
  } else {
    isTrain       = rep(T,dim(X)[1])
  }
  
  
  #make node status as integer matrix
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
    if(!calc_np) {
      #print("node predictions must be re-calculated for random forest of type classification, set calc_np=T)
      #error conditions: if(!calc_np && rf.fit$type='classification')")
      print(" ")
      print("setting calc_np=TRUE")
      calc_np=TRUE
    }
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
  if(is.null(rf.fit$forest)) {
    stop("rf.fit$forest is null, try set keep.forest=TRUE during training")
  }
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
  
  if(bootstrapFC) {
    #Compute FC for random bootstrapping or stratification over
    #local increments from training set to root nodes, by bootstrap/stratificaiton
    #compute LIs with inbag samples
    
    #manual root mean calculation
    #rootSum = apply(rf.fit$inbag*Y,2,sum) #vector, Y mean in each tree
    #rootMean = rootSum / apply(inbagCounts,2,sum) # vector root predictions
    #... or just fetch from rf object
    rootMean = rf.fit$forest$nodepred[1,]
    grandMean = mean(Y[isTrain]) #training set target mean, not including test
    bootStrapLIs = rootMean - grandMean #vector, one LI for each tree
    
    #sum LIs over OOB samples
    OOB.indices = as.matrix(rf.fit$inbag == 0)
    OOB.indices[!OOB.indices] = NA #ignore samples being inbag
    OOB.bootStrapLIs = t(t(OOB.indices) * bootStrapLIs) #collect LI for each sample when OOB
    bootstrapFC.col = apply(OOB.bootStrapLIs,1,mean,na.rm=TRUE) #sum LI into FCs
    localIncrements = cbind(localIncrements,bootstrapFC=bootstrapFC.col) #bind bootstrap col
  }
  
  #writing out list
  imp = as.matrix(rf.fit$importance)[,1]
  out = list(X=as.data.frame(X), #cast as data.frame
             Y=Y,
             importance = imp,
             imp_ind = sort(imp,decreasing=TRUE,index.return=TRUE)$ix,
             FCmatrix = localIncrements,
             isTrain = isTrain
  )
  class(out) = "forestFloor_regression"
  return(out)
  }

#test multiClass.cpp
forestFloor_randomForest_multiClass <- function(rf.fit,
                                     X,
                                     Xtest=NULL,
                                     calc_np = TRUE,
                                     binary_reg = FALSE,
                                     bootstrapFC = FALSE,
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
  
  
  #merge X and Xtest if Xtest is provided
  if(!is.null(Xtest)) {
    isTrain       = c(rep(T,dim(X)[1]),rep(F,dim(Xtest)[1])) #mark OOB-CV FC and ext. test FC
    merged.list   = Xtestmerger(X,Xtest,rf.fit$inbag,rf.fit$y) #test for compatability and merge
    X             = merged.list$bigX     #rbind X and Xtest (specific factor merging)
    rf.fit$inbag  = merged.list$bigInbag #correct inbag matrix
    rf.fit$y      = merged.list$bigy     #fill in dummy target values, not used as test is always OOB
    rf.fit$oob.times  = c(rf.fit$oob.times,rep(rf.fit$ntree,sum(!merged.list$isTrain)))
    
  } else {
    isTrain       = rep(T,dim(X)[1])
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
  
  #outout variable - double vector, structured as cube array (1)nclasses, (2)obs, (3)vars
  localIncrements = rep(0.0,nClasses * obs * vars)
  storage.mode(localIncrements) = "double"
  
  
  # C++ function, recursively finding increments of all nodes of all trees
  # where OOB samples are present. vars, obs and ntree is "passed by number"
  # Anything else is passed by reference. Found increments are imediately
  # summed to localIncrements matrix.
  forestFloor:::multiTree(
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
    localIncrements = localIncrements
    #local increments are summed directly to double vector localIncrements within multiTree
  )
  #returning from multiTree. Vector, localIncrements, now contain the feature contributions.
  #Vector, localIncrements are structured as (1)classes-(2)obs-(3)var
  
  if(bootstrapFC) {
    #local increments from training set to root nodes, by bootstrap/stratificaiton
    #compute LIs with inbag samples
    
    Yt = Yd[isTrain]
    
    #function to compute class rates of nClasses
    getRate = function(Yt,nClasses) {
      count = sapply((1:nClasses)-1,function(classInd) sum(Yt==classInd))
      rate = count / length(Yt) #vector of nClasses
    }
    
    #compute rates for trainining and each rootNode
    base_rate = getRate(Yt,nClasses) #vector of nClasses length
    
    #for each tree in a list compute rootNode_rates (vector with class ratios in root node) 
    rootNode_rates = lapply(1:dim(rf.fit$inbag)[2],function(iTree) {
      IB_ind = rf.fit$inbag[,iTree]!=0 #get indices of obs used
      thisIB = Yt[IB_ind] #place obs in vector
      thisIBcount = rf.fit$inbag[IB_ind,iTree] #get inbagCount for each in inbag
      thisClassCount = sapply((1:nClasses)-1,function(iClass) { #for each class
        sum((thisIB==iClass)*thisIBcount)#count obs equal to iClass, multiply with inbagCount
      }) / sum(thisIBcount) #divide by total obs in node
    }) #list of vectors of nClasses length
    #compute bootstrap local increments rootnode_rate minus base_rate 
    bootStrapLIs = lapply(rootNode_rates,'-',base_rate)
    
    #compute FC for both X and Xtest (Yd length)
    bootstrapFC_list = lapply(1:length(Yd), #indices of 1 to ntree
      function(iObs) {
        OOB.ind = rf.fit$inbag[iObs,]==0
        iObs_OOB_LIs = bootStrapLIs[OOB.ind]#pick LIs where iObs was OOB
        iObs_rates_trees = do.call(rbind,iObs_OOB_LIs) #matrix, nClasses*n times OOB
        iOBS_FC = apply(iObs_rates_trees,2,mean)
    })
    bootstrapFC_matrix = do.call(rbind,bootstrapFC_list)
    
    #restructure localIncrements as cube array (1)obs-(2)vars-(3)classes
    #for each obs*vars slice: add column with bootstrapFC
    localIncrements = unlist(lapply(1:nClasses,function(i) {
      m = localIncrements[(1:length(localIncrements))%%(nClasses)==(i%%nClasses)]
      m = matrix(m,nrow=length(Yd))
      cbind(m,bootstrapFC_matrix[,i]) #extend for each class_matrix with FCbootstrap
    }))
    #set as cube array
    localIncrements = array(localIncrements,dim=c(obs,vars+1,nClasses))
  
  } else { #do not include bootStrapFC
  
    #just restructure localIncrements as cube array (1)obs-(2)vars-(3)classes
    localIncrements = unlist(lapply(c(1:(nClasses-1),0),function(i) {
      localIncrements[(1:length(localIncrements))%%(nClasses)==i]
    }))
    localIncrements = array(localIncrements,dim=c(obs,vars,nClasses))
  }
  
  #writing out list
  imp = as.matrix(rf.fit$importance)[,1]
  out = list(X=as.data.frame(X), #cast as data.frame
             Y=rf.fit$y,
             importance = imp,
             imp_ind = sort(imp,decreasing=TRUE,index.return=TRUE)$ix,
             FCarray = localIncrements,
             sumOfInbags = apply(rf.fit$inbag,1,sum),
             isTrain = isTrain
             #  all = mget(ls()) #export everything in list
  )

  class(out) = "forestFloor_multiClass"
  return(out)
}



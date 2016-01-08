
#write fast adaptor to to change a ranger model into randomForest model
ranger_RFadaptor = function(ra,y=NULL) {
  inbag = do.call(cbind,ra$inbag.counts)
  
  #compute treemap/nnodes
  ntrees = length(ra$forest$child.nodeIDs)
  maxnodes = max(sapply(ra$forest$child.nodeIDs,length))
  
  treemaps = lapply(ra$forest$child.nodeIDs,function(tree) {
    lt =length(tree)
    #bind together all left/rigth daughters in matrix with 2 col and maxnodes rows
    this.treemap = do.call(rbind,c(
      lapply(tree,function(x) c(x+1,0,0)[1:2]), #write -1,-1 if terminal, 0->1 indice
      #extend if less than maxnodes, else escape with c arg recursive=FALSE
      if(lt<maxnodes) replicate(maxnodes-lt,c(0,0),simplify = FALSE) else recursive=FALSE)
    )
  })
  #redefine treemap as cube
  treemap = array(unlist(treemaps),dim=c(maxnodes,2,ntrees))
  
  
  #bestvar and nodestatus computed together
  #cbind list of vectors containing split.varIDs(aka. bestvar)
  bestvar = do.call(cbind,lapply(ra$forest$split.varIDs,function(x) {
    #increase length of vector to maxnodes length to make every column if equal length, fillin -2  
    if(length(x)==maxnodes) x else c(x,rep(-2,maxnodes-length(x)))
  }))
  
  #nodestatus can be reconstructed from bestvar
  nodestatus = bestvar #copy matrix of bestvar
  nodestatus[           ] =  1 #write every as intermediate
  nodestatus[bestvar== 0] = -1 #write any node with no bestvar(0) from ranger as terminal
  nodestatus[bestvar==-2] =  0 #write any fillin number(-2) from conversion  as zero 
  
  #bestvar fillin -2 is corrected to zero
  bestvar[bestvar==-2] = 0 #convert bestvar
  
  xbestsplit = do.call(cbind,lapply(ra$forest$split.values,function(x) {
    #increase length of vector to maxnodes length to make every column if equal length, fillin -2  
    if(length(x)==maxnodes) x else c(x,rep(0,maxnodes-length(x)))
  }))
  
  
  dim(xbestsplit)
  rf=list(inbag = do.call(cbind,ra$inbag.counts),
          type  = tolower(ra$forest$treetype),
          classes = ra$forest$levels,
          y = y,
          ntree=ra$num.trees,
          importance = replicate(2,ra$variable.importance))
  rf$oob.times =         unname(apply(rf$inbag,1,function(x) sum(x==0)))
  rf$forest=list(bestvar    = bestvar,
                 nodestatus = nodestatus,
                 treemap    = treemap,
                 nrnodes    = maxnodes,
                 xbestsplit = xbestsplit,
                 xlevels    = rep(0,length(ra$forest$independent.variable.names)),
                 nclass     = length(ra$forest$class.values),#not used
                 nodepred   = matrix(0,dim(nodestatus))
  )
  names(rf$forest$xlevels) = ra$forest$independent.variable.names
  class(rf)=c("randomForest_ranger","randomForest")
  
  #convert to regression style
  if(rf$type=="regression") {
    rf$forest$leftDaughter = treemap[,1,]
    rf$forest$rightDaughter = treemap[,2,]
    rf$forest$treemap=NULL
    rf$forest$nclass=NULL
    rf$forest$nodestatus[rf$forest$nodestatus==1]=-3
    rf$forest$ndbigtree = apply(rf$inbag,2,function(x) sum(x!=0))
  }
  
  return(rf)
}
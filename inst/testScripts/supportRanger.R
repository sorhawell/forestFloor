# install.packages("ranger")
# install.packages("Rborist")
# install.packages("randomForest")
# install.packages("devtools")
#library(devtools)
#install_github("sorhawell/forestFloor")
rm(list=ls())


#define function to translate ranger to pseudo randomForest
ranger_RFadaptor = function(ra,y=NULL) {
  inbag = do.call(cbind,ra$inbag.counts)
  
  #compute treemap/nnodes
  ntrees = length(ra$forest$child.nodeIDs)
  maxnodes = max(sapply(ra$forest$child.nodeIDs, function(x) length(x[[2]])))+1
  
  treemaps = lapply(ra$forest$child.nodeIDs,function(tree) {
    lt =length(tree[[1]])
    #bind together all left/rigth daughters in matrix with 2 col and maxnodes rows
    this.treemap = do.call(rbind,
      lapply(tree,function(x) {
        inds = x!=0
        x[inds] = x[inds] + 1
        c(x,rep(0,maxnodes-lt))
      })
    ) #write -1,-1 if terminal, 0->1 indice
  })
  
  #redefine treemap as cube
  treemap = array(unlist(treemaps),dim=c(2,maxnodes,ntrees))
  
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
  rf=list(
    inbag = do.call(cbind,ra$inbag.counts),
    type  = tolower(ra$forest$treetype),
    classes = ra$forest$levels,
    y = y,
    ntree=ra$num.trees,
    importance = replicate(2,ra$variable.importance),
    predicted = ra$predictions       
  )
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
    rf$forest$leftDaughter = treemap[1,,]
    rf$forest$rightDaughter = treemap[2,,]
    rf$forest$treemap=NULL
    rf$forest$nclass=NULL
    rf$forest$nodestatus[rf$forest$nodestatus==1]=-3
    rf$forest$ndbigtree = apply(rf$inbag,2,function(x) sum(x!=0))
  }
  
  return(rf)
}


library(ranger)
library(randomForest)
library(forestFloor)

#test if stuff works
set.seed(1)
X = data.frame(replicate(6,rnorm(500)))
y = with(X,X1+X2^2+X3*X4+sin(X5*3))
Data = data.frame(y,X)
ra = ranger(y~.,Data,keep.inbag = TRUE,num.trees = 500,importance = "permutation") #some run time error ocours at more trees
ra
rfra = ranger_RFadaptor(ra,y)
ffra = forestFloor(rfra,X,calc_np = TRUE)
plot(ffra)


rf = randomForest(X,y,keep.inbag = T,importance=T)
ffrf = forestFloor(rf,X,calc_np = TRUE)
plot(ffrf)

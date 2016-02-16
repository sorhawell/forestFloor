Xtestmerger = function(X,test,inbag=NULL,y=NULL) {
  #cast as data.frame
  if(class(X)!="data.frame") X = as.data.frame(X)
  if(class(test)!="data.frame") test = as.data.frame(test)
  
  #check class match
  Xclass = sapply(X,class)
  testClass = sapply(test,class)
  if(length(Xclass)!=length(testClass)) stop("X and Xtest has not same number of columns") 
  classCheck = Xclass==testClass
  if(any(!classCheck)) stop(paste("column(s)",which(!classCheck),
    "in test and X has class mismatch. Must be numeric-numeric,factor-factor"))
  
  #check names match
  namesCheck = names(X)==names(test)
  if(any(!namesCheck)) stop(paste("column(s)",which(!namesCheck),
    "in Xtest and X has names mismatch. Col names must match"))
  
  #check levels in all factors match
  factor.ind = which(sapply(X,is.factor)) #also test factors
  unmatchedTestLevels = mapply(
    #return unmatched levels of test in X
    FUN = function(xl,tl) tl[which(is.na(match(tl,xl)))], #test levels not in x
    #for factor column, get all used levels
    lapply(lapply(   X[factor.ind],droplevels),levels),#xl
    lapply(lapply(test[factor.ind],droplevels),levels),#tl
    SIMPLIFY = FALSE, USE.NAMES = TRUE
  )
  unmatchedLevelsCount = sapply(unmatchedTestLevels,length)
  unmatch.ind = which(unmatchedLevelsCount>0)
  if(length(unmatch.ind)>0) stop(paste(
     "unmatched levels, please check following levels[] of following factors$: \n",
     paste(capture.output(print(unmatchedTestLevels[unmatch.ind])),collapse=" "),
     "\n Info: any level in test must match X,",
     "as a forest cannot score a new level after training"
  ))
  
  #set test levels as X levels
  test[factor.ind] = mapply(
    FUN = function(tf,Xf) factor(tf,levels(Xf)), 
    test[factor.ind], #test factors
    X[factor.ind],    #X factors
    SIMPLIFY = FALSE
  )  
  row.names(test) = paste0("test.",1:dim(test)[1])

  #merge and return in list
  return(list(
  bigX     = rbind(X,test),
  bigInbag = if(!is.null(inbag)) rbind(inbag,matrix(0,nrow=dim(test)[1],ncol=dim(inbag)[2])),
  bigy     = if(!is.null(y)) {
    bigy=y
    bigy[length(y)+(1:dim(test)[1])] = y[1]
    bigy
  },
  isTrain  = c(rep(T,dim(X)[1]),rep(F,dim(test)[1]))
  ))
}
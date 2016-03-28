checkPlotTest = function(plotTest,isTrain) {
  if(is.null(plotTest)) { #if plotTest is unspecified/NULL
    #if all obseravtions of isTrain are TRUE plot train else plot test observations
    plotThese = if(all(isTrain)) isTrain else !isTrain
  } else {
    #match with options
    matchArg = pmatch(plotTest,c(T,F,"andTrain")) #possible arguments are TRUE FALSE and "andTrain"
    if(length(matchArg)<1 || is.na(matchArg)) {
      stop(paste("plotTest= '",plotTest,"' is an incorrect argument"))
    }
    
    #if plotTest is T, plot test set if there is one, else raise error
    if(matchArg==1) plotThese = if(!all(isTrain)) !isTrain else {
      stop("no test set found, did you pass a Xtest when computing forestFloor?")
    }
    
    #if not to plot test set, then plot train, train should also be there
    if(matchArg==2) plotThese = isTrain
    
    #if plotTest is "andTrain" blot both train and test set
    if(matchArg==3) {
      plotThese = rep(T,length(isTrain))
      if(all(isTrain)) warning("no test found, only plotting train")
    }
  }
return(plotThese)
}
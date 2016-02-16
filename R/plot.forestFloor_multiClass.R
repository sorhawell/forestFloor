
#2dplot of forstFloor_multiClass
plot.forestFloor_multiClass  = function(
  x,
  plot_seq=NULL,
  label.seq=NULL,
  plotTest = NULL,
  limitY=TRUE,
  colLists = NULL,
  orderByImportance=TRUE,
  fig.columns = NULL,
  plot_GOF=TRUE,
  GOF_args = list(),
  speedup_GOF = TRUE,
  jitter_these_cols = NULL,
  jitter.factor = NULL,
  ...) {
 
  #pre-checking graphial parameters, and split into separate lists
  moreArgs = list(...) #args passed to par() if no match, passed instead to plot(,...) 
  pars = par(no.readonly = TRUE) #save previous graphical parameters
  toPlotOnly=c("cex","col") #these args always passed to plot(,...)
  #TRUE/FALSE vector, passed to par() (TRUE) or plot() FALSE
  parArgs.ind = mapply("&&", 
                       names(moreArgs) %in% names(pars),#is a par-arg  match
                       !names(moreArgs) %in% toPlotOnly  #not par-excluded
  )
  if(length(parArgs.ind)==0) {
    userArgs.plot=list()
    userArgs.par =list()
  } else {
    userArgs.plot = moreArgs[!parArgs.ind]
    userArgs.par  = moreArgs[ parArgs.ind]
  }

  #label.seq must be resolved before cropping training levels
  if(is.null(label.seq)) label.seq = 1:min(8,length(levels(x$Y)))
  #if not all test/train is to be plotted remove train or test
  plotThese = checkPlotTest(plotTest,x$isTrain)
  if(!(all(plotThese))) {
    x = with(x, {
      #cut to those which should be plotted
      FCarray = FCarray[plotThese,,]
      Y = Y[plotThese]
      X = X[plotThese,]
      mget(ls())
    })
  }
  
  void = with(x,{ #set scope inside x object
    
    if(is.null(plot_seq)) plot_seq = 1:min(18,dim(X)[2])
    fig.columns = if(is.null(fig.columns)) {
      if(length(plot_seq)<=4) c(1,2,3,2)[length(plot_seq)] else 3
    }
    if(orderByImportance) plot_seq = x$imp_ind[plot_seq]
    if(is.null(colLists)) colLists =
      lapply(1:length(label.seq), function(i) factor(rep(i,dim(X)[1])))
    
    #set graphical pars
    n.plots = min(dim(X)[2],length(plot_seq))
    plotdims.y = min(ceiling(n.plots/fig.columns),5)
    plotdims.x = min(fig.columns , n.plots)
    args.par = append.overwrite.alists(
      masterArgs = userArgs.par, #user args for par is master
       slaveArgs = list(mfrow=c(plotdims.y,plotdims.x),mar = c(2,2,3,1)) #std args
    )
    par(args.par)
    if(plot_GOF) {
      if(speedup_GOF) { #reduce observatoins to compute forest GOF faster
        obs = length(Y)
        reduce.exp = 1 - 0.025  * sum(obs>c(500,700,1000,1500,2500,4500,10000,20000))
        x = with(x,{ #this is a little tricky, global object is overwritten with local
                     #reduced version if speedup true. Not reduced content of x still
                     #available locally as with(x, have been called once before
          subSample=sample(obs,ceiling(obs^reduce.exp))
          X=X[subSample,]
          Y=Y[subSample]
          FCarray=FCarray[subSample,,]
          mget(ls())
        })
      }
      
      fits = lapply(label.seq,function(label.ind) {
        ff2 = convolute_ff(list(FCmatrix = x$FCarray[,,label.ind],
                                X=apply(x$X[,],2,as.numeric.factor),
                                Xind = apply(x$X,2,function(xcol) { 
                                  sort(xcol,index.return=TRUE)$ix})),
                           these.vars=sort(plot_seq))
      })
      
      fit.seq = (1:length(label.seq)) %in% 1:length(Y)
    }  else fits = invisible()
    
    
    #define standard graphical arguments as non-evaluated alist
    stdArgs.plot = alist(
      x    = X.jittered,
      y    = FCarray[,j,i],
      col  = colLists[[i]],
      ylim = range(FCarray),
      ylab = " ",
      xlab = paste0("X[ ,",j,"]"),
      
      #if fit is plotted goodness of fit is added to title
      main = paste(names(X)[j],if(plot_GOF) {
        paste("R^2=",
              #compute fit of each class and calculate average R^2
          round(mean(sapply(label.seq,function(lSeq) {
            cor( fits[[lSeq]]$FCfit[,j],x$FCarray[,j,lSeq])^2 #i,j refer to later loops
          })),digits=2)
        )
      } else "")
    )
    
    #define std GOF graphical arguments
    stdArgs.GOF = alist(
      x   = X[Xind[,j],j],
      y   = FCfit[Xind[,j],j],
      col = colLists[[i]],
      cex = 0.05,
      type="l",
      lwd=5
    )
    #iterate all plot windows and all classes
    for(j in plot_seq) {
      for(i in label.seq) {
        #plot samples by each feature by each class
        X.jittered = if(j %in% jitter_these_cols) jitter(X[,j],jitter.factor) else X[,j]
        allArgs.plot = append.overwrite.alists(userArgs.plot,stdArgs.plot)
        if(i==1) do.call(plot,allArgs.plot) else do.call(points,allArgs.plot)
        #plot fitted lines by each class
        if(plot_GOF) with(fits[[i]],do.call(points,
          append.overwrite.alists(GOF_args,stdArgs.GOF)))  
      }
    }
    par(pars)
    return(fits)
  })
}

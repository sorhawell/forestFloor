
#2dplot of forstFloor_multiClass
plot.forestFloor_multiClass  = function(
  x,
  plot_seq=NULL,
  label.seq=NULL,
  plotTest = NULL,
  limitY=TRUE,
  col = NULL,
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
  if(is.null(label.seq)) {
    if(length(levels(x$Y))>8)
      message("FYI, only plotting first 8 classes to avoid overplotting.
Use label.seq to select more or other classes")
    label.seq = 1:min(8,length(levels(x$Y)))
    
  }
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
  
  #if not plot_seq not chosen, pick up to 9 first plot windows
  if(is.null(plot_seq)) plot_seq = 1:min(9,dim(x$X)[2])
  
  #if fig_columns not chosen, set to 1,2 or depeding on number of plots
  fig.columns = if(is.null(fig.columns)) {
    if(length(plot_seq)<=4) c(1,2,3,2)[length(plot_seq)] else 3
  }
  
  #if plot_seq refer to importance ordered features, reorder plot_seq accordingly
  imp_ind = if(orderByImportance) x$imp_ind else 1:ncol(x$X)
  
  #interface colLists through col
  if(!is.null(col)) {
    if(class(col) == "list") {
      colLists = col #already a list of colour vectors
    } else {
      #only one colour, copy two all colour vectors
      if(length(col)==1) colLists = as.list(rep(col,length(label.seq))) else {
        #    
        colLists = as.list(col[1:length(label.seq)])
      }
    }
  }
  
  #if colLists (list of colour vectors) not provided, make one
  #each class label will get one colour
  if(is.null(colLists)) colLists =
    lapply(1:length(label.seq), function(i) factor(rep(i,dim(x$X)[1])))
  
  #set graphical pars
  n.plots = min(dim(x$X)[2],length(plot_seq))
  plotdims.y = min(ceiling(n.plots/fig.columns),5)
  plotdims.x = min(fig.columns , n.plots)
  args.par = append.overwrite.alists(
    masterArgs = userArgs.par, #user args for par is master
    slaveArgs = list(mfrow=c(plotdims.y,plotdims.x),mar = c(2,2,3,1)) #std args
  )
  par(args.par)
  if(plot_GOF) {
    if(speedup_GOF) { #reduce observatoins to compute forest GOF faster
      obs = length(x$Y)
      reduce.exp = 1 - 0.025  * sum(obs>c(500,700,1000,1500,2500,4500,10000,20000))
      x_small = with(x,{ #this is a little tricky, global object is overwritten with local
        #reduced version if speedup true. Not reduced content of x still
        #available locally as with(x, have been called once before
        subSample=sample(obs,ceiling(obs^reduce.exp))
        X=X[subSample,]
        Y=Y[subSample]
        FCarray=FCarray[subSample,,]
        subSample = subSample
        colLists = lapply(colLists,function(colVec) {
          #only subsample of colourVector has one colour per sample
          if(length(colVec) == length(Y)) colVec[subSample] else colVec
        })
        mget(ls())
      })
    } else x_small = x #no reduction of samples
    
    #sort.index for X, needed for drawing lines
    X.sort.ind = apply(x_small$X,2,function(xcol) { 
      sort(xcol,index.return=TRUE)$ix})
    
    #for each class probability
    fits = lapply(label.seq,function(label.ind) {
      #make a regression like forestFloor object, target is prob of this class
      ff_temp = list(
        FCmatrix = x_small$FCarray[,,label.ind],
        X=apply(x_small$X[,],2,as.numeric.factor)
      )
      #compute fit for 'these vars' for this object
      ff_temp = convolute_ff(ff_temp,these.vars=imp_ind[plot_seq]) #features to fit lines
      ff_temp[c("FCmatrix","X")] = NULL #only need the fit
      
      return(ff_temp)
    })
  }  else fits = NULL #the plot function will return fits, NULL if not computed
  
  
  #define standard graphical arguments as non-evaluated alist
  #j,i refer to later loops (j=is this plot feature, i this class)
  stdArgs.plot = alist(
    x    = X.jittered,
    y    = x$FCarray[,imp_ind[j],i],
    col  = as.character(colLists[[i]]),
    ylim = range(x$FCarray),
    ylab = " ",
    xlab = paste0("X[ ,",imp_ind[j],"]"),
    
    #if fit is plotted goodness of fit is added to title
    main = paste(names(x$X)[imp_ind[j]],if(plot_GOF) {
      paste("R^2=",
            #compute fit of each class and calculate average R^2
            round(mean(sapply(label.seq,function(lSeq) {
              cor( fits[[lSeq]]$FCfit[,       j], #fits all ready indexed by imp_ind
                   x_small$FCarray[,imp_ind[j],lSeq])^2 
            })),digits=2)
      )
    } else "")
  )
  
  #define std GOF graphical arguments
  stdArgs.GOF = alist(
    x   =       x_small$X[x_small_sorted_ind, imp_ind[j]],
    y   = fits[[i]]$FCfit[x_small_sorted_ind,          j], #FC fit already sorted by importance
    col = as.character(x_small$colLists[[i]]), #use subsmapled colour vector
    cex = 0.05,
    type="l",
    lwd=1
  )
  
  
  #iterate all plot windows/features
  for(j in plot_seq) {
    #and all chosen class labels, 
    for(i in label.seq) {
      
      #jitter X.axis if requested (typical useful for factor)
      X.jittered = if(j %in% jitter_these_cols)
        jitter(x$X[,imp_ind[j]],jitter.factor) else x$X[,imp_ind[j]]
      
      #plot points
      allArgs.plot = append.overwrite.alists(userArgs.plot,stdArgs.plot)
      if(i==1) do.call(plot,allArgs.plot) else do.call(points,allArgs.plot)
      
      #plot fitted lines by each class label
      if(plot_GOF) {
        x_small_sorted_ind = X.sort.ind[,imp_ind[j]] #short handle
        do.call(points,append.overwrite.alists(GOF_args,stdArgs.GOF))   
      }  
      
    }
  }
  par(pars)
  return(invisible(fits))
}


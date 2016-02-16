#m2 plot output
plot.forestFloor_regression = function(x,
                                       plot_seq=NULL, 
                                       limitY=TRUE,
                                       orderByImportance=TRUE, 
                                       cropXaxes=NULL, 
                                       crop_limit=4,
                                       plot_GOF=TRUE,
                                       GOF_args = list(col="#33333399"),
                                       speedup_GOF = TRUE,
                                       ...){
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
  
  #crop x(forestFloor) object to only visualize test or train
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
  
  
  #short for features and feature contribution in object
  X = x$X
  FCs = x$FCmatrix
  if(plot_GOF && is.null(x$FCfit)) { 
    #compute goodness of fit of one way presentation
    #leave-one-out k-nearest neighbor(guassian kernel), kknn package
    print("compute goodness-of-fit with leave-one-out k-nearest neighbor(guassian weighting), kknn package")
    if(speedup_GOF) { #reduce observatioins to compute forest GOF
      obs = length(x$Y)
      reduce.exp = 1 - 0.025  * sum(obs>c(500,700,1000,1500,2500,4500,10000,20000))
      x = with(x,{
        
        subSample=sample(obs,ceiling(obs^reduce.exp))
        X=X[subSample,]
        Y=Y[subSample]
        FCmatrix=FCmatrix[subSample,]
        return(mget(ls()))
      })
    }
      
    if(is.null(x$FCfit)) x = convolute_ff(x) 
    #retrieve fitted values and compare to actual feature contributions for every variable
  }
  if(plot_GOF) GOFs = sapply(1:dim(X)[2],function(j) cor(x$FCfit[,j],x$FCmatrix[,j])^2)
  
  
  #obsolete
  #   #Auto setting transparancy variable. The more obs, the more transparrency
  #   if(alpha=="auto") alpha = min(max(400/dim(X)[1],0.2),1)
  #   
  #If no sequnce, choosing to plot first 18 variables
  if(is.null(plot_seq)) plot_seq = 1:min(dim(X)[2],24)
  
  #make catogorical features numeric, save jitter.template
  jitter.template=rep(FALSE,dim(X)[2]) #list of what features are catagorical
  as.numeric.factor <- function(x,rearrange=TRUE) {
    if(is.numeric(x)) return(x)
    if(rearrange) x = match(x,levels(droplevels(x))) else x = match(x,levels(x))
    return(x)
  }
  for(i in 1:dim(X)[2]) {
    if(is.factor(X[,i])) {
      jitter.template[i]=TRUE
      this.fac=as.numeric.factor(X[,i])
      X[,i] = this.fac
    }
    if(is.character(X[,i])) X[,i] = as.numeric(X[,i])
  } 
  
  ##get dimensions of plots and set graphical (par)ameters
  n.plots = min(dim(X)[2],length(plot_seq))
  plotdims.y = min(ceiling(n.plots/3),5)
  plotdims.x = min(3 , n.plots)
  if(n.plots==4) {
    plotdims.x=2
    plotdims.y=2
  }
  
  stdArgs.par = list(
    mar=c(2.2,2.2,1.2,1.2),
    cex=.5,
    mfrow=c(plotdims.y,plotdims.x))
  
  args.par = append.overwrite.alists(
    userArgs.par, #userArgs for par() [MASTER args]
     stdArgs.par) #std args for par() [SLAVE args]
  par(args.par) #changing par, narrowing plot margins, smaller points
  
  ##get importance for plotting
  imp = x$importance     #fetch importance
  imp.ind = x$imp_ind    #fetch importance ranking/indices
  
  ##plot the n.plots most important variables
  Xsd = 0:1 #initialize Xsd
  if(!orderByImportance) imp.ind=sort(imp.ind) #optinal removal of importance order
  for(i in plot_seq) {
    
    if(i %in% cropXaxes && !is.null(cropXaxes)) {
      limitX = TRUE
      Xsd = box.outliers(as.numeric(X[,imp.ind[i]],2),limit=crop_limit,normalize=F)
    } else {
      limitX = FALSE
    }
    
    #arguments for plooting
    
    stdArgs.plot = alist(
      
      #the XY coordinates to plot
      x = data.frame(
      physical.value = jitter(X[,imp.ind[i]],factor=jitter.template[imp.ind[i]]*2),
      partial.contribution  = FCs[,imp.ind[i]]
      ),
      #the title
      main = if(!plot_GOF) names(imp)[imp.ind[i]] else {
        imp=imp
        imp.ind = imp.ind
        i=i
        theName = names(imp)[imp.ind[i]]
        theNumber = round(GOFs[imp.ind[i]],2)
        paste0(theName,",R^2= ",theNumber)
      },
      
      #plot limits
      ylim = list(NULL,range(FCs))[[limitY+1]],
      xlim = list(NULL,range(Xsd))[[limitX+1]]
    )
    
    #merge args.plot with user args(...), conflicts resolved by ...
    allArgs.plot = append.overwrite.alists(userArgs.plot,stdArgs.plot)
    do.call(plot,allArgs.plot)
      
    if(plot_GOF) {
      X_unique = sort(unique(x$X[,imp.ind[i]])) #refer to x, as may be reduced by speedup_GOF
      X_unique.ind = match(X_unique,x$X[,imp.ind[i]]) #refer to x not X
      FC_match = x$FCfit[X_unique.ind,imp.ind[i]]     #refer to x not X
      allArgs.points = append.overwrite.alists(
        GOF_args,
        alist(x=X_unique,y=FC_match,type="l",lwd=2)
      )
      do.call(points,allArgs.points)
    }
  }
  pars = with(pars,if(exists("pin")) {
    rm(pin)
    return(mget(ls()))
  }) 
  par(pars)
}

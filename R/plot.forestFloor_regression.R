#m2 plot output
plot.forestFloor_regression = function(x,
                                       #colour_by=1,  #remove
                                       #col_axis = 1, #remove
                                       plot_seq=NULL, 
                                       #alpha="auto", #remove
                                       limitY=TRUE,
                                       orderByImportance=TRUE, 
                                       #external.col=NULL, #remove
                                       cropXaxes=NULL, 
                                       crop_limit=4,
                                       plot_GOF=FALSE,
                                       GOF_col = "#33333399",
                                       speedup_GOF = TRUE,
                                       ...){
  
  pars = par(no.readonly = TRUE) #save previous graphical par(emeters)
  par(mar=c(2.2,2.2,1.2,1.2),cex=.5) #changing par, narrowing plot margins, smaller points
  
  #short for phys.val and feature contribution in object
  
  X = x$X
  FCs = x$FCmatrix
  if(plot_GOF && is.null(x$FCfit)) { 
    #compute goodness of fit of one way presentation
    #leave-one-out k-nearest neighbor(guassian kernel), kknn package
    print("compute goodness-of-fit with leave-one-out k-nearest neighbor(guassian kernel), kknn package")
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
  
  ##get dimensions of plots
  n.plots = min(dim(X)[2],length(plot_seq))
  plotdims.y = min(ceiling(n.plots/3),5)
  plotdims.x = min(3 , n.plots)
  par(mfrow=c(plotdims.y,plotdims.x))
  
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
    
    plot(
      data.frame( # data to plot
        physical.value        = jitter(X[,imp.ind[i]],
                                       factor=jitter.template[imp.ind[i]]*2),
        partial.contribution  = FCs[,imp.ind[i]]
      ),
      main = if(!plot_GOF) names(imp)[imp.ind[i]] else {
        imp=imp
        imp.ind = imp.ind
        i=i
        theName = names(imp)[imp.ind[i]]
        theNumber = round(GOFs[imp.ind[i]],2)
        paste0(theName,",R^2= ",theNumber)
      },
      ylim = list(NULL,range(FCs))[[limitY+1]], #same Yaxis if limitY == TRUE
      xlim = list(NULL,range(Xsd))[[limitX+1]],
      ...
    )
    if(plot_GOF) {
      X_unique = sort(unique(x$X[,imp.ind[i]])) #refer to x, as may be reduced by speedup_GOF
      X_unique.ind = match(X_unique,x$X[,imp.ind[i]]) #refer to x not X
      FC_match = x$FCfit[X_unique.ind,imp.ind[i]]     #refer to x not X
      points(X_unique,FC_match,col=GOF_col,type="l",lwd=2)
    }
  }
  pars = with(pars,if(exists("pin")) {
    rm(pin)
    return(mget(ls()))
  }) 
  par(pars)
}

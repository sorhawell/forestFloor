
# Methods:
#m1 print output
print.forestFloor = function(x,...) {
  cat("this is a forestFloor('x') object \n
      this object can be plotted in 2D with plot(x), see help(plot.forestFloor) \n
      this object can be plotted in 3D with show3d(x), see help(show3d) \n
      \n
      x contains following internal elements: \n ",with(x,ls()))
}

#m2 plot output
plot.forestFloor = function(x,
                            #colour_by=1,  #remove
                            #col_axis = 1, #remove
                            plot_seq=NULL, 
                            #alpha="auto", #remove
                            limitY=TRUE,
                            order_by_importance=TRUE, 
                            #external.col=NULL, #remove
                            cropXaxes=NULL, 
                            crop_limit=4,
                            plot_GOF=FALSE,
                            GOF_col = "#33333399",
                            ...)
{
  
  pars = par(no.readonly = TRUE) #save previous graphical par(emeters)
  par(mar=c(2.2,2.2,1.2,1.2),cex=.5) #changing par, narrowing plot margins, smaller points
  
  #short for phys.val and feature contribution in object
  
  X = x$X
  FCs = x$FCmatrix
  if(plot_GOF && is.null(x$FCfit)) { 
    #compute goodness of fit of one way presentation
    #leave-one-out k-nearest neighbor(guassian kernel), kknn package
    print("compute goodness-of-fit with leave-one-out k-nearest neighbor(guassian kernel), kknn package")
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
  if(!order_by_importance) imp.ind=sort(imp.ind) #optinal removal of importance order
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
      X_unique = sort(unique(X[,imp.ind[i]]))
      X_unique.ind = match(X_unique,X[,imp.ind[i]])
      FC_match = x$FCfit[X_unique.ind,imp.ind[i]]
      points(X_unique,FC_match,col=GOF_col,type="l",lwd=2)
    }
  }
  pars = with(pars,if(exists("pin")) {
    rm(pin)
    return(mget(ls()))
  }) 
  par(pars)
}

#f1a 3d show function
show3d_new = function(ff,
                      Xi  = 1:2,
                      FCi = NULL,
                      col = "#12345678",    
                      sortByImportance = TRUE,
                      surface=TRUE,   
                      combineFC = sum,  
                      zoom=1.2,       
                      grid.lines=30,  
                      limit=3, 
                      kknnGrid.args = alist(),  
                      plot.rgl.args = alist(),  
                      surf.rgl.args = alist()   
) {
  if(class(ff)!="forestFloor") stop("ff, must be of class forestFloor")
  if(length(Xi)!=2) {
    warning("Xi should be of length 2, if 1 first elements is used twice, if >2 only two first elements is used")
    if(length(Xi) > 2) Xi=Xi[1:2] else Xi = Xi[c(1,1)]
  }
  if(!all(Xi %in% 1:dim(ff$X)[2]))   stop( "input  Xi points to columns indices out of range of feature matrix ff$X")
  if(is.null(FCi)) FCi=Xi
  if(!all(FCi %in% 1:dim(ff$FCmatrix)[2]) && length(FCi)>0) stop("input FCi points to columns indices out of range of feature matrix ff$X")
  
  #should Xi and FCi refer to coloumns sorted by importance?
  if(sortByImportance) {
    Xi  = ff$imp_ind[ Xi]
    FCi = ff$imp_ind[FCi]
  }
  
  #fetch selected coloums from object
  X = ff$X[,Xi]
  FC = ff$FCmatrix[,FCi]
  
  #define xy coordinates from features and z from feature contributions
  xaxis = X[,1]
  yaxis = X[,2]
  if(length(FCi)==1) zaxis = FC else zaxis = apply(FC,1,combineFC) #if multiple FCis these will summed to one value.
  
  #fixing categorical features
  as.numeric.factor <- function(x,rearrange=TRUE) {
    if(is.numeric(x)) return(x) #numeric variables are left unchanged
    if(rearrange) x = match(x,levels(droplevels(x))) else x = match(x,levels(x))
    return(x)
  }
  xaxis = as.numeric.factor(xaxis)
  yaxis = as.numeric.factor(yaxis)
  zaxis = as.numeric.factor(zaxis)
  
  #plotting points
  #merge current/user, wrapper arguments for plot3d in proritized order
  wrapper_arg = list(x=xaxis, y=yaxis, z=zaxis, col=col,
                     xlab=names(X)[1],ylab=names(X)[2],zlab=paste(names(ff$X[,FCi]),collapse=" - "),
                     alpha=.4,size=3,scale=.7,avoidFreeType = TRUE,add=FALSE)
  calling_arg = append.overwrite.alists(plot.rgl.args,wrapper_arg)
  do.call("plot3d",args=calling_arg)
  
  #plotting surface
  #merge arguments again
  if(surface) {
    #compute grid
    grid = convolute_grid(ff, Xvars=Xi,FCvars=FCi, limit=limit, grid=grid.lines, zoom=zoom,  userArgs.kknn = kknnGrid.args)
    wrapper_arg = alist(x=unique(grid[,2]),y=unique(grid[,3]),z=grid[,1],add=TRUE,alpha=0.2,col=c("grey","black")) #args defined in this wrapper function
    calling_arg = append.overwrite.alists(surf.rgl.args,wrapper_arg)   
    do.call("persp3d",args=calling_arg)
  }
}




#f2 - show vec plot 2D and 3D
vec.plot = function(model,X,i.var,grid.lines=100,VEC.function=mean,zoom=1,limitY=F,col="#20202050") {
  
  #compute grid range
  d = length(i.var)
  scales = lapply(i.var, function(i) {
    rXi = range(X[,i])
    span = abs(rXi[2]-rXi[1])*zoom/2
    center = mean(rXi)
    seq(center-span,center+span,length.out=grid.lines)
  })
  
  #expand grid range to a n-dimensional VEC-space and predict by model
  anchor.points = as.matrix(expand.grid(scales),dimnames=NULL)
  Xgeneralized=apply(X,2,VEC.function)    
  Xtest.vec = data.frame(t(replicate(dim(anchor.points)[1],Xgeneralized)))
  Xtest.vec[,i.var] = anchor.points
  yhat.vec = predict(model,Xtest.vec)
  
  #add observations to VEC-space
  values.to.plot=X[,i.var]
  Xmean=apply(X,2,VEC.function)
  Xtest.obs = data.frame(t(replicate(dim(X)[1],Xgeneralized)))
  Xtest.obs[,i.var] = values.to.plot
  yhat.obs =  predict(model, Xtest.obs)
  
  #plot VEC-space versus predictions (only 2D and 3D plot supported)
  if(d==2) { #if 2D VEC space
    plot3d(x=values.to.plot[,1],y=values.to.plot[,2],z=yhat.obs,
           xlab=names(X)[i.var][1],ylab=names(X)[i.var][2],main="VEC-SURFACE",col=col)
    surface3d(x=scales[[1]],
              y=scales[[2]],
              z=yhat.vec,col="#404080",size=4,alpha=0.4)
  }else{ #otherwise if 1D VEC-space
    if(limitY) {ylim = range(model$y)} else {ylim = NULL}
    plot(x=scales[[1]],y=yhat.vec,col="red",type="l",xlab=names(X)[i.var][1],ylim=ylim)
    points(values.to.plot,yhat.obs)
  }
}



#f3 input a forestFloor object, computes convoluted feature contributions with kknn
#outout a new ff object as input with attached ff$FCfit
#kknn arguments can be accessed directly from this wrapper by userArgs.kknn
#if conflicting with wrappe
convolute_ff = function(ff,
                        these.vars=NULL,
                        k.fun=function() round(sqrt(n.obs)/2),
                        userArgs.kknn = alist(kernel="gaussian")
) {
  
  n.obs=dim(ff$X)[1]
  n.vars=dim(ff$X)[2]
  k=k.fun()
  if(is.null(these.vars)) these.vars = 1:n.vars
  
  
  #merge user and wrapper args
  Data = "I only exist to satisfy R cmd CHECK" #dummy declaration
  defaultArgs.kknn = alist(formula=fc~.,data=Data,kmax=k,kernel="gaussian")
  kknn.args=append.overwrite.alists(userArgs.kknn,defaultArgs.kknn)
  
  #iterate for seleceted variables
  ff$FCfit = sapply(these.vars, function(this.var) {
    #force factors into numeric levels
    Xcontext = ff$X[,this.var]
    if(!is.numeric(Xcontext)) Xcontext = as.numeric(Xcontext)
    #print(Xcontext)
    #construct data.frame of FCs and context X
    Data = data.frame(fc=ff$FCmatrix[,this.var],x=Xcontext)
    #train and predict FC as function of X, LOO crosvalidated
    knn.obj = do.call("train.kknn",kknn.args)$fitted.values
    knn.obj[[length(knn.obj)]] #extract crosvalidated predictions
  })
  ff  
}


#f3 input a forestFloor object, as convolute_ff but do not iterate all variables. Instead wrapper will use
#kknn to convolute a designated featureContribution with designated features - oftenly the corresponding features.
convolute_ff2 = function(ff,
                         Xvars,
                         FCvars = NULL,
                         k.fun=function() round(sqrt(n.obs)/2),
                         userArgs.kknn = alist(kernel="gaussian")
) {
  if(is.null(FCvars)) FCvars = Xvars
  n.obs=dim(ff$X)[1]
  k=k.fun()
  
  #merge user and wrapper args
  defaultArgs.kknn = alist(formula=fc~.,data=Data,kmax=k,kernel="gaussian")
  kknn.args=append.overwrite.alists(userArgs.kknn,defaultArgs.kknn)
  
  #collect coloumns
  if(length(FCvars)>1) {
    fc = apply(ff$FCmatrix[,FCvars],1,sum)
  } else {
    fc = ff$FCmatrix[,FCvars]
  }
  x = ff$X[,Xvars]
  
  #compute topology
  Data = data.frame(fc=fc,x=x)
  knn.obj = do.call("train.kknn",kknn.args)$fitted.values
  out = knn.obj[[length(knn.obj)]]
}

#f4 input a forestFloor object, as convolute_ff but do not iterate all variables. Instead wrapper will use
#kknn to convolute a designated featureContribution with designated features - oftenly the corresponding features.
convolute_grid = function(ff,
                          Xvars,
                          FCvars = NULL,
                          grid = 30,
                          limit = 3,
                          zoom = 3,
                          k.fun=function() round(sqrt(n.obs)/2),
                          userArgs.kknn = alist(kernel="gaussian")
) {
  
  #input defaults
  if(is.null(FCvars)) FCvars = Xvars
  n.obs=dim(ff$X)[1]
  k=k.fun() # will be overided if a "k=" argument is provided in userArgs.kknn-list
  
  #collect coloumns of data
  if(length(FCvars)>1) {
    fc = apply(ff$FCmatrix[,FCvars],1,sum)
  } else {
    fc = ff$FCmatrix[,FCvars]
  }
  X = ff$X[,Xvars]
  
  #make grid, or use external grid if provided
  if(length(grid)==1) {
    X = box.outliers(X,limit=limit,normalize=F)
    get.seq = function(x) {
      upper = (max(x) - mean(x)) * zoom
      lower = (min(x) - mean(x)) * zoom
      seq(lower+mean(x), upper+mean(x),length.out=grid)
    }
    ite.val=lapply(1:dim(X)[2],function(i) get.seq(X[,i])) #for each variable define span of grid
    gridX = as.data.frame(expand.grid(ite.val))
    names(gridX) = names(X)
    
    ##WOUPS NEVERMIND it worked...
    #     #could not make list of vectors work with expand.grid as doc promises
    #     #this hack calls expand.grid with specified arguments
    #     
    #     #hack - make charecter string of appropiate code
    #     run.this = "alist("
    #     for(i in 1:dim(X)[2]) run.this = paste(run.this,names(X)[i]," = ite.val[,",i,"],",sep="")
    #     run.this = substr(run.this,1,nchar(run.this)-1)
    #     run.this = paste(run.this,")")
    #     #hack - parse and evaluate string into a argument list of one argument for each dimension
    #     arg.list = eval(parse(text=run.this))
    #     #call expand.grid with specified arguments
    #     gridX=as.data.frame(do.call(expand.grid,arg.list)) #grid coordinates
    
  } else {
    gridX = grid
  }
  
  #prepare data
  Data = data.frame(fc=fc,x=ff$X[,Xvars])
  gridX = data.frame(x=gridX)
  
  #merge user args and args of this wrapper function, user args have priority
  defaultArgs.kknn = alist(formula=fc~.,train=Data,k=k,kernel="gaussian",test=gridX)
  kknn.args=append.overwrite.alists(userArgs.kknn,defaultArgs.kknn)
  
  #execute kknn function and retrive convolution
  gridFC = do.call("kknn",kknn.args)$fitted.values
  
  #gather results in data frame, with convoluted feature contributinos in grid
  return(data.frame(fc = gridFC,gridX))
}



# #sf5 scale data and grid, to allow knn 
# scale.by = function(scale.this,by.this) {
#   center = attributes(by.this)$'scaled:center'
#   scales = attributes(by.this)$'scaled:scale'
#   nvars = dim(scale.this)[2]
#   sapply(1:nvars, function(i) (scale.this[,i]-center[i])/scales[i])
# }




#sf6  reduce outliers to within limit of 1.5 std.dev and/or output as normalized 
box.outliers = function(x,limit=1.5,normalize=TRUE) {
  
  sx=scale(x)
  if(limit!=FALSE) {
    sx[ sx>limit] =  limit
    sx[-sx>limit] = -limit
  }
  
  if(normalize) { 
    sx.span = max(sx) - min(sx)
    sx = sx - min(sx)
    sx = sx / sx.span
  } else {
    obs=attributes(sx)$"dim"[1]
    if(dim(sx)[2]>1) {
      sx = sx * t(replicate(obs,attributes(sx)$"scaled:scale")) +
        t(replicate(obs,attributes(sx)$"scaled:center"))
    } else {
      sx  = sx * attributes(sx)$"scaled:scale" + attributes(sx)$"scaled:center" 
    }
  }
  
  if(class(x)=="data.frame") {
    sx = as.data.frame(sx,row.names=row.names(x))
    names(sx) = names(x)
  }
  return(sx)
}

# #sf7  - calculate deviance from surface
# surf.error = function() {
#   outs=replicate(knnBag, {  #the following is replicated/performed severeal ~20 times
#     this.boot.ind = sample(dim(XY)[1]*bag.ratio,replace=T) #pick a bootstrap from samples             
#     sXY = scale(XY[this.boot.ind,])  #scale bootstrap to uni-variance
#     sgridXY = scale.by(scale.this=gridXY,by.this=sXY) #let grid be scaled as this bootstrap was scaled to sXY
#     out=knn.reg(train=sXY,
#                 test=sgridXY,
#                 y=axisval$z[this.boot.ind],
#                 k=k,
#                 algorithm="kd_tree")$pred  #predict grid from bootstrap of samples
#   })
# }

# #sf7 estimate surface with kNNbag, depends on "sf5 - scale.by"
# kNN.surf = function(knnBag,
#                     XY,
#                     gridXY,
#                     k,
#                     y,
#                     bag.ratio=.8,
#                     replace=TRUE) {
#   outs=replicate(knnBag, {  #the following is replicated/performed severeal ~20 times
#     this.boot.ind = sample(dim(XY)[1]*bag.ratio,replace=TRUE) #pick a bootstrap from samples             
#     sXY = scale(XY[this.boot.ind,])  #scale bootstrap to uni-variance
#     sgridXY = scale.by(scale.this=gridXY,by.this=sXY) #let grid be scaled as this bootstrap was scaled to sXY
#     out=knn.reg(train=sXY,
#                 test=sgridXY,
#                 y=y[this.boot.ind],
#                 k=k,
#                 algorithm="kd_tree")$pred  #predict grid from bootstrap of samples
#   })
#   out = apply(outs,1,mean) # collect predictions
#   return(out)
# }



#sf8 neat function to help increase adaptability of wrappers, default args defined by wrapper. User can 
#input an alist and by this function the wrapper will append new args and overwrite conflicting arguments.
#one set of args either user or defaults are set as master
append.overwrite.alists= function(masterArgs,slaveArgs) {
  slaveArgs.to.overwrite = names(slaveArgs) %in% names(masterArgs)
  for(i in which(slaveArgs.to.overwrite))  slaveArgs[i] = masterArgs[match(names(slaveArgs[i]),names(masterArgs))]
  masterArgs.to.append = !(names(masterArgs) %in% names(slaveArgs))
  c(slaveArgs,masterArgs[masterArgs.to.append])
}
#sf9: colour function
fcol = function(ff,
                cols = NULL,
                orderByImportance = NULL,
                X.matrix = TRUE,
                hue = NULL,
                saturation = NULL,
                brightness = NULL,
                hue.range  = NULL,
                sat.range  = NULL,
                bri.range  = NULL,
                alpha = NULL,
                RGB = NULL,
                max.df=3,
                imp.weight = NULL,
                imp.exp = 1,
                outlier.lim = 3,
                RGB.exp = NULL) {
  
  ##ssf8.1: is between function
  ib <- function(x, low, high) (x -low) * (high-x) > 0
  ##ssf8.2: move center range of vector at mid with new width of span
  span <- function(x, mid, width) if(min(x)!=max(x)) {
    ((x-min(x))/(max(x)-min(x))-0.5)*width+mid
  } else {
    x[] = mid #fix to avoid division by zero
  }
  ##ssf8.3: compute widest range possible with given brightness or saturation
  auto.range = function(level,low=0,high=1) abs(min(level-low,high-level))*2
  ##ssf8.4: contain a vector such that any out side limits will be reduced to limits
  contain = function(x,low=0,high=1) {
    x[x>high]=high
    x[x<low ]=low
    x
  }
  
  #get/check data.frame/matrix, convert to df, remove outliers and normalize
  if(class(ff)=="forestFloor") {
    if(X.matrix) colM = ff$X else colM = ff$FCmatrix
    if(is.null(imp.weight)) imp.weight=TRUE
    if(is.null(orderByImportance)) orderByImportance = TRUE
  } else {
    colM=ff
    if(is.null(imp.weight)) imp.weight=FALSE
    if(is.null(orderByImportance)) orderByImportance = FALSE
  }
  
  #reorder colM by importance
  if(orderByImportance) if(class(ff) == "forestFloor") {
    colM = colM[,ff$imp_ind]
  } else {
    warning("orderByImportance=TRUE takes no effect for non 'forestFloor'-class. As if set to NULL or FALSE...")
  }
  
  #check colM is either data.frame or matrix
  if(!class(colM) %in% c("data.frame","matrix")) {
    stop(paste(class(colM),"input is neither matrix or data.frame"))
  }
  
  #convert matrix to data.frame
  colM = data.frame(colM)
  
  #checking selected cols
  if(is.null(cols)) cols = 1:dim(colM)[2] #select all columns
  if(length(cols)<1 || !is.numeric(cols) || any(!cols %in% 1:dim(colM)[2])) {
    stop("no cols selected or is not integer/numeric or wrong coloumns")
  }
  sel.colM = data.frame(colM[,cols])    #use only selected columns
  sel.cols = 1:length(cols) #update cols to match new col.indices of colM
  
  #auto choose colour system: RGB=TRUE is colours system one
  if(is.null(RGB)) if(length(cols)==1) RGB=TRUE else RGB=FALSE
  if(!RGB) {
    if(is.null(saturation)) saturation = .85
    if(is.null(brightness)) brightness = .75
    if(is.null(hue))        hue = .25
  } else {
    if(is.null(saturation)) saturation = 1
    if(is.null(brightness)) brightness = .75
    if(is.null(hue))        hue = .66    
    if(is.null(RGB.exp))    RGB.exp=1.2
    if(is.null(hue.range))  hue.range=2
  }
  
  #function to force catogorical features to become numeric
  as.numeric.factor <- function(x,rearrange=TRUE) {
    if(is.numeric(x)) return(x)
    if(rearrange) x = match(x,levels(droplevels(x))) else x = match(x,levels(x))
    return(x)
  }
  
  for(i in 1:dim(sel.colM)[2]) {
    if(is.factor(sel.colM[,i])) {
      this.fac=as.numeric.factor(sel.colM[,i])
      sel.colM[,i] = this.fac
    }
    if(is.character(sel.colM[,i])) sel.colM[,i] = as.numeric(sel.colM[,i])
  } 
  
  #restrain outliers by limit(std.dev) and normalize.
  sel.colM = box.outliers(sel.colM,limit=outlier.lim)
  
  #inflating data by importance
  if(imp.weight && length(cols)>1) {
    if(class(ff)=="forestFloor") {
      sel.imp = ff$importance[cols]
      non.negative.imp = sel.imp+min(sel.imp)
      sumnorm.imp =  non.negative.imp / sum(non.negative.imp)
      exp.imp = sumnorm.imp ^ imp.exp #included weight exponent
      impM = t(replicate(dim(colM)[1],exp.imp))
      sel.colM = sel.colM*impM #inflate by importance
      sel.colM = sel.colM / max(sel.colM)
    } else {warning("importance weighting only possible for class 'forestFloor'")}
  }
  
  #Setting up ranges for colours
  if(any(!c(class(hue),class(saturation),class(brightness)) %in% c("numeric","integer"))){
    stop("hue, saturation and brightness must be of class numeric or integer")
  }
  #correct input to be within [0,1]
  hue = hue - floor(hue)
  saturation = max(min(saturation,1),0)
  brightness = max(min(brightness,1),0)
  
  ###################
  ###colours system A:  1-way gradient Red-Green-BLUE scale
  
  if(RGB==TRUE) {
    if(is.null(bri.range)) bri.range=0.05
    if(is.null(alpha)) alpha=.7
    len.colM = box.outliers(sel.colM,limit=Inf)
    if(dim(len.colM)[2]==1) nX = as.numeric(len.colM[,1]) else nX = as.numeric(apply(len.colM,1,mean))
    hsvcol    = t(sapply(nX,function(x) rgb2hsv(x^RGB.exp,
                                                1-x^RGB.exp-(1-x)^RGB.exp,
                                                (1-x)^RGB.exp)))
    hue.vec = hsvcol[,1] * hue.range + hue
    hue.vec[hue.vec>1] = hue.vec[hue.vec>1] - floor(hue.vec[hue.vec>1])
    hsvcol[,1] = hue.vec
    sat.range = auto.range(saturation)
    hsvcol[,2] = span(hsvcol[,2],saturation,sat.range)
    hsvcol[,2] = contain(hsvcol[,2])
    bri.range = auto.range(brightness)
    hsvcol[,3] = span(hsvcol[,3],brightness,bri.range)
    hsvcol[,3] = contain(hsvcol[,3])
    colours = apply(hsvcol,1,function(x) hsv(x[1],x[2],x[3],alpha=alpha))
    #     a = mget(ls())
    #     print(str(a))
    return(colours) #function terminates with these colours
  }
  
  ############
  ##Colour system B: Hue, saturation, value, consist of a 1D, 2D and 3D scale
  
  #if maxPC is less than n selected coloumns
  #centering, no scaling and PCA is applied
  #output scores is transformed to range [0,1]
  #cols are correect to lower manifold number maxPC
  col.df = length(cols)
  if(col.df>max.df) {
    len.colM = box.outliers(prcomp(sel.colM)$x[,1:max.df],limit=Inf)
    col.df = max.df
  } else {
    len.colM = box.outliers(sel.colM,limit=Inf)
  }
  
  #define ranges if not defined for different dimensions
  if(is.null(hue.range)) {
    if(col.df==1) hue.range = .85
    if(col.df==2) hue.range = 1 #circular no range lim needed
    if(col.df==3) hue.range = 1 #circular no range lim needed
  }
  if(is.null(sat.range)) {
    if(col.df==1) sat.range = "not used"
    if(col.df==2) sat.range = auto.range(saturation)
    if(col.df==3) sat.range = auto.range(saturation)
  } 
  if(is.null(bri.range)) {
    if(col.df==1) bri.range = "not used"
    if(col.df==2) bri.range = "not used"
    if(col.df==3) bri.range = auto.range(brightness)
  }
  if(is.null(alpha)) alpha = min(1,400/dim(len.colM)[1])
  
  
  ##writing colour scale dependent on colour degrees of freedom(col.df)
  #one way gradient
  if(col.df==1) {
    hue.vec = as.numeric(len.colM[,1]) * hue.range + hue
    hue.vec[hue.vec>1] = hue.vec[hue.vec>1] - floor(hue.vec[hue.vec>1])
    colours = hsv(h = hue.vec,
                  s = saturation,
                  v = brightness,
                  alpha = alpha) #defining colour gradient along X3)
  }
  
  #two way gradient
  if(col.df==2) {
    hsvcol = t(rgb2hsv(len.colM[,1],len.colM[,2],1-apply(len.colM,1,mean)))
    hue.vec = hsvcol[,1] * hue.range + hue
    hue.vec[hue.vec>1] = hue.vec[hue.vec>1] - 1
    hsvcol[,1] = hue.vec
    #saturation is proportional with distance to center
    hsvcol[,2] = ((len.colM[,1]-mean(len.colM[,1]))^2
                  +(len.colM[,2]-mean(len.colM[,2]))^2)^sat.range * saturation
    hsvcol[,2] = hsvcol[,2] / max(hsvcol[,2])
    hsvcol[,3] = brightness
    colours = hsv(hsvcol[,1],hsvcol[,2],hsvcol[,3],alpha=alpha)
  }  
  
  #three-way gradient
  if(col.df==3) {  
    hsvcol      = t(rgb2hsv(len.colM[,1],len.colM[,2],len.colM[,3]))
    #set hue
    hue.vec     = hsvcol[,1] * hue.range + hue
    hue.vec[hue.vec>1] = hue.vec[hue.vec>1] - 1
    hsvcol[,1]  = hue.vec
    #set sat
    span.sat    = span(hsvcol[,2],saturation,sat.range)
    hsvcol[,2]  = contain(span.sat)
    #set bri
    mean.bri    = apply(len.colM,1,mean)
    span.bri    = span(mean.bri,brightness,bri.range)
    hsvcol[,3]  = contain(span.bri)
    colours     = hsv(hsvcol[,1],hsvcol[,2],hsvcol[,3],alpha=alpha)
  }
  
  return(colours)
}



forestFloor = function(rfo,X,calc_np=FALSE) { 
  
  #check the RFobject have a inbag
  if(is.null(rfo$inbag)) stop("input randomForest-object have no inbag, set keep.inbag=T,
                              try, randomForest(X,Y,keep.inbag=T) for regression where Y is numeric
                              and, cinbag(X,Y,keep.inbag=T,keep.forest=T) for binary-class where Y is factor
                              ..cinbag is from trimTrees package...
                              error condition: if(is.null(rfo$inbag))")
  
  #make node status a integer matrix
  ns = rfo$forest$nodestatus
  storage.mode(ns) = "integer"
  
  
  #translate binary classification RF-object, to regression mode
  if(rfo$type=="classification") {
    if(length(levels(rfo$y))!=2) stop("no multiclass, must be binary classification.
                                      error condition: if(length(levels(rfo$y))!=2")
    print("RF is classification, converting factors/categories to numeric 0 an 1")
    Y = as.numeric((rfo$y))-1
    cat(" defining",levels(rfo$y)[1]," as 0\n defining",levels(rfo$y)[2],"as 1")
    rfo$forest$leftDaughter  = rfo$forest$treemap[,1,] #translate daughter representation to regression mode
    rfo$forest$rightDaughter = rfo$forest$treemap[,2,] 
    ns[ns==1] = -3  ##translate nodestatus representation to regression mode
    if(is.null("rfo$inbagCount")) stop("classification topology not supported with randomForest() {randomForest}
                                       Grow forest with cinbag::trimTrees instead of randomForest(). The two
                                       functions are identical, except cinbag() entails a more detailed inbag record,
                                       which is needed to estimate binary node probabilities.
                                       error condition:  if(is.null('rfo$inbagCount'))")
    
    if(!calc_np) stop("node predictions must be re-calculated for random forest of type classification, set calc_np=T)
                      error conditions: if(!calc_np && rfo$type='classification')")
    
    inbag = rfo$inbagCount
  } else {
    Y=rfo$y
    inbag = rfo$inbag
  }
  
  
  #preparing data, indice-correction could be moved to C++
  #a - This should be fethed from RF-object, flat interface
  ld = rfo$forest$leftDaughter-1 #indice correction, first element is 0 in C++ and 1 in R.
  storage.mode(ld) = "integer"
  rd = rfo$forest$rightDaughter-1
  storage.mode(rd) = "integer"
  bv = rfo$forest$bestvar-1
  storage.mode(bv) = "integer"
  np = rfo$forest$nodepred
  storage.mode(np) = "double"
  bs = rfo$forest$xbestsplit
  storage.mode(bs) = "double"
  ib = inbag
  storage.mode(ib) = "integer"
  Yd = as.numeric(Y)
  storage.mode(Yd) = "double"
  ot  = rfo$oob.times
  storage.mode(ot) = "integer"
  
  
  ##recording types of variables
  xlevels = unlist(lapply(rfo$forest$xlevels,length),use.names=F)
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
    ntree=rfo$ntree,
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
  
  
  
  #writing out list
  imp = as.matrix(rfo$importance)[,1]
  out = list(X=X,Y=Y,
             importance = imp,
             imp_ind = sort(imp,decreasing=TRUE,index.return=TRUE)$ix,
             FCmatrix = localIncrements
  )
  class(out) = "forestFloor"
  return(out)
}

#globalVariables("FC","X")
##experimental function to forestFloor 2d with ggplot2
ggPlotForestFloor = function(
  ff, #a forestFloor object
  plot_seq=NULL, #a sequence of windows to plot
  col=NULL, # a colour vector of N length, use fcol
  orderByImportance = TRUE
  ) {
  
  if(is.null(plot_seq)) plot_seq = 1:max(6,dim(ff$X)[2])
  if(is.null(col)) col.points=fcol(ff)
  if(orderByImportance) {
    #if order by importance but no importance in forestFloor object
    #then create a vector with same order as training set
    if(is.null(ff$imp_ind)) imp.ind = 1:max(plot_seq) else imp.ind=ff$imp_ind
  } else {
    imp.ind = 1:dim(ff$X)[2]
  }
  thisVar=1 #declare this to kill a R check note
  print(plot_seq)
  #define plotting of one frame
  
  make.one.frame = function(ff=ff,
                            thisVar=thisVar,
                            col.points=col.points,
                            imp.ind=imp.ind) {
  #if not importance sorting, sort by training set col order
    
    
    #get requested dataset
    df = data.frame(X = ff$FCmatrix[,imp.ind[thisVar]],
                    FC= ff$X  [,imp.ind[thisVar]]  )
    #start plot plot points
    h.out  <- ggplot(df,aes_string("FC","X")) + 
      geom_point(col = col.points) +
      xlab(names(ff$X)[imp.ind[thisVar]]) +
      ylab("")
  }
  
  #plot all desired frmesm
  all.ggplots = lapply(plot_seq, function(thisVar) {
    make.one.frame(ff=ff,
                   thisVar=thisVar,
                   col.points=col.points,
                   imp.ind=imp.ind)
  })
  
  do.call(grid.arrange,c(all.ggplots)) 
}

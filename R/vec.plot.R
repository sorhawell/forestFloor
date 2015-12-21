#f2 - show vec plot 2D and 3D
vec.plot = function(model,X,i.var,grid.lines=100,VEC.function=mean,zoom=1,limitY=F,moreArgs=list(),...) {
  
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
    #merge arguments for plot3d 
    plot3dArgs.std = alist(
      x=values.to.plot[,1],
      y=values.to.plot[,2],
      z=yhat.obs,
      xlab=names(X)[i.var][1],
      ylab=names(X)[i.var][2],
      main="VEC-SURFACE")
    plot3dArgs.all = append.overwrite.alists(list(...),plot3dArgs.std)
    do.call(plot3d,plot3dArgs.all)
    
    #merge arguments for surf3d
    surfArgs.std = alist(
      x     = scales[[1]],
      y     = scales[[2]],
      z     = yhat.vec,
      col   = "#404080",
      size  = 4,
      alpha = 0.4)
    surfArgs.all = append.overwrite.alists(moreArgs,surfArgs.std)
    do.call(surface3d,surfArgs.all)
  }else{ #otherwise if 1D VEC-space
    if(limitY) {ylim = range(model$y)} else {ylim = NULL}
    plotArgs.std = alist(
      x=scales[[1]],
      y=yhat.vec,
      type="l",
      xlab=names(X)[i.var][1],
      col = "red",
      ylim=ylim)
    plotArgs.all = append.overwrite.alists(list(...),plotArgs.std)
    do.call(plot,plotArgs.all)
    
    pointsArgs.std = alist(
      x = values.to.plot,
      y = yhat.obs,
      col="#DD202030")
    pointsArgs.all = append.overwrite.alists(moreArgs,pointsArgs.std)  
    do.call(points,pointsArgs.all)
  }
}

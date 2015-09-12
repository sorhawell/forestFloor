show3d = function(x,...) {
  UseMethod("show3d")
}


##3d plot of forestFloor_multiClass
show3d.forestFloor_multiClass = function(
  x,Xi,FCi=NULL,label.seq=NULL,kknnGrid.args=list(NULL),
  plot.rgl.args=list(),plot_GOF=FALSE,user.gof.args=list(NULL),...) {
  
  if(class(x)!="forestFloor_multiClass") stop("class(x) != forestFloor_multiClass")
  if(is.null(FCi)) FCi = Xi
  if(is.null(label.seq)) label.seq = 1:min(8,length(levels(x$Y)))
  
  #compute mean goodness of fit of label surfaces of 3d-plot
  #gof is the squared pearson correlation of any FC and fitted surface
  if(plot_GOF) {
    fits = lapply(label.seq, function(label.ind) {
      forestFloor_obj = list(FCmatrix = x$FCarray[,,label.ind],
                             X=apply(x$X[,],2,as.numeric.factor)
      )
      class(forestFloor_obj)="forestFloor_multiClass"
      convolute_ff2(forestFloor_obj,
                    Xi=Xi,
                    FCi=FCi,
                    userArgs.kknn=user.gof.args)
    })
    label_gofs =sapply(label.seq,function(label.ind) {
      joinedFC = if(length(FCi)>1) {
        apply(x$FCarray[,FCi,label.ind],1,sum)
      } else {
        x$FCarray[,FCi,label.ind]
      }
      #       plot(fits[[label.ind]],joinedFC)
      #       plot(fits[[label.ind]],fits[[label.ind]]-joinedFC)
      cor(fits[[label.ind]],joinedFC)^2
    })
    mean_gof = round(mean(label_gofs),digits=2)
  }
  
  with(x, {
    for(i in label.seq) {
      if(length(FCi)>1) {
        FCcombined = apply(FCarray[,FCi,i],1,sum)
      } else {
        FCcombined = FCarray[,FCi,i]    
      }
        
      std.rgl.args = list(X[,Xi[1]],
                          X[,Xi[2]],
                          FCcombined,
                          add = {if(i==label.seq[1]) F else T},
                          col=(i)^((i==as.numeric(Y))*1),
                          alpha=1-.8*(i!=as.numeric(Y)),
                          type=if(length(label.seq)*dim(X)[1] <500) "s" else "p",
                          size=if(length(label.seq)*dim(X)[1] <500) 1 else 3,
                          main = if(plot_GOF) paste0("R^2=",mean_gof) else "",
                          xlab = names(x$X)[Xi[1]],
                          ylab = names(x$X)[Xi[2]],
                          zlab = if(length(FCi==1)) names(x$X)[FCi] else "joined FC"
      )
      run.args = append.overwrite.alists(plot.rgl.args,std.rgl.args)
      do.call(plot3d,run.args)
      
      ffpar = list(FCmatrix=FCarray[,,i],X=X)
      class(ffpar) = "forestFloor_multiClass"
      
      #merge user arguments for grid estimation with default arguments and estimate...
      default.grid.args = alist(ff=ffpar,Xi=Xi,FCi=FCi,zoom=1,
                                grid=25,userArgs.kknn=alist(k=10))
      run.args = append.overwrite.alists(kknnGrid.args,default.grid.args)
      Spar = do.call(convolute_grid,run.args)
      
      #draw grid
      persp3d(unique(Spar[,2]),
              unique(Spar[,3]),
              Spar[,1],
              alpha=0.15,
              col=i,
              add=T)
    }
  })
}

show3d.forestFloor_regression = function(
  x,
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
  surf.rgl.args = alist(),
  user.gof.args = alist(),
  plot_GOF = TRUE,
  ...) {
if(class(x)!="forestFloor_regression") stop("x, must be of class forestFloor_regression")
  if(length(Xi)!=2) {
    warning("Xi should be of length 2, if 1 first elements is used twice, if >2 only two first elements is used")
    if(length(Xi) > 2) Xi=Xi[1:2] else Xi = Xi[c(1,1)]
  }
  if(!all(Xi %in% 1:dim(x$X)[2]))   stop( "input  Xi points to columns indices out of range of feature matrix x$X")
  if(is.null(FCi)) FCi=Xi
  if(!all(FCi %in% 1:dim(x$FCmatrix)[2]) && length(FCi)>0) stop("input FCi points to columns indices out of range of feature matrix x$X")
  
  #should Xi and FCi refer to coloumns sorted by importance?
  if(sortByImportance) {
    Xi  = x$imp_ind[ Xi]
    FCi = x$imp_ind[FCi]
  }
  
  #fetch selected coloums from object
  X = x$X[,Xi]
  FC = x$FCmatrix[,FCi]
  
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
 
  #computing goodness-of-viusalization
  if(plot_GOF) {
    fittedFC = convolute_ff2(x,
                             Xi=Xi,
                             FCi=FCi,
                             userArgs.kknn=user.gof.args)
    joinedFC = apply(x$FCmatrix[,FCi],1,sum)
    sqCor = cor(joinedFC,fittedFC)^2
    mean_gof = paste("R^2=",round(sqCor,digits=2),collapse="")
  } else {mean_gof=""}

  #plotting points
  #merge current/user, wrapper arguments for plot3d in proritized order
  wrapper_arg = list(x=xaxis, y=yaxis, z=zaxis, col=col,main=mean_gof,
                     xlab=names(X)[1],ylab=names(X)[2],zlab=paste(names(x$X[,FCi]),collapse=" - "),
                     alpha=.4,size=3,scale=.7,avoidFreeType = TRUE,add=FALSE)
  calling_arg = append.overwrite.alists(plot.rgl.args,wrapper_arg)
  do.call("plot3d",args=calling_arg)
  
  #plotting surface
  #merge arguments again
  if(surface) {
    #compute grid
    grid = convolute_grid(x, Xi=Xi,FCi=FCi, limit=limit, grid=grid.lines, zoom=zoom,  userArgs.kknn = kknnGrid.args)
    wrapper_arg = alist(x=unique(grid[,2]),y=unique(grid[,3]),z=grid[,1],add=TRUE,alpha=0.2,col=c("grey","black")) #args defined in this wrapper function
    calling_arg = append.overwrite.alists(surf.rgl.args,wrapper_arg)   
    do.call("persp3d",args=calling_arg)
  }

invisible()
}
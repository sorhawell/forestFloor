
#sub function to fix categorical features
as.numeric.factor <- function(x,drop.levels=TRUE) {
  if(is.numeric(x)) return(x) ## if already numeric, do onothing
  if(drop.levels) x = match(x,levels(droplevels(x))) else x = match(x,levels(x))
  return(x)
}



##3d plot of forestFloor_multiClass
show3d_forestFloor_multiClass = function(x,Xvars,FCvars=NULL,label.seq=NULL,user.grid.args=list(NULL),user.rgl.args=list(),
                      compute_GOF=FALSE,user.gof.args=list(NULL)) {
  if(class(x)!="forestFloor_multiClass") stop("class(x) != forestFloor_multiClass")
  if(is.null(FCvars)) FCvars = Xvars
  if(is.null(label.seq)) label.seq = 1:min(8,length(levels(x$Y)))
  
  #compute mean goodness of fit of label surfaces of 3d-plot
  #gof is the squared pearson correlation of any FC and fitted surface
  if(compute_GOF) {
    fits = lapply(label.seq, function(label.ind) {
      forestFloor_obj = list(FCmatrix = x$FCarray[,,label.ind],
                             X=apply(x$X[,],2,as.numeric.factor)
      )
      class(forestFloor_obj)="forestFloor"
      convolute_ff2(forestFloor_obj,
                    Xvars=Xvars,
                    FCvars=FCvars,
                    userArgs.kknn=user.gof.args)
    })
    label_gofs =sapply(label.seq,function(label.ind) {
      joinedFC = if(length(FCvars)>1) {
        apply(x$FCarray[,FCvars,label.ind],1,sum)
      } else {
        x$FCarray[,FCvars,label.ind]
      }
      #       plot(fits[[label.ind]],joinedFC)
      #       plot(fits[[label.ind]],fits[[label.ind]]-joinedFC)
      cor(fits[[label.ind]],joinedFC)^2
    })
    mean_gof = round(mean(label_gofs),digits=2)
  }
  
  
  with(x, {
    for(i in label.seq) {
      if(length(FCvars)>1) FCcombined = apply(FCarray[,FCvars,i],1,sum) else FCcombined = FCarray[,FCvars,i]
      
      
      
      
      
      std.rgl.args = list(X[,Xvars[1]],
                          X[,Xvars[2]],
                          FCcombined,
                          add = {if(i==label.seq[1]) F else T},
                          col=(i)^((i==as.numeric(Y))*1),
                          alpha=1-.8*(i!=as.numeric(Y)),
                          type=if(length(label.seq)*dim(X)[1] <500) "s" else "p",
                          size=if(length(label.seq)*dim(X)[1] <500) 1 else 3,
                          main = if(compute_GOF) paste0("R^2=",mean_gof) else "",
                          xlab = names(x$X)[Xvars[1]],
                          ylab = names(x$X)[Xvars[2]],
                          zlab = if(length(FCvars==1)) names(x$X)[FCvars] else "joined FC"
      )
      run.args = append.overwrite.alists(user.rgl.args,std.rgl.args)
      do.call(plot3d,run.args)
      
      ffpar = list(FCmatrix=FCarray[,,i],X=X)
      class(ffpar) = "forestFloor"
      
      #merge user arguments for grid estimation with default arguments and estimate...
      default.grid.args = alist(ff=ffpar,Xvars=Xvars,FCvars=FCvars,zoom=1,
                                grid=25,userArgs.kknn=alist(k=10))
      run.args = append.overwrite.alists(user.grid.args,default.grid.args)
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
#F 3.2 : Tiltle
#




#2dplot of forstFloor_multiClass
plot.forestFloor_multiClass  = function(
  x,
  plot_seq=NULL,
  label.seq=NULL,
  limitY=TRUE,
  colLists = NULL,
  order_by_importance=TRUE,
  fig.columns = NULL,
  plot_GOF=FALSE,
  GOF_col=NULL,
  jitter_these_cols = NULL,
  jitter.factor = NULL,
  compute_GOF = F,
  ...) {
  void = with(x,{ #inside x object
    
    if(is.null(plot_seq))         plot_seq = 1:min(18,dim(X)[2])
    fig.columns = if(is.null(fig.columns)) {
      if(length(plot_seq<=4)) c(1,2,3,2)[length(plot_seq)] else 3
    }
    if(order_by_importance) plot_seq = x$imp_ind[plot_seq]
    if(is.null(label.seq)) label.seq = 1:min(8,length(levels(Y)))
    if(is.null(colLists)) colLists =
      lapply(1:length(label.seq), function(i) factor(rep(i,dim(X)[1])))
    if(plot_GOF && is.null(GOF_col)) GOF_col = sapply(colLists,function(x) x[1])
    
    n.plots = min(dim(X)[2],length(plot_seq))
    plotdims.y = min(ceiling(n.plots/fig.columns),5)
    plotdims.x = min(fig.columns , n.plots)
    par(mfrow=c(plotdims.y,plotdims.x),mar = c(2,2,3,1))
      
    
    if(plot_GOF) {
      fits = lapply(label.seq,function(label.ind) {
        ff2 = convolute_ff(list(FCmatrix = x$FCarray[,,label.ind],
                                X=apply(x$X[,],2,as.numeric.factor,),
                                Xind = apply(x$X,2,function(xcol) { 
                                  sort(xcol,index.return=TRUE)$ix})),
                           these.vars=sort(plot_seq))
      })
      
      fit.seq = (1:length(label.seq)) %in% 1:length(Y)
    }  else fits = invisible()
    
    
   
    for(j in plot_seq)
      for(i in label.seq) {
        X.jittered = if(j %in% jitter_these_cols) jitter(X[,j],jitter.factor) else X[,j]
        if(i==1) {
          plot(X.jittered,
               FCarray[,j,i],
               col=colLists[[i]],
               ...,
               ylim=range(FCarray),
               ylab= " ",
               xlab = paste0("X[ ,",j,"]"),
               #if fit is plotted goodness of fit is added to title
               main = paste(names(X)[j],if(plot_GOF) {paste("R^2=",round(mean(sapply(label.seq,function(lSeq) {
                 #plot(fits[[lSeq]]$FCfit[,j],FCarray[,j,lSeq],col=4)
                 cor( fits[[lSeq]]$FCfit[,j],FCarray[,j,lSeq])^2
               }))
               ,digits=2))
               
               } else "")
          )
        } else {
          points(X.jittered,FCarray[,j,i],col=colLists[[i]],...)
        }
        if(plot_GOF) with(fits[[i]],{
          points(X[Xind[,j],j],FCfit[Xind[,j],j],col=GOF_col[i],cex=0.05,type="l",lwd=5) 
        })
      }
    return(fits)
  })
}

# 
# #this function can plot triad-diagrams for 3way-classification
# plot_ffmc_K3 = function(ffmc,
#                    Xvars=NULL,
#                    includeTotal=TRUE,
#                    label.col=NULL,
#                    fig.cols=3,
#                    fig.rows= NULL,
#                    auto.alpha=0.25,
#                    fig3d=FALSE,
#                    restore_par=TRUE,
#                    set_pars=TRUE,
#                    zoom.fit=NULL,
#                    var.col = NULL,
#                    plot.sep.centroid=FALSE) {
#   if(class(ffmc)!= "forestFloor_MultiClass") {
#     stop("this function takes input of class 'forestFloor_MultiClass'")
#   }
#   if(length(unique(ffmc$Y))!=3) stop("this plot is only for 3 classes")
#   
#   #all variables as those used to calculate total separation
#   if(is.null(Xvars)) {
#     Xvars = 1:dim(ffmc$X)[2]
#     calVars = Xvars
#     plot_total = F
#   }
#   #if Xvars is 0, only total separation will be plotted
#   if(Xvars[1]==0) {
#     calVars = 1:dim(ffmc$X)[2]
#     if(is.null(fig.cols)) fig.cols=1
#     plot_total=T
#   }else {
#     plot_total=F
#     calVars = Xvars
#   }
#   
#   #defining colours
#   #if no colours choose default add auto.alpha
#   N = length(ffmc$Y)
#   
#   if(is.null(label.col)) {
#     label.col = c(rgb(0,0,0,min(1,400/N*auto.alpha)),
#                   rgb(1,0,0,min(1,400/N*auto.alpha)),
#                   rgb(0,1,0,min(1,400/N*auto.alpha)))
#   } else {
#     #if colours, still add auto alpha if such exist
#     if(!is.null(auto.alpha)) {
#       RGBAs = col2rgb(label.col,alpha=T)
#       RGBAs[4,] = 400/N*auto.alpha
#       label.col = apply(RGBAs,2,function(x) rgb(x[1],x[2],x[3],x[4]*255,max=255))
#     }
#   }
#   if(length(label.col)!=3) warning("only three colours are needed in label.col")
#   if(is.null(var.col)) {
#     Col = label.col[match(ffmc$Y,levels(droplevels(ffmc$Y)))]
#   } else {
#     Col = "var.col" #wait modifying col until plot loop to chose separate color for each plot
#   }
#   
#   #remove alpha for corners
#   corner.col = apply(col2rgb(label.col),2,function(x) rgb(x[1],x[2],x[3],max=255))
#   
#   #modifying graphical paremeters
#   pars = par(no.readonly = TRUE) 
#   if(is.null(fig.rows)) fig.rows = min(ceiling(length(Xvars)/fig.cols),5)
#   if(set_pars) par(mfrow=c(fig.rows,fig.cols),mar=c(3,3,2,2))
#   
#   #computing K3-simplex axis vectors. First axis is vertically upward/north
#   #second and third axis are pointing west-south-west(-150 degrees)
#   #... and east-south-east(-30 degrees)
#   xc1 = cos(-30/360*pi*2)
#   xc2 = sin(-30/360*pi*2)
#   xd1 = c(0,xc1*2/3,-xc1*2/3)
#   xd2 = c(2/3,xc2*2/3,xc2*2/3)
#   
#   #compute grand bootstrap mean
#   grandMean = with(ffmc,{
#     grandMean = sapply(levels(Y),function(thisLabel) sum((Y==thisLabel) * sumOfInbags))
#     grandMean=grandMean /sum(grandMean)
#     return(grandMean)
#   })
#   
#   #plot all figures
#   if(includeTotal && length(Xvars)>1) ite=c(Xvars,0) else ite=Xvars
#   if(plot_total) ite=0 #only plot total sep. but use all variables (Xvars)
#   for(i in ite) {  
#     if(i == 0) FC=apply(ffmc$FCarray[,calVars,],c(1,3),sum) else FC=ffmc$FCarray[,i,]
#     #draw triangle
#     mainname = if(i!=0) names(X)[i] else {
#       if(plot_total) "total separation" else "combined sepratation"
#     }
#     
#     #find mean of stratified forest
#     cent3 = grandMean
#     cent2 = c(sum(cent3*xd1),sum(cent3*xd2)) #convert to simplex space
#     #find range zoom fit range
#     zoom.range = range(c(apply(xd1*t(FC[,]),2,sum),
#                          apply(xd2*t(FC[,]),2,sum)))/zoom.fit
#     plot(xd1,xd2,col=corner.col,
#          xlim = if(!is.null(zoom.fit)) zoom.range+cent2[1] else c(-.6,.6),
#          ylim = if(!is.null(zoom.fit)) zoom.range+cent2[2] else c(-.35,.7),
#          pch=1,cex=2,main=mainname)
#     points(xd1[c(1:3,1)],xd2[c(1:3,1)],col="#00001050",xlim=c(-1,1),ylim=c(-1,1),type="l")
#     #draw separation lines
#     segments((xd1[1]+xd1[2])/2,(xd2[1]+xd2[2])/2,0,0)
#     segments((xd1[2]+xd1[3])/2,(xd2[2]+xd2[3])/2,0,0)
#     segments((xd1[3]+xd1[1])/2,(xd2[3]+xd2[1])/2,0,0)
#     
#     #draw points
#     
#     points(apply(xd1*t(FC[,]),2,sum)+cent2[1],
#            apply(xd2*t(FC[,]),2,sum)+cent2[2],cex=0.3,
#            col= if(Col[1]=="var.col") do.call(fcol,c(list(ffmc$X,i),var.col)) else Col)
#     
#     points(cent2[1],cent2[2],pch=4,cex=3,col=4)
#     points(cent2[1],cent2[2],pch=4,cex=3,col=4)
#     
#     if(fig3d && i!=0) {
#       plot3d(rep(xd1,N),rep(xd2,N)
#              ,ffmc$X[,i],
#              col=label.col,
#              pch=1,cex=2,main=mainname)
#       plot3d(apply(xd1*t(FC[,]),2,sum)+cent2[1],
#              apply(xd2*t(FC[,]),2,sum)+cent2[2],
#              ffmc$X[,i],col=Col,add=TRUE)
#       print("pausing press enter to plot next 3D-Graph")
#       readline()
#     }
#   }
#   #end of plotting, restore graphical pars
#   if(restore_par) par(pars) else return(pars)
# }

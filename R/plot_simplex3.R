plot_simplex3 = function(ff,
                         Xi=NULL,
                         includeTotal=TRUE,
                         label.col=NULL,
                         fig.cols=3,
                         fig.rows= NULL,
                         auto.alpha=0.25,
                         fig3d=FALSE,
                         restore_par=TRUE,
                         set_pars=TRUE,
                         zoom.fit=NULL,
                         var.col = NULL,
                         plot.sep.centroid=TRUE) {
  if(class(ff)!= "forestFloor_multiClass") {
    stop("this function takes input of class 'forestFloor_multiClass'")
  }
  if(length(unique(ff$Y))!=3) stop("this plot is only for 3 classes")
  
  #all variables as those used to calculate total separation
  if(is.null(Xi)) {
    Xi = 1:dim(ff$X)[2]
    calVars = Xi
    plot_total = F
  }
  #if Xi is 0, only total separation will be plotted
  if(Xi[1]==0) {
    calVars = 1:dim(ff$X)[2]
    if(is.null(fig.cols)) fig.cols=1
    plot_total=T
  }else {
    plot_total=F
    calVars = Xi
  }
  
  #defining colours
  #if no colours choose default add auto.alpha
  N = length(ff$Y)
  
  if(is.null(label.col)) {
    label.col = c(rgb(0,0,0,min(1,400/N*auto.alpha)),
                  rgb(1,0,0,min(1,400/N*auto.alpha)),
                  rgb(0,1,0,min(1,400/N*auto.alpha)))
  } else {
    #if colours, still add auto alpha if such exist
    if(!is.null(auto.alpha)) {
      RGBAs = col2rgb(label.col,alpha=T)
      RGBAs[4,] = 400/N*auto.alpha
      label.col = apply(RGBAs,2,function(x) rgb(x[1],x[2],x[3],x[4]*255,maxColorValue=255))
    }
  }
  if(length(label.col)!=3) warning("only three colours are needed in label.col")
  if(is.null(var.col)) {
    Col = label.col[match(ff$Y,levels(droplevels(ff$Y)))]
  } else {
    Col = "var.col" #wait modifying col until plot loop to chose separate color for each plot
  }
  
  #remove alpha for corners
  corner.col = apply(col2rgb(label.col),2,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))
  
  #modifying graphical paremeters
  pars = par(no.readonly = TRUE) 
  if(is.null(fig.rows)) fig.rows = min(ceiling(length(Xi)/fig.cols),5)
  if(set_pars) par(mfrow=c(fig.rows,fig.cols),mar=c(3,3,2,2))
  
  #computing K3-simplex axis vectors. First axis is vertically upward/north
  #second and third axis are pointing west-south-west(-150 degrees)
  #... and east-south-east(-30 degrees)
  xc1 = cos(-30/360*pi*2)
  xc2 = sin(-30/360*pi*2)
  xd1 = c(0,xc1*2/3,-xc1*2/3)
  xd2 = c(2/3,xc2*2/3,xc2*2/3)
  
  #compute grand bootstrap mean
  grandMean = with(ff,{
    grandMean = sapply(levels(Y),function(thisLabel) sum((Y==thisLabel) * sumOfInbags))
    grandMean=grandMean /sum(grandMean)
    return(grandMean)
  })
  
  #plot all figures
  if(includeTotal && length(Xi)>1) ite=c(Xi,0) else ite=Xi
  if(plot_total) ite=0 #only plot total sep. but use all variables (Xi)
  for(i in ite) {  
    if(i == 0) FC=apply(ff$FCarray[,calVars,],c(1,3),sum) else FC=ff$FCarray[,i,]
    #draw triangle
    mainname = if(i!=0) names(ff$X)[i] else {
      if(plot_total) "total separation" else "combined sepratation"
    }
    
    #find mean of stratified forest
    cent3 = grandMean
    cent2 = c(sum(cent3*xd1),sum(cent3*xd2)) #convert to simplex space
    #find range zoom fit range
    zoom.range = range(c(apply(xd1*t(FC[,]),2,sum),
                         apply(xd2*t(FC[,]),2,sum)))/zoom.fit
    plot(xd1,xd2,col=corner.col,
         xlim = if(!is.null(zoom.fit)) zoom.range+cent2[1] else c(-.6,.6),
         ylim = if(!is.null(zoom.fit)) zoom.range+cent2[2] else c(-.35,.7),
         pch=1,cex=2,main=mainname)
    points(xd1[c(1:3,1)],xd2[c(1:3,1)],col="#00001050",xlim=c(-1,1),ylim=c(-1,1),type="l")
    #draw separation lines
    segments((xd1[1]+xd1[2])/2,(xd2[1]+xd2[2])/2,0,0)
    segments((xd1[2]+xd1[3])/2,(xd2[2]+xd2[3])/2,0,0)
    segments((xd1[3]+xd1[1])/2,(xd2[3]+xd2[1])/2,0,0)
    
    #draw points
    
    points(apply(xd1*t(FC[,]),2,sum)+cent2[1],
           apply(xd2*t(FC[,]),2,sum)+cent2[2],cex=0.3,
           col= if(Col[1]=="var.col") do.call(fcol,c(list(ff$X,i),var.col)) else Col)
    
    if(plot.sep.centroid) points(cent2[1],cent2[2],pch=4,cex=3,col=4)
    
    
    if(fig3d && i!=0) {
      plot3d(rep(xd1,N),rep(xd2,N)
             ,ff$X[,i],
             col=label.col,
             pch=1,cex=2,main=mainname)
      plot3d(apply(xd1*t(FC[,]),2,sum)+cent2[1],
             apply(xd2*t(FC[,]),2,sum)+cent2[2],
             ff$X[,i],col=Col,add=TRUE)
      print("pausing press enter to plot next 3D-Graph")
      readline()
    }
  }
  #end of plotting, restore graphical pars
  if(restore_par) par(pars) else return(pars)
}

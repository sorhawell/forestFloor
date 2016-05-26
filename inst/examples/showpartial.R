  library(rgl)
  #function to make a copy paste print of matrix
  printMat = function(m) {
    cat(" t(matrix(nrow=",nrow(m),", c( \n")
    for(i in 1:nrow(m)) {
      
      if(i!=nrow(m))  cat(paste(paste(m[i,],collapse=","),",","\n"))
      if(i==nrow(m))  cat(paste(paste(m[i,],collapse=",")    ,"\n"))
    }
    cat(")))")
  }
  
  #quick handle for drawing lines
  draw.lines = function(x_points,y_range=c(0,pi),steps=100,fun,fix_y=FALSE,fix_x=FALSE,...) {
    Args=list(...)
    for(i in 1:n_lines) {
      ys = seq(y_range[1],y_range[2],le=steps)
      if(fix_y!=FALSE) y = fix_y else y = ys 
      if(fix_x!=FALSE) x = fix_x else x = x_points[i]
      do.call(lines3d,
        c(list(
            x = x,
            y = y,
            z = fun(rep(x_points[i],steps),ys))
          ,Args)
      )
    }
  }
  
  #quick handle for drawing points
  draw.points = function(x_points,y_points,fun,...) {
    Args = list(...)
    do.call(plot3d, c(list(x = x_points,y = y_points,z = fun(x_points,y_points)),
                      Args)
    )
  }
  #quick handle for seq()
  seq2 = function(Range,length.out=100) seq(Range[1],Range[2],length.out = length.out)
  
  #plot limits
  yran=c(pi/6,pi)
  xran=c(0,pi*2/3)
  mirror_x=-.2
  
  #data points
  set.seed(0)
  n_lines = 14 #how many line xamples
  x_points = sort(jitter(seq(.0,.5,le=n_lines)*pi,amount=.23))[-1]
  y_points = ((runif(n_lines,.2,.8))*pi)[-1]
  
  #create 2-way feature grid, hidden function and compute output values
  xseq = seq2(xran)
  X = as.matrix(expand.grid(xseq,seq2(yran)))
  f = function(x,y) sin(x)^8*sin(y)^8
  y = f(X[,1],X[,2])
  
  

  #draw extrapolated points for computing partial dependence 
  for(xi in x_points) {
    plot3d(xi,y_points,f(xi,y_points),add=T,size=6,col="darkgreen")
  }
  for(yi in y_points) {
    xsorted=sort(x_points)
    lines3d(xsorted ,yi,f(xsorted,yi),add=T,size=5,col="darkgreen",lwd=1.5)
    lines3d(mirror_x,yi,f(xsorted,yi),add=T,size=5,col="darkgreen",lwd=1.5)
    
  }
  
  #draw slices of model structure, each intersecting a data.point
  draw.lines(x_points = x_points,
             y_range  = yran,
             steps = 100,
             fun=f,
             lwd=1.5)
  
  #draw data centroid slice on surface and on mirror plane, corresponding to 1D-vec plot
  with(new.env(), {
       x = rep(mean(x_points),100)
       y = seq2(yran)
       z = f(x,y)+0.02
       lines3d(x,y,z,alpha=1,col="red",lwd=5)
       lines3d(mirror_x,y,z,alpha=1,lwd=5,col="red")
  })
    
  
  #compute mean partial output for all data.points
  mean.partial = sapply(y_points, function(y) {
    mean(f(x_points,y))
  })
  #sort data points for plotting projected partial dependance line in order
  y.ind = sort(y_points,ind=T)$ix
  #compute 
  xs_mirror = rep(mirror_x,le=length(mean.partial))
  
  #draw projections in mirror plane
  for(i in 1:n_lines) {
    #draw all slices of function in mirror plane
    draw.lines(x_points,yran,fun=f,fix_x=mirror_x) #fix_x set all xcoords to mirror_x
    #draw partial dependence lines + black dots to mark at data points in 
    lines3d(x=mirror_x,y=y_points[y.ind],z=mean.partial[y.ind],lwd=5,col="darkgreen")
    plot3d( x=mirror_x,y=y_points[y.ind],z=mean.partial[y.ind],size=3,add=T,col="black")
    #draw data points
    plot3d(mirror_x,y=y_points,z=f(x_points,y_points),col="blue",type="p",size=8.1,add=T,alpha=1)
  }
  #draw all extrapolated date.points in mirror plane
  for(xi in x_points) plot3d(mirror_x,y_points,f(xi,y_points),add=T,size=4,alpha=1,col="darkgreen")
   
  
  #draw 14 data.point examples
  draw.points(x_points,y_points,fun=function(x,y) f(x,y)+0.001,
              add = TRUE, type = "p",
              alpha = 1, size=8.1,col="blue")
  
  

  
  par3d(windowRect=c(0,  0, 1920,  1080)*1)
  points3d(expand.grid(list(c(-.2,2.1),c(.5,4),c(1.1,-.1))),col="white")
  #observer3d(-0.42,-.15, z =4.1, auto = FALSE)
  view3d(userMatrix =  
           t(matrix(nrow= 4 , c( 
             0.965744256973267,0.259164363145828,0.0131042888388038,0.399015128612518 , 
             -0.0691364333033562,0.208297848701477,0.975618839263916,0.257272839546204 , 
             0.250116020441055,-0.94310450553894,0.219080179929733,0.0325090624392033 , 
             0,0,0,1 
           )))
  ,zoom=.48)
#rotate manualy and extract hardcoded usermatrix
#par3d(userMatrix=par3d('userMatrix'))
#printMat(par3d('userMatrix'))


# par3d(userMatrix=
#         t(matrix(nrow= 4 , c( 
#           0.966068029403687,0.257725834846497,0.0170262530446053,.1 , 
#           -0.0418718233704567,0.0912240147590637,0.994949638843536,.2 , 
#           0.254871040582657,-0.961902022361755,0.0989200696349144,0 , 
#           0,0,0,1 
#         )))
# )

#export lines as postscript

#rgl.postscript("post.pdf",fmt="eps")


#export surfaces as png
#draw function surface and mirror plane

#rgl.clear()
points3d(expand.grid(list(c(-.2,2.1),c(.5,4),c(1.1,-.1))),col="white")
 surface3d(xseq,seq2(yran),y,col="#000000",alpha=0.1)
 surface3d(c(mirror_x,mirror_x+.0001),yran,c(1,0,1,0),col="#000000",alpha=0.1)

# snapshot3d(file="snap.png")

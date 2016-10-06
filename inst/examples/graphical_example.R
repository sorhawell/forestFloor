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
draw.lines = function(x_points,y_range=c(0,pi),steps=100,fun,fix_y=FALSE,fix_x=FALSE,invertXY=FALSE,...) {
  Args=list(...)
  if(class(fun)=="function") {
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
  } else {
    #if(class=="FFlines") {}
    n_lines= length(x_points)
    for(i in 1:n_lines) {
     Xline = data.frame(
       X1 = rep(x_points[i],steps),
       X2 = seq2(y_range,steps)
     )
     if(invertXY) Xline[] = Xline[2:1]
     pred = predict(fun,Xline)
     do.call(lines3d,
            c(list(
              x = sort(Xline[,1]),
              y = sort(Xline[,2]),
              z = pred),Args)
     )
     
    }
    
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

xran=c(-pi/2,pi/2)
yran=c(0,pi)
mirror_x=-.2


#data points
n_lines=25
set.seed(0)
n_data = 14 #how many line xamples
x_points = sort(jitter(seq(.0,.5,le=n_lines)*pi,amount=.23))[-1]
y_points = ((runif(n_lines,.2,.8))*pi)[-1]

#create 2-way feature grid, hidden function and compute output values
xseq = seq2(xran)
X = as.matrix(expand.grid(xseq,seq2(yran)))
f = function(x,y) (-(x^2)*3-cos(y*8))/3
y = f(X[,1],X[,2])

#points3d(X[,1],X[,2],y,alpha=0.3)
Xtrain = data.frame(X1=runif(5000,xran[1]+.2*pi,xran[2]-.2*pi),
                    X2=runif(5000,yran[1]+.2*pi,yran[2]-.2*pi))
ytrain = f(Xtrain[,1],Xtrain[,2]) +rnorm(5000)/3



library(randomForest)
rf = randomForest(x = Xtrain,y = ytrain,keep.inbag = T,importance=T,ntree=100,sampsize=300)

#surface3d(x=seq2(xran),y=seq2(yran),y,col="red",alpha=0.3)
library(forestFloor)

#ff2 = forestFloor(rf,Xtrain[1,],Xtest)

#draw main structure
xyran = lapply(lapply(list(xran,yran),range),seq2,100)
Xmat = as.data.frame(expand.grid(xyran))  
names(Xmat)=names(Xtrain)
nLines=25
steps=100
ff = forestFloor(rf,Xtrain,Xtest=Xmat)
ff = convolute_ff(ff)
surface3d(x=xyran[[1]],y=xyran[[2]],predict(rf,Xmat),alpha=0.4,col="#002080")
draw.lines(seq2(xran,nLines),yran,steps=steps,fun=rf)  
draw.lines(seq2(yran,nLines),xran,steps=steps,fun=rf,invertXY = T) 



#draw.fc(xyran,rf,Xtrain,FCvar=1,nLines=15,steps=50,Xmat) {
  Xmat = as.data.frame(expand.grid(xyran))  
  names(Xmat)=names(Xtrain)
  center = c(0,3.5,mean(ytrain))  
  surface3d(x=xyran[[1]]+center[1],
            y=xyran[[2]]+center[2],
            ff$FCmatrix[!ff$isTrain,1]+center[3],alpha=0.4,col="#801010")
  ffarray=array(ff$FCmatrix[!ff$isTrain,],dim=c(sapply(xyran,length),ncol(ff$FCmatrix)))
  dim(ffarray)
  FCvar=1
  for(i in 1:25) {
    this_x = seq2(range(xyran[[1]]),25)[i]
    this_y = seq2(range(xyran[[2]]),25)[i]
    xi=which.min(abs(this_x-xyran[[1]]))
    yi=which.min(abs(this_y-xyran[[2]]))
    lines3d(x = this_x             + center[1],
          y = xyran[[2]]         + center[2],
          z = ffarray[xi, ,FCvar] + center[3])
  
    lines3d(x = xyran[[1]]           + center[1],
            y = this_y               + center[2],
            z = ffarray[  ,yi,FCvar] + center[3])
  }
#}





# draw.lines(seq2(xran,25),yran,steps=250,fun=rf)  
# draw.lines(seq2(yran,25),xran,steps=250,fun=rf,invertXY = T) 
surface3d(x=xyran[[1]],y=xyran[[2]]-3.5,ff$FCmatrix[!ff$isTrain,2]+mean(ytrain),alpha=0.4,col="#108010")
center = c(0,-3.5,mean(ytrain))
FCvar=2
for(i in 1:25) {
  this_x = seq2(range(xyran[[1]]),25)[i]
  this_y = seq2(range(xyran[[2]]),25)[i]
  xi=which.min(abs(this_x-xyran[[1]]))
  yi=which.min(abs(this_y-xyran[[2]]))
  lines3d(x = this_x             + center[1],
          y = xyran[[2]]         + center[2],
          z = ffarray[xi, ,FCvar] + center[3])
  
  lines3d(x = xyran[[1]]           + center[1],
          y = this_y               + center[2],
          z = ffarray[  ,yi,FCvar] + center[3])
}

points3d(Xtrain[,1]+7,Xtrain[,2],ytrain,col=fcol(Xtrain),alpha=.4)
points3d(Xtrain[,1]+3 ,Xtrain[,2],rf$predicted,col=fcol(Xtrain),alpha=.4)

#plot X1 FC points plus projection
xs  = Xtrain[,1]+3
x.ind = sort(xs,ind=T)$ix
ys  = Xtrain[,2]+3.5
y.ind = sort(ys,ind=T)$ix
zs  = ff$FCmatrix[ff$isTrain,1]+mean(ytrain)
zhat= filter(x=ff$FCfit[ff$isTrain,1][x.ind]+mean(ytrain), rep(1,100)/100) #moving average for smoothing
col = fcol(Xtrain)
points3d(xs,ys,zs,col=fcol(Xtrain),alpha=.4)
surface3d(range(xs),max(ys)+c(.8,.8001),range(zs)[c(2,2,1,1)],col=rgb(.95,.95,.95))
points3d(xs,max(ys)+.76,zs,col=fcol(Xtrain),alpha=.1)
lines3d(xs[x.ind],max(ys)+.70,zhat[],lwd=1.5)

#plot X2 points plus projection
xs  = Xtrain[,1]+3
x.ind = sort(xs,ind=T)$ix
ys  = Xtrain[,2]-3.5
y.ind = sort(ys,ind=T)$ix
zs  = ff$FCmatrix[ff$isTrain,2]+mean(ytrain)
zhat= filter(x=ff$FCfit[ff$isTrain,2][y.ind]+mean(ytrain), rep(1,100)/100)
col = fcol(Xtrain)
points3d(xs,ys,zs,col=fcol(Xtrain),alpha=.4)
surface3d(max(xs)+c(.8,.8001),range(ys),rep(range(zs),2),col="grey")
points3d(max(xs)+.70,ys,zs,col=fcol(Xtrain),alpha=.1)
lines3d(max(xs)+.70,ys[y.ind],zhat[],lwd=1.5)






# 
# points3d(Xtrain[,1]+7   ,Xtrain[,2],predict(rf,Xtrain),col=fcol(Xtrain))
# ff3 = forestFloor(rf,X=Xtrain[,],Xtest=Xtrain)
# points3d(Xtrain[,1]+7 ,Xtrain[,2]+3.5,ff3$FCmatrix[!ff3$isTrain,1],col=fcol(Xtrain))
# points3d(Xtrain[,1]+7 ,Xtrain[,2]-3.5,ff3$FCmatrix[!ff3$isTrain,2],col=fcol(Xtrain))



# draw.lines(seq2(xran,25),yran,steps=250,fun=rf)  
# draw.lines(seq2(yran,25),xran,steps=250,fun=rf,invertXY = T) 

#ff$FCmatrix[,]

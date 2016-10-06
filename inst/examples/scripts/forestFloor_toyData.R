#forestFloor article code example 1

#one way example
rm(list=ls())
export = FALSE
expPa = "./graphics/artwork/"
pa = getwd()


library(forestFloor)
library(randomForest)
library(rgl)

obs = 5000
vars = 6
noise.factor = .5

X = data.frame(replicate(vars,runif(obs,-1,1)))
Ysignal = with(X,X1^2+.5*sin(2*pi*X2)+X3*X4)

Ytotal = Ysignal + rnorm(obs,sd=sd(Ysignal)*noise.factor)
var(Ysignal)/var(Ytotal)*100
cor(Ytotal,Ysignal)^2
rfo = randomForest(X,Ytotal,keep.inbag=T,ntree=500)
print(rfo) # about 40% varaince explained out of 50% theoretically possible

ff = forestFloor(rfo,X)
Col = fcol(ff,3,RGB=T,order=F,alpha=0.15)
plot(ff,cex=0.1,col=Col,order=F,limitY=T,plot_GOF=T)

if(export) {
  dev.copy(cairo_pdf,file=paste0(expPa,'simplot1.pdf'), width=7, height=3)
  dev.off()
}

par3d(font=3,cex=3)
show3d(ff,3:4,sort=F,col=Col,plot_GOF=T,
           plot.rgl=list(zlab="X3+X4"))#,main=paste("R^2=",R2)))

if(F) {
  rgl.snapshot(file=paste0(expPa,"simplot3d.3.png"))
  rgl.snapshot(file=paste0(expPa,"simplot3d.4.png"))
}

Col = fcol(ff,1:2,order=F)
show3d(ff,1:2,sort=F,col=Col,
           plot.rgl=list(zlab="X1+X2",size=5))#,main=paste("R^2=",R2)))


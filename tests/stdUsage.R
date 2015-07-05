library(forestFloor)
library(randomForest)
#simulate data
obs=2500
vars = 6 

X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 1 * rnorm(obs))


#grow a forest, remeber to include inbag
rfo=randomForest(X,Y,keep.inbag = TRUE,sampsize=500,ntree=100)

#compute topology
ff = forestFloor(rfo,X)


#print forestFloor
print(ff) 

#plot partial functions of most important variables first
plot(ff,order_by_importance=TRUE) 

#Non interacting functions are well displayed, whereas X3 and X4 are not
#by applying different colourgradient, interactions reveal themself 
#also a k-nearest neighbor fit is applied to evaluate goodness of fit
Col=fcol(ff,3,orderByImportance=FALSE)
plot(ff,col=Col,plot_GOF=TRUE) 

#if ever needed, k-nearest neighbor parameters for goodness-of-fit can be access through convolute_ff
#a new fit will be calculated and added to forstFloor object as ff$FCfit
ff = convolute_ff(ff,userArgs.kknn=alist(kernel="epanechnikov",kmax=5))
plot(ff,col=Col,plot_GOF=TRUE)

#in 3D the interaction between X3 and X reveals itself completely
show3d_new(ff,3:4,col=Col,plot.rgl=list(size=5),sortByImportance=FALSE) 

#although no interaction, a joined additive effect of X1 and X2
#colour by FC-component FC1 and FC2 summed
Col = fcol(ff,1:2,orderByImportance=FALSE,X.m=FALSE,RGB=TRUE)
plot(ff,col=Col) 
show3d_new(ff,1:2,col=Col,plot.rgl=list(size=5),sortByImportance=FALSE) 

#...or two-way gradient is formed from FC-component X1 and X2.
Col = fcol(ff,1:2,orderByImportance=FALSE,X.matrix=TRUE,alpha=0.8) 
plot(ff,col=Col) 
show3d_new(ff,1:2,col=Col,plot.rgl=list(size=5),sortByImportance=FALSE)


if(!interactive()) Sys.setenv(RGL_USE_NULL=TRUE) #disable RGL for headless machines
library(forestFloor)
library(randomForest)
#simulate data
obs=2000
vars = 6 

X = data.frame(replicate(vars,rnorm(obs)))
Xtest = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + .5 * rnorm(obs))


#grow a forest, remeber to include inbag
rf42=randomForest(X,Y,keep.inbag = TRUE,sampsize=499,ntree=100)
#compute feature contributions
ff42 = forestFloor(rf42,X,bootstrapFC = TRUE)

#test accuracy of feature contributions
#y_hat_OOB = row sum FC + Y_grandMean
FC.residuals = rf42$predicted - apply(ff42$FCmatrix[ff42$isTrain,],1,sum) - mean(Y)
if(max(abs(FC.residuals))>1E-12) stop(
  paste0("When testing if:  y_hat_OOB = row sum FCmatrix + Y_grandMean
  one/some FCs error exceed allowed 1e-12, found.error=",max(abs(FC.residuals)))
)

#test same results are reached with Xtest
ff43 = forestFloor(rf42,X,Xtest,bootstrapFC = TRUE)
if(max(abs(ff43$FCmatrix[ff43$isTrain,]-ff42$FCmatrix)) > 1E-12) stop(
  "forestFloor with/without Xtest gives different feature contributions"
)



plot(ff42)



#print forestFloor
print(ff42) 

#plot partial functions of most important variables first
plot(ff42,orderByImportance=TRUE) 

#Non interacting functions are well displayed, whereas X3 and X4 are not
#by applying different colourgradient, interactions reveal themself 
#also a k-nearest neighbor fit is applied to evaluate goodness of fit
Col=fcol(ff42,3,orderByImportance=FALSE)
plot(ff42,col=Col,plot_GOF=TRUE,speed=T) 

#if ever needed, k-nearest neighbor parameters for goodness-of-fit can be access through convolute_ff
#a new fit will be calculated and added to forstFloor object as ff42$FCfit
ff42 = convolute_ff(ff42,userArgs.kknn=alist(kernel="epanechnikov",kmax=5))
plot(ff42,col=Col,plot_GOF=TRUE)

#in 3D the interaction between X3 and X reveals itself completely
show3d(ff42,3:4,col=Col,plot.rgl=list(size=5),orderByImportance=FALSE) 

#although no interaction, a joined additive effect of X1 and X2
#colour by FC-component FC1 and FC2 summed
Col = fcol(ff42,1:2,orderByImportance=FALSE,X.m=FALSE,RGB=TRUE)
plot(ff42,col=Col) 
show3d(ff42,1:2,col=Col,plot.rgl=list(size=5),orderByImportance=FALSE) 

#...or two-way gradient is formed from FC-component X1 and X2.
Col = fcol(ff42,1:2,orderByImportance=FALSE,X.matrix=TRUE,alpha=0.8) 
plot(ff42,col=Col) 
show3d(ff42,1:2,col=Col,plot.rgl=list(size=5),orderByImportance=FALSE,plot_GOF=T)


#testing single feature contribution indice plot, implemented from 1.8.7
show3d(ff42,Xi=1:2,
       FCi=1, #only one feature contribution is chosen
       col=Col,plot.rgl=list(size=5),
       orderByImportance=FALSE,plot_GOF=T)



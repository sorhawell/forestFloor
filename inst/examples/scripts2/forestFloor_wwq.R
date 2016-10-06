#script to produce pictures for forestFloor article, wines 3.0

#define environment
rm(list=ls())
library(forestFloor)
library(randomForest)
library(rgl)

#source("./mulitFF_source.R") #get extra functions not in official package
par(mfrow=c(1,1))
expPa = "~/graphics/artwork/"
export=F

#load data
url =  "http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
download.file(url,destfile="winequality-white.csv",mode="w")
wwq = read.csv(file="winequality-white.csv",header=T,sep=";")
save(wwq,file="wwq.rda")
load(file="wwq.rda")
samps = sample(1:dim(wwq)[1])
X = wwq[samps,names(wwq) != "quality"]
y = wwq[samps,"quality"]

#train model
rfo = randomForest(X,y,keep.inbag=TRUE,importance=T)
plot(rfo) ##OOB explained variance, psedo correlation
mean(abs(round(rfo$predicted,9)-y)) #OOB MAD prediction performance
cor(round(rfo$predicted,5),y)^2 #OOB R2 squared pearsons

#compute crossvalidated feature contributions of training set with forestFloor
ff = forestFloor(rfo,X)

## 1: plot main effects + colour gradient to learn interactions with "alcohol"
#add fit of main effects with k-nearest neighbor, 50 neighbors chosen manually
#ff = convolute_ff(ff,userArgs.kknn=list(kmax=50)) #saved in ff$FCfit
Col1=fcol(ff,1,alpha=.05,order=T)#apply colour gradient by values of most important variable (alcohol)
plot(ff,col=Col1,plot_GOF=T,cropXaxes=c(1:11)) #plot one-way main effects, crop outliers
if(export) { #export as vector graphics
  dev.copy(cairo_pdf,file=paste0(expPa,'simplot4.pdf'), width=7, height=9)
  dev.off()
}


## 2: Use 3Dplot to investigate inteaction between volatile acid and alcohol 
par3d(cex=2)
show3d(ff,c(1,3),c(3,3),col=Col1,limit=7,plot.rgl=list(box=F,size=8))
decorate3d(add=T,xlab="",ylab="",zlab="",axes=F)
if(export) rgl.snapshot(file=paste0(expPa,'wwq.volatile.png'), fmt = "png")

##check that residual.sugar, density and alcohol only have to degrees of freedom
summary(princomp(scale(data.frame(X$residual,X$dens,X$alc))))


#check if a linear model with sqaured terms do out-perform RF?
#it does not
library(foreach)
folds = split(sample(1:length(y)),1:5)
test.preds = foreach(i = folds,.combine=cbind) %do% {
  Data.train = data.frame(X=X[-i,],X2=X[-i,]^.5,Y=y[-i])
  Data.test  = data.frame(X=X[i ,],X2=X[i ,]^.5,Y=y[ i])
  lmf = lm(Y~.,Data.train)  
  test.pred = rep(0,length(y))
  test.pred[i] = predict(lmf,Data.test)
  return(test.pred)
}
test.preds = apply(test.preds,1,sum)
cat(cor(test.preds,y)^2)
cat(mean(abs(round(test.preds-y,0))))
plot(rfo$pred,y,col="#00000010")

#compare performance with Paulo Cortez SVM, using 3-fold CV
folds = split(sample(1:length(y)),1:3)
library(foreach)
test.preds = foreach(i = folds,.combine=cbind) %do% {
  Data.train = data.frame(X=X[-i,],Y=y[-i])
  Data.test  = data.frame(X=X[i ,],Y=y[ i])
  RF = randomForest(Y~.,Data.train)  
  test.pred = rep(0,length(y))
  test.pred[i] = predict(RF,Data.test)
  return(test.pred)
}
cor(apply(test.preds,1,sum),y)^2
mean(abs(apply(test.preds,1,sum)-y))

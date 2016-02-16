if(!interactive()) Sys.setenv(RGL_USE_NULL=TRUE) #disable RGL for headless machines
library(randomForest)
library(forestFloor)
require(utils)

data(iris)
iris
X = iris[,!names(iris) %in% "Species"]
Y = iris[,"Species"]
as.numeric(Y)
rf.42 = randomForest(X,Y,keep.forest=T,replace=F,keep.inbag=T,samp=15,ntree=100)
ff.42 = forestFloor(rf.42,X,calc_np = F,bootstrapFC = TRUE)

#test accuracy of feature contributions
#y_hat_OOB = row sum FC + Y_grandMean
FCc = t(t(apply(ff.42$FCarray,c(1,3),sum))+as.vector(table(Y)/length(Y)))
FC.residuals = FCc-predict(rf.42,type="prob")
if(max(abs(FC.residuals))>1E-12) stop(
  paste0("When testing if:  y_hat_OOB = row sum FCmatrix + Y_grandMean
         one/some FCs error exceeds allowed 1e-12, found.error=",max(abs(FC.residuals)))
)

Xtest = iris[1:50,] #copy
Xtest = Xtest[,-5] #drop Species
Xtest[1:4] = lapply(iris[1:4],sample,50) #random resample 50 samples


#test same results are reached with Xtest
ff.43 = forestFloor(rf.42,X,Xtest,bootstrapFC = TRUE)
if(max(abs(ff.43$FCarray[ff.43$isTrain,,]-ff.42$FCarray)) > 1E-12) stop(
  "forestFloor with/without Xtest gives different feature contributions"
)


pred = sapply(1:3,function(i) apply(ff.42$FCarray[,,i],1,sum))+1/3
rfPred = predict(rf.42,type="vote",norm.votes=T)
rfPred[is.nan(rfPred)] = 1/3
if(cor(as.vector(rfPred),as.vector(pred))^2<0.99) stop("fail testMultiClass")
attributes(ff.42)
args(forestFloor:::plot.forestFloor_multiClass)
plot(ff.42,plot_GOF=T,cex=.7,
     colLists=list("#FF0000A5",
                   "#00FF0050",
                   "#0000FF35")
     )

#try to alter std par
plot(ff.42,plot_GOF=T,cex=.7,
     colLists=list("#FF0000A5",
                   "#00FF0050",
                   "#0000FF35"),
     mfrow=c(4,3)
)


show3d(ff.42,1:2,1:2,plot_GOF=T)
show3d(ff.42,1:2,1,plot_GOF=T)#test plotting only one feature contribution


#plot all effect 2D only
pars = plot_simplex3(ff.42,Xi=c(1:3),restore_par=F,zoom.fit=NULL,var.col=NULL,fig.cols=2,fig.rows=1,
               fig3d=F,includeTotal=T,auto.alpha=.4,set_pars=T)
pars = plot_simplex3(ff.42,Xi=0,restore_par=F,zoom.fit=NULL,var.col=alist(alpha=.3,cols=1:4),
               fig3d=F,includeTotal=T,auto.alpha=.8,set_pars=F)



for (I in ff.42$imp_ind[1:4])  {
  #plotting partial OOB-CV separation(including interactions effects), coloured by true class
  pars = plot_simplex3(ff.42,Xi=I,restore_par=F,zoom.fit=NULL,var.col=NULL,fig.cols=4,fig.rows=2,
                 fig3d=F,includeTotal=F,label.col=1:3,auto.alpha=.3,set_pars = (I==ff.42$imp_ind[1]))
  #plotting partial OOB-CV separation(including interactinos effects), coloured by varaible value
  pars = plot_simplex3(ff.42,Xi=I,restore_par=F,zoom.fit=T,var.col=alist(order=F,alpha=.8),
                 fig3d=F,includeTotal=(I==4),auto.alpha=.3,set_pars=F)
}




##more plots
rm(list=ls())
library(randomForest)
library(forestFloor)
#simulate data
obs=2500
vars = 6
X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 1 * rnorm(obs))
Y.class <- Y
Y.class[Y<=-1] <- 1
Y.class[Y>1] <- 2
Y.class[Y.class<1] <- 3
Y.class <- factor(Y.class)
#grow a forest, remember to include inbag

rfo=randomForest(X,Y.class,keep.inbag = TRUE,sampsize=1500,ntree=500)

#compute topology
ff = forestFloor(rfo,X)

#plot
Col = fcol(ff,1,alpha=.25,orderByImportance=FALSE) #farve bruges her bedst til kende forskel på klasser

plot(ff,
     plot_GOF=T,
     colLists=list("#00000010",
                   "#FF000010",
                   "#00FF0010"),
     GOF_args = alist(col = c("#3A3A3A80","#FF3A3A80","#3AFF3A80")[i]))

plot(ff,plot_seq=1:6,
     colLists=list("#00000002", #farve vector til class1 (length 1 eller 2500)
                   "#00000002", #farve vector til class2 (length 1 eller 2500)
                   Col) #tredje  farves med fcol (length 1 eller 2500)
     ,plot_GOF=T)



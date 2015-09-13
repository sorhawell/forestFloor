if(!interactive()) Sys.setenv(RGL_USE_NULL=TRUE) #disable RGL for headless machines
library(randomForest)
library(forestFloor)
require(utils)

data(iris)
iris
X = iris[,!names(iris) %in% "Species"]
Y = iris[,"Species"]
as.numeric(Y)
rf.test42 = randomForest(X,Y,keep.forest=T,replace=F,keep.inbag=T,samp=15,ntree=100)
ff.test42 = forestFloor(rf.test42,X,calc_np = F)

pred = sapply(1:3,function(i) apply(ff.test42$FCarray[,,i],1,sum))+1/3
rfPred = predict(rf.test42,type="vote",norm.votes=T)
rfPred[is.nan(rfPred)] = 1/3
if(cor(as.vector(rfPred),as.vector(pred))^2<0.99) stop("fail testMultiClass")
attributes(ff.test42)
args(forestFloor:::plot.forestFloor_multiClass)
plot(ff.test42,plot_GOF=T,cex=.7,
     colLists=list(c("#FF0000A5"),
                   c("#00FF0050"),
                   c("#0000FF35")))

show3d(ff.test42,1:2,3:4,plot_GOF=T)
#plot all effect 2D only
pars = plot_simplex3(ff.test42,Xi=c(1:3),restore_par=F,zoom.fit=NULL,var.col=NULL,fig.cols=2,fig.rows=1,
               fig3d=F,includeTotal=T,auto.alpha=.4,set_pars=T)
pars = plot_simplex3(ff.test42,Xi=0,restore_par=F,zoom.fit=NULL,var.col=alist(alpha=.3,cols=1:4),
               fig3d=F,includeTotal=T,auto.alpha=.8,set_pars=F)



for (I in ff.test42$imp_ind[1:4])  {
  #plotting partial OOB-CV separation(including interactions effects), coloured by true class
  pars = plot_simplex3(ff.test42,Xi=I,restore_par=F,zoom.fit=NULL,var.col=NULL,fig.cols=4,fig.rows=2,
                 fig3d=F,includeTotal=F,label.col=1:3,auto.alpha=.3,set_pars = (I==ff.test42$imp_ind[1]))
  #plotting partial OOB-CV separation(including interactinos effects), coloured by varaible value
  pars = plot_simplex3(ff.test42,Xi=I,restore_par=F,zoom.fit=T,var.col=alist(order=F,alpha=.8),
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
     GOF_col=c("#3A3A3A80",
               "#FF3A3A80",
               "#3AFF3A80"))

plot(ff,plot_seq=1:6,
     colLists=list("#00000002", #farve vector til class1 (length 1 eller 2500)
                   "#00000002", #farve vector til class2 (length 1 eller 2500)
                   Col) #tredje  farves med fcol (length 1 eller 2500)
     ,plot_GOF=T)

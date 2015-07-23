library(randomForest)
library(forestFloor)
require(utils)

data(iris)
X = iris[1:100,!names(iris) %in% "Species"]
Y = iris[1:100,"Species"]
Y = Y[,drop=T]
Y
#test randomForest binary classification, treated as binary classification
rf = randomForest(X,Y,keep.forest=T,replace=F,keep.inbag=T,samp=66,ntree=500)
ff = forestFloor(rf,X,T,F)
nLevels = length(levels(Y))
pred = sapply(1:nLevels,function(i) apply(ff$FCarray[,,i],1,sum))+1/nLevels
rfPred = predict(rf,type="vote",norm.votes=T)
rfPred[is.nan(rfPred)] = 1/nLevels
if(cor(as.vector(rfPred),as.vector(pred))^2<0.99) stop("fail accuracy binaryMultiClass")
attributes(ff)
args(forestFloor:::plot.forestFloor_multiClass)
plot(ff,compute_GOF=T,plot_GOF=T,cex=.7,
     colLists=list(c("#FF0000A5"),
                   c("#00FF0050"),
                   c("#0000FF35")))
show3d(ff,1:2,3:4)


#test with trimTrees::cinbag binary classification treat as the same
library(trimTrees)
rf = cinbag(X,Y,keep.forest=T,replace=T,keep.inbag=T,samp=100,ntree=500)
ff = forestFloor(rf,X,T,F)
nLevels = length(levels(Y))
rfPred = predict(rf,type="vote",norm.votes=T)
rfPred[is.nan(rfPred)] = 1/nLevels
if(cor(as.vector(rfPred),as.vector(pred))^2<0.99) stop("fail accuracy binaryMultiClass")
attributes(ff)
args(forestFloor:::plot.forestFloor_multiClass)
plot(ff,compute_GOF=T,plot_GOF=T,cex=.7,
     colLists=list(c("#FF0000A5"),
                   c("#00FF0050"),
                   c("#0000FF35")))
show3d(ff,1:2,3:4)


##test randomForest binary classification treated as, regression
rf = cinbag(X,Y,keep.forest=T,replace=T,keep.inbag=T,samp=100,ntree=500)
ff = forestFloor(rf,X,T,T)
rf = cinbag(X,Y,keep.forest=T,replace=T,keep.inbag=T,samp=100,ntree=500)
ff = forestFloor(rf,X,T,T)
plot(ff,col=Y)
show3d(ff,col=as.numeric(Y))



# #plot all effect 2D only
# pars = plot_K3(ff,Xvars=0,restore_par=F,zoom.fit=NULL,var.col=NULL,fig.cols=2,fig.rows=1,
#                fig3d=F,includeTotal=T,auto.alpha=.4,set_pars=T)
# pars = plot_K3(ff,Xvars=0,restore_par=F,zoom.fit=NULL,var.col=alist(alpha=.3,cols=1:4),
#                fig3d=F,includeTotal=T,auto.alpha=.8,set_pars=F)
# 
# for (I in ff$imp_ind[1:4])  {
#   
#   pars = plot_K3(ff,Xvars=I,restore_par=F,zoom.fit=T,var.col=NULL,
#                  fig3d=F,includeTotal=F,label.col=1:3,auto.alpha=.1,set_pars = F)
#   pars = plot_K3(ff,Xvars=I,restore_par=F,zoom.fit=T,var.col=alist(order=F,alpha=.1),
#                  fig3d=F,includeTotal=(I==4),auto.alpha=.1,set_pars=F)
# }
# 
# 
# par(mfrow=c(2,2))
# out = plot.ffmc(ff,plot.fit=T,fig.columns=2,
#                 order=T,cex=0.7,compute_gof=T,
#                 colLists=list("#000000A5",
#                               "#FF000050",
#                               "#00FF0035"))

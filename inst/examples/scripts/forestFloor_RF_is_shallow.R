#code for appendix, testing what order of interaction randomForest can captivate
rm(list=ls())
library(forestFloor)
library(doParallel)
library(rgl)

cl  = makeCluster(max(detectCores()-2,1))
registerDoParallel(cl)


#define simulation function
simThis = function(ite.par,hidden.y,distr=rnorm) {  
  ite.par.matrix = expand.grid(ite.par)
  ite.par.list = apply(ite.par.matrix,1,as.list)
  results = foreach(pars = ite.par.list,.combine=c,.packages="randomForest") %dopar% {
    out = with(pars,{
      replicate(max(floor(12000/ite.obs),1),{  #data sets of fewer observations will be replivated
      #data sets of N<=6000 will be run two times etc.
        #draw uncorrelated variables from a distribution
        X = data.frame(replicate(ite.vars,distr(ite.obs))) 
        #define target by hidden function
        y = hidden.y(X,ite.vars)
        if(length(unique(X)<3)) X = sapply(X,as.factor)
        rfo = randomForest(x=X,y=y,ntree=500)$rsq[500] #train forest
      })
    })
    out = mean(out)
    gc() #delete
    return(out)
  }
  attr(results,"ite.par") = ite.par
  results
}

show.out = function(avg.res,main="title") {
  ite.par = attr(avg.res,"ite.par")
  ma.res = matrix(avg.res,nrow=length(ite.par[[1]]))
  j=length(ite.par[[1]])
  for(i in 1:j) {
    Col = rgb(j-i,i-1,1,max=j-1)
    if(i==1) plot(y=ma.res[i,],x=ite.par[[2]],log="x",main=main,
                  type="l",col=Col,ylim=range(ma.res),
                  xlab="N train samples",
                  ylab="Out-of-bag, explained variance R2") else 
                    points(y=ma.res[i,],x=ite.par[[2]],col=Col,type="l")
  }
}




par(mfrow=c(1,3))
saddle.out = simThis(ite.par = list(ite.vars=8:1,ite.obs=10^seq(4,2,le=10)),
             hidden.y = function(X,ite.vars) if(ite.vars>1) apply(X,1,prod) else X[,1],
             distr=function(n) runif(n,-1,1))
show.out(saddle.out,"saddle")

sineprod.out = simThis(ite.par = list(ite.vars=8:1,ite.obs=10^seq(4,2,le=10)),
                     hidden.y = function(X,ite.vars) {
                      if(ite.vars>1) apply(X,1,function(x) prod(sin(x))) else sin(X[,1])
                     },
                    distr=function(n) runif(n,-pi/2,pi/2))
show.out(sineprod.out,"sineProd")

stepprod.out = simThis(ite.par = list(ite.vars=8:1,ite.obs=10^seq(4,2,le=10)),
                       hidden.y = function(X,ite.vars) {
                         if(ite.vars>1) apply(X,1,function(x) prod(x)) else X[,1]
                       },
                       distr = function(n) sample(c(-1,1),n,replace=T))
show.out(stepprod.out,"binaryProd")



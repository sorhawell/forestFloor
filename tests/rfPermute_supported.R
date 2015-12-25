# ##This test check if rfPermute is supported
# if(!interactive()) Sys.setenv(RGL_USE_NULL=TRUE) #disable RGL for headless machines
# library(forestFloor)
# library(randomForest)
# rm(list=ls())
# library(rfPermute)
# 
# #regression
# obs=300
# vars=6
# X = data.frame(replicate(vars,rnorm(obs)))
# y = with(X,-X1^2+2*sin(X2*pi))
# rf = rfPermute(X,y,nrep=2,ntree=50,
#                xtest = data.frame(replicate(vars,rnorm(obs))),
#                ytest=rep(1,300),
#                keep.inbag=T,  #mandatory
#                importance=T,  #recommended, otherwise ordering are more random
#                replace=F,     #mandatory for classification
#                keep.forest=T) #mandatory for rfImpute regression
# 
# print(class(rf))
# ff = forestFloor(rf,X)
# Col=fcol(ff,2,X.m=F,orderByImportance = FALSE) #some colours
# plot(ff,col=Col)
# show3d(ff,col=Col)
# 
# #classification
# y.class = factor(y>median(y))
# rf.class = rfPermute(X,y.class,nrep=2,ntree=50,
#                keep.inbag=T,  #mandatory
#                importance=T,  #recommended, otherwise ordering are more random
#                replace=F)        #mandatory for classification,
# ff.class = forestFloor(rf.class,X)
# Col=fcol(ff.class,2,X.m=T,orderByImportance = FALSE) #some colours
# plot(ff.class,col=Col)
# show3d(ff.class,col=Col)

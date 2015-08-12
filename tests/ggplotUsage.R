if(!interactive()) Sys.setenv(RGL_USE_NULL=F) #disable RGL for headless machines
##testing ggplot functionality
rm(list=ls())


library(forestFloor)
library(randomForest)
#library(gridExtra)
#simulate data
obs=1000
vars = 18
X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 1 * rnorm(obs))
#grow a forest, remeber to include inbag
rfo=randomForest(X,Y,keep.inbag = TRUE,sampsize=250,ntree=50)
#compute topology
ff = forestFloor(rfo,X)
#plot(ff,1:9,col=fcol(ff))
ggPlotForestFloor(ff,1:9)

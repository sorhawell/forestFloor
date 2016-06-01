library(randomForest);library(forestFloor)
#simulate data
X     = data.frame(replicate(6,4*(runif(3000)-.5)))
Xtest = data.frame(replicate(6,4*(runif(1500)-.5)))
y     = with(X,X1^2+sin(X2*2*pi)+X3*X4) + rnorm(3000)/3
ytest = with(Xtest,X1^2+sin(X2*6*pi)+X3*X4) + rnorm(3000)/3

#define boosted tree wrapper
simpleBoost = function(
  X,y,    #training data
  M=100,  #boosting iterations and ntrees
  v=.1,   #learning rate
  ...) {  #other parameters passed to randomForest
  y_hat = y * 0  #latest ensemble prediction
  res_hat = 0    #residuals hereof...
  Fx = list()    #list for trees
  for(m in 1:M) {
    y_hat = y_hat + res_hat * v  #update prediction, by learning rate
    res = y - y_hat              #compute residuals
    hx = randomForest(X,res,ntree=1,keep.inbag=T,...) #grow tree on residuals
    res_hat = predict(hx,X)                           #predict residuals
    cat("SD=",sd(res),  "\n")    #print
    hx$forest$nodepred = hx$forest$nodepred * v #multiply nodepredictions by learning rate
    Fx[[m]] = hx  #append tree to forest
  }
  Fx = do.call(combine,Fx) #combine trees with randomForest::combine()
  Fx$y = y #append y
  Fx$oob.times = apply(Fx$inbag,1,function(x) sum(!x)) #update oob.times
  class(Fx) = c("simpleBoost","randomForest") #make simpleBoost a subclass of randomForest
  return(Fx)
}

predict.simpleBoost = function(Fx,X) {
  class(Fx) = "randomForest"
  predMatrix = predict(Fx,X,predict.all = T)$individual
  ntrees = dim(predMatrix)[2]
  return(apply(predMatrix,1,sum))
}

plot.simpleBoost = function(Fx,X,ytest,add=F,...) { #plots learning curve
  class(Fx) = "randomForest"
  predMatrix = predict(Fx,X,predict.all = T)$individual
  ntrees = dim(predMatrix)[2]
  allPreds = apply(predMatrix,1,cumsum)
  preds = apply(allPreds,1,function(pred) sd(ytest-pred))
  if(add) plot=points
  plot(1:ntrees,preds,...)
  return()
}

#build gradient boosted forest
rb = simpleBoost(X,y,M=300,replace=F,mtry=6,sampsize=500,v=0.005)

#make forestFloor plots
ffb = forestFloor(rb,X,Xtest)
#correct for that tree votes of gradient boosts are summed, not averaged.
#forestFloor will as default divide by the same number as here multiplied with
ffb$FCmatrix = ffb$FCmatrix * c(rb$oob.times,rep(rb$ntree,sum(!ffb$isTrain)))

#plot forestFloor for OOB-CV feature contributions and regular feature contributions
plot(ffb,plotTest=T,col=fcol(ffb,3,plotTest = TRUE))
plot(ffb,plotTest=F,col=fcol(ffb,1,plotTest = FALSE))

#validate model structure
pred = predict(rb,X)
predtest = predict(rb,Xtest)
plot(y,pred,col="#00000034")
plot(rb,Xtest,ytest,log="x")
vec.plot(rb,X,i.var=1:2)

#export plot
png(file = "ffGradientBoost.png", bg = "transparent",width=800,height = 500)
plot(ffb,plotTest=T,col=fcol(ffb,1))
rect(1, 5, 3, 7, col = "white")
dev.off()
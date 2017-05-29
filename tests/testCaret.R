##test caret suuport

rm(list=ls())
library(caret)
library(e1071) #caret is quietly  requiring this package...
library(forestFloor)
library(randomForest)
N = 1000
vars = 15
noise_factor = .3
bankruptcy_baserate = 0.2
X = data.frame(replicate(vars,rnorm(N)))
y.signal = with(X,X1^2+X2^2+X3*X4+X5+X6^3+sin(X7*pi)*2) #some non-linear f
y.noise = rnorm(N) * sd(y.signal) * noise_factor
y.total = y.noise+y.signal
y = factor(y.total>=quantile(y.total,1-bankruptcy_baserate))

set.seed(1)
caret_train_obj <- train(
  x = X, y = y, 
  method = "rf",
  keep.inbag = TRUE,  #always set keep.inbag=TRUE passed as ... parameter to randomForest
  ntree=50 #speed up this example, if set too low, forestFloor will fail
)

rf = caret_train_obj$finalModel #extract model
if(!all(rf$y==y)) warning("seems like training set have been resampled, using smote?")
ff = forestFloor(rf,X,binary_reg = T)

#... or simply pass train
ff = forestFloor(caret_train_obj,X,binary_reg = T)
plot(ff,1:6,plot_GOF = TRUE)

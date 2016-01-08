library(ranger)
library(forestFloor)
library(randomForest)

#some regression problem
Data = data.frame(replicate(4,rnorm(2500)))
Data$y = with(Data, X1^2 + sin(X2*2*pi) + X3*X4)

#train model
ra = ranger(y~.,data=Data,keep.inbag = TRUE, write.forest = TRUE,num.trees =1000)
rf = ranger_RFadaptor(ra,Data$y)
ff = ffor(rf,Data[,-5],calc_np=T)
plot(ff)
show3d(ff,3:4,col=fcol(Data,3:4))

rf2=randomForest(Data[,-5],Data[,5],ntree=1000, keep.inbag = T)
ff2 = ffor(rf2,Data[,-5],calc_np=T)
plot(ff2)
show3d(ff2,3:4,col=fcol(Data,3:4))


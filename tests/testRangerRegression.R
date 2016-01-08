# library(ranger)
# library(forestFloor)
# library(randomForest)
# library(rgl)
# #some regression problem
# Data = data.frame(replicate(4,rnorm(1500)))
# Data$y = with(Data, X3*X4)
# 
# #train model
# ra = ranger(y~.,data=Data,keep.inbag = TRUE, write.forest = TRUE,num.trees =1000)
# rf = ranger_RFadaptor(ra,Data$y)
# ff = forestFloor(rf,Data[,-5],calc_np=T)
# plot(ff)
# show3d(ff,3:4,col=fcol(Data,3:4))
# 
# rf2 = randomForest(Data[,-5],Data[,5],ntree=1000, keep.inbag = T,mtry=2)
# ff2 = forestFloor(rf2,Data[,-5],calc_np=F)
# plot(ff2)
# open3d()
# show3d(ff2,1:2,col=fcol(Data,3:4))


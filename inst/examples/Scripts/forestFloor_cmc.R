#Soren H Welling Sepember 2015

#this script for forestFloor article computes and visualize multiclass forestFloor
#the script depends on the rfFC(rForge) implementation of feature contributions
#feature contributions are computed with nFold cross validations
library(forestFloor)
library(randomForest)


rm(list=ls())
expPa = "./graphics/artwork/"

url =  "http://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data"
download.file(url,destfile="cmcFile.txt",mode="w")
cmc = read.csv(file="cmcFile.txt",header=FALSE)

names(cmc) =  c("W age","W education","H education", "n children", "W religion","W working","H occupation",
                    "standardOfLiving","mediaExposure","Contraceptive method")

# #make these as factors
# for(i in which(names(cmc) %in% c("mediaExposure","Contraceptive method","W religion","W working"))) {
#   cmc[,i] = factor(cmc[,i])
# }

X = cmc[,!names(cmc) %in% "Contraceptive method"]
sapply(X,class)
y = factor(cmc[,"Contraceptive method"])

rfo = randomForest(X,y,ntree=1500,mtry=2,sampsize=400,replace=F,
                   keep.forest=T,keep.inbag=T,importance=T)
ffmc = forestFloor(rfo,X)

#plot all effect 2D only
pars = plot_simplex3(ffmc,Xi=0,restore_par=F,zoom.fit=NULL,var.col=NULL,fig.cols=4,fig.rows=5,
               fig3d=F,includeTotal=T,auto.alpha=.4,set_pars=T)
pars = plot_simplex3(ffmc,Xi=0,restore_par=F,zoom.fit=NULL,var.col=alist(alpha=.1,cols=1:9),
               fig3d=F,includeTotal=T,auto.alpha=.8,set_pars=F)
for (I in ffmc$imp_ind[1:9])  {
  pars = plot_simplex3(ffmc,Xi=I,restore_par=F,zoom.fit=T,var.col=NULL,
         fig3d=F,includeTotal=F,label.col=1:3,auto.alpha=.4,set_pars = F)
  pars = plot_simplex3(ffmc,Xi=I,restore_par=F,zoom.fit=T,var.col=alist(order=F,alpha=.1),
         fig3d=F,includeTotal=(I==9),auto.alpha=.4,set_pars=F)
}


for (I in c(1,2,4))  {
  pars = plot_simplex3(ffmc,Xi=I,fig.cols=3,fig.rows=3,restore_par=F,zoom.fit=T,
                 var.col=alist(order=F,alpha=.1,cols=1),
                 fig3d=F,includeTotal=(I==9),auto.alpha=.8,set_pars=(I==1))
  pars = plot_simplex3(ffmc,Xi=I,restore_par=F,zoom.fit=T,var.col=alist(order=F,alpha=.1,cols=2),
                 fig3d=F,includeTotal=(I==9),auto.alpha=.8,set_pars=F)
  pars = plot_simplex3(ffmc,Xi=I,restore_par=F,zoom.fit=T,var.col=alist(order=F,alpha=.1,cols=4),
                 fig3d=F,includeTotal=(I==9),auto.alpha=.8,set_pars=F)
}



out = plot(ffmc,jitter_these_cols=c(1:9),jitter.factor=0.8,
           plot_GOF=T,order=T,cex=0.3,
           colLists=list("#00000025","#FF000020","#00FF0015"))
  

# par3d(useFreeType=T,font=1,cex="2")
# show3d(ffmc,Xi=c(4,2),
#             FCvars=c(4,2),
#             label.seq=1:3,
#             user.rgl=list(xlab="W. Education",ylab="N Children",zlab="change of probality for W. Education",
#                           size=0.01),
#             user.grid=list(userArgs.kknn=list(k=15),grid=20),
#             compute_gof=T,
#             user.gof=list(kmax=15))
# rgl.snapshot(paste0(expPa = paste0(expPa,"cmc.2ndAlign.1.png")))
# rgl.snapshot(paste0(expPa = paste0(expPa,"cmc.2ndAlign.2.png")))



#this file is old and will be deleted soon


# ##3d plot of forestFloor_multiClass
# show3d.forestFloor_multiClass = function(x,Xi,FCi=NULL,label.seq=NULL,user.grid.args=list(NULL),user.rgl.args=list(),
#                       compute_GOF=FALSE,user.gof.args=list(NULL)) {
#   if(class(x)!="forestFloor_multiClass") stop("class(x) != forestFloor_multiClass")
#   if(is.null(FCi)) FCi = Xi
#   if(is.null(label.seq)) label.seq = 1:min(8,length(levels(x$Y)))
#   
#   #compute mean goodness of fit of label surfaces of 3d-plot
#   #gof is the squared pearson correlation of any FC and fitted surface
#   if(compute_GOF) {
#     fits = lapply(label.seq, function(label.ind) {
#       forestFloor_obj = list(FCmatrix = x$FCarray[,,label.ind],
#                              X=apply(x$X[,],2,as.numeric.factor)
#       )
#       class(forestFloor_obj)="forestFloor_multiClass"
#       convolute_ff2(forestFloor_obj,
#                     Xi=Xi,
#                     FCi=FCi,
#                     userArgs.kknn=user.gof.args)
#     })
#     label_gofs =sapply(label.seq,function(label.ind) {
#       joinedFC = if(length(FCi)>1) {
#         apply(x$FCarray[,FCi,label.ind],1,sum)
#       } else {
#         x$FCarray[,FCi,label.ind]
#       }
#       #       plot(fits[[label.ind]],joinedFC)
#       #       plot(fits[[label.ind]],fits[[label.ind]]-joinedFC)
#       cor(fits[[label.ind]],joinedFC)^2
#     })
#     mean_gof = round(mean(label_gofs),digits=2)
#   }
#   
#   
#   with(x, {
#     for(i in label.seq) {
#       if(length(FCi)>1) FCcombined = apply(FCarray[,FCi,i],1,sum) else FCcombined = FCarray[,FCi,i]
#       
#       
#       
#       
#       
#       std.rgl.args = list(X[,Xi[1]],
#                           X[,Xi[2]],
#                           FCcombined,
#                           add = {if(i==label.seq[1]) F else T},
#                           col=(i)^((i==as.numeric(Y))*1),
#                           alpha=1-.8*(i!=as.numeric(Y)),
#                           type=if(length(label.seq)*dim(X)[1] <500) "s" else "p",
#                           size=if(length(label.seq)*dim(X)[1] <500) 1 else 3,
#                           main = if(compute_GOF) paste0("R^2=",mean_gof) else "",
#                           xlab = names(x$X)[Xi[1]],
#                           ylab = names(x$X)[Xi[2]],
#                           zlab = if(length(FCi==1)) names(x$X)[FCi] else "joined FC"
#       )
#       run.args = append.overwrite.alists(user.rgl.args,std.rgl.args)
#       do.call(plot3d,run.args)
#       
#       ffpar = list(FCmatrix=FCarray[,,i],X=X)
#       class(ffpar) = "forestFloor_multiClass"
#       
#       #merge user arguments for grid estimation with default arguments and estimate...
#       default.grid.args = alist(ff=ffpar,Xi=Xi,FCi=FCi,zoom=1,
#                                 grid=25,userArgs.kknn=alist(k=10))
#       run.args = append.overwrite.alists(user.grid.args,default.grid.args)
#       Spar = do.call(convolute_grid,run.args)
#       
#       #draw grid
#       persp3d(unique(Spar[,2]),
#               unique(Spar[,3]),
#               Spar[,1],
#               alpha=0.15,
#               col=i,
#               add=T)
#     }
#   })
# }
#F 3.2 : Tiltle
#



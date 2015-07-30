
#2dplot of forstFloor_multiClass
plot.forestFloor_multiClass  = function(
  x,
  plot_seq=NULL,
  label.seq=NULL,
  limitY=TRUE,
  colLists = NULL,
  order_by_importance=TRUE,
  fig.columns = NULL,
  plot_GOF=FALSE,
  GOF_col=NULL,
  speedup_GOF = TRUE,
  jitter_these_cols = NULL,
  jitter.factor = NULL,
  compute_GOF = F,
  ...) {
  void = with(x,{ #inside x object
    
    if(is.null(plot_seq))         plot_seq = 1:min(18,dim(X)[2])
    fig.columns = if(is.null(fig.columns)) {
      if(length(plot_seq)<=4) c(1,2,3,2)[length(plot_seq)] else 3
    }
    if(order_by_importance) plot_seq = x$imp_ind[plot_seq]
    if(is.null(label.seq)) label.seq = 1:min(8,length(levels(Y)))
    if(is.null(colLists)) colLists =
      lapply(1:length(label.seq), function(i) factor(rep(i,dim(X)[1])))
    if(plot_GOF && is.null(GOF_col)) GOF_col = sapply(colLists,function(x) x[1])
    
    n.plots = min(dim(X)[2],length(plot_seq))
    plotdims.y = min(ceiling(n.plots/fig.columns),5)
    plotdims.x = min(fig.columns , n.plots)
    par(mfrow=c(plotdims.y,plotdims.x),mar = c(2,2,3,1))
    
    
    if(plot_GOF) {
      if(speedup_GOF) { #reduce observatoins to compute forest GOF faster
        obs = length(x$Y)
        reduce.exp = 1 - 0.025  * sum(obs>c(500,700,1000,1500,2500,4500,10000,20000))
        x = with(x,{ #this is a little tricky, global object is overwritten with local
                     #reduced version if speedup true. Not reduced content of x still
                     #available locally as with(x, have been called once before
          subSample=sample(obs,ceiling(obs^reduce.exp))
          X=X[subSample,]
          Y=Y[subSample]
          FCarray=FCarray[subSample,,]
          mget(ls())
        })
      }
      
      fits = lapply(label.seq,function(label.ind) {
        ff2 = convolute_ff(list(FCmatrix = x$FCarray[,,label.ind],
                                X=apply(x$X[,],2,as.numeric.factor,),
                                Xind = apply(x$X,2,function(xcol) { 
                                  sort(xcol,index.return=TRUE)$ix})),
                           these.vars=sort(plot_seq))
      })
      
      fit.seq = (1:length(label.seq)) %in% 1:length(Y)
    }  else fits = invisible()
    
    
    
    for(j in plot_seq)
      for(i in label.seq) {
        X.jittered = if(j %in% jitter_these_cols) jitter(X[,j],jitter.factor) else X[,j]
        if(i==1) {
          plot(X.jittered,
               FCarray[,j,i],
               col=colLists[[i]],
               ...,
               ylim=range(FCarray),
               ylab= " ",
               xlab = paste0("X[ ,",j,"]"),
               #if fit is plotted goodness of fit is added to title
               main = paste(names(X)[j],if(plot_GOF) {paste("R^2=",round(mean(sapply(label.seq,function(lSeq) {
                 #plot(fits[[lSeq]]$FCfit[,j],FCarray[,j,lSeq],col=4)
                 cor( fits[[lSeq]]$FCfit[,j],x$FCarray[,j,lSeq])^2
               }))
               ,digits=2))
               
               } else "")
          )
        } else {
          points(X.jittered,FCarray[,j,i],col=colLists[[i]],...)
        }
        if(plot_GOF) with(fits[[i]],{
          points(X[Xind[,j],j],FCfit[Xind[,j],j],col=GOF_col[i],cex=0.05,type="l",lwd=5) 
        })
      }
    return(fits)
  })
}

#globalVariables("FC","X")
##experimental function to forestFloor 2d with ggplot2
ggPlotForestFloor = function(
  ff, #a forestFloor object
  plot_seq=NULL, #a sequence of windows to plot
  col=NULL, # a colour vector of N length, use fcol
  orderByImportance = TRUE
) {
  
  if(is.null(plot_seq)) plot_seq = 1:max(6,dim(ff$X)[2])
  if(is.null(col)) col.points=fcol(ff)
  if(orderByImportance) {
    #if order by importance but no importance in forestFloor object
    #then create a vector with same order as training set
    if(is.null(ff$imp_ind)) imp.ind = 1:max(plot_seq) else imp.ind=ff$imp_ind
  } else {
    imp.ind = 1:dim(ff$X)[2]
  }
  thisVar=1 #declare this to kill a R check note
  print(plot_seq)
  #define plotting of one frame
  
  make.one.frame = function(ff=ff,
                            thisVar=thisVar,
                            col.points=col.points,
                            imp.ind=imp.ind) {
    #if not importance sorting, sort by training set col order
    
    
    #get requested dataset
    df = data.frame(X = ff$FCmatrix[,imp.ind[thisVar]],
                    FC= ff$X  [,imp.ind[thisVar]]  )
    #start plot plot points
    h.out  <- ggplot(df,aes_string("FC","X")) + 
      geom_point(col = col.points) +
      xlab(names(ff$X)[imp.ind[thisVar]]) +
      ylab("")
  }
  
  #plot all desired frmesm
  all.ggplots = lapply(plot_seq, function(thisVar) {
    make.one.frame(ff=ff,
                   thisVar=thisVar,
                   col.points=col.points,
                   imp.ind=imp.ind)
  })
  
  do.call(grid.arrange,c(all.ggplots)) 
}

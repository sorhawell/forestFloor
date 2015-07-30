#sf9: colour function
fcol = function(ff,
                cols = NULL,
                orderByImportance = NULL,
                X.matrix = TRUE,
                hue = NULL,
                saturation = NULL,
                brightness = NULL,
                hue.range  = NULL,
                sat.range  = NULL,
                bri.range  = NULL,
                alpha = NULL,
                RGB = NULL,
                max.df=3,
                imp.weight = NULL,
                imp.exp = 1,
                outlier.lim = 3,
                RGB.exp = NULL) {
  
  if(!X.matrix) if(class(ff)=="forestFloor_multiClass")
    stop("cannot colour by feature contributions for object of class
         'forestFloor_multiClass'. Set X.matrix=TRUE")
  
  ##ssf8.1: is between function
  ib <- function(x, low, high) (x -low) * (high-x) > 0
  ##ssf8.2: move center range of vector at mid with new width of span
  span <- function(x, mid, width) if(min(x)!=max(x)) {
    ((x-min(x))/(max(x)-min(x))-0.5)*width+mid
  } else {
    x[] = mid #fix to avoid division by zero
  }
  ##ssf8.3: compute widest range possible with given brightness or saturation
  auto.range = function(level,low=0,high=1) abs(min(level-low,high-level))*2
  ##ssf8.4: contain a vector such that any out side limits will be reduced to limits
  contain = function(x,low=0,high=1) {
    x[x>high]=high
    x[x<low ]=low
    x
  }
  
  #get/check data.frame/matrix, convert to df, remove outliers and normalize
  if(class(ff) %in% c("forestFloor_regression","forestFloor_multiClass")) {
    if(X.matrix) colM = ff$X else colM = ff$FCmatrix
    if(is.null(imp.weight)) imp.weight=TRUE
    if(is.null(orderByImportance)) orderByImportance = TRUE
  } else {
    colM=ff
    if(is.null(imp.weight)) imp.weight=FALSE
    if(is.null(orderByImportance)) orderByImportance = FALSE
  }
  
  #reorder colM by importance
  if(orderByImportance) if(class(ff) %in% c("forestFloor_regression",
                                            "forestFloor_multiClass")) {
    colM = colM[,ff$imp_ind]
  } else {
    warning("orderByImportance=TRUE takes no effect for non 'forestFloor'-class. As if set to NULL or FALSE...")
  }
  
  #check colM is either data.frame or matrix
  if(!class(colM) %in% c("data.frame","matrix")) {
    stop(paste(class(colM),"input is neither matrix or data.frame"))
  }
  
  #convert matrix to data.frame
  colM = data.frame(colM)
  
  #checking selected cols
  if(is.null(cols)) cols = 1:dim(colM)[2] #select all columns
  if(length(cols)<1 || !is.numeric(cols) || any(!cols %in% 1:dim(colM)[2])) {
    stop("no cols selected or is not integer/numeric or wrong coloumns")
  }
  sel.colM = data.frame(colM[,cols])    #use only selected columns
  sel.cols = 1:length(cols) #update cols to match new col.indices of colM
  
  #auto choose colour system: RGB=TRUE is colours system one
  if(is.null(RGB)) if(length(cols)==1) RGB=TRUE else RGB=FALSE
  if(!RGB) {
    if(is.null(saturation)) saturation = .85
    if(is.null(brightness)) brightness = .75
    if(is.null(hue))        hue = .25
  } else {
    if(is.null(saturation)) saturation = 1
    if(is.null(brightness)) brightness = .75
    if(is.null(hue))        hue = .66    
    if(is.null(RGB.exp))    RGB.exp=1.2
    if(is.null(hue.range))  hue.range=2
  }
  
  #function to force catogorical features to become numeric
  as.numeric.factor <- function(x,rearrange=TRUE) {
    if(is.numeric(x)) return(x)
    if(rearrange) x = match(x,levels(droplevels(x))) else x = match(x,levels(x))
    return(x)
  }
  
  for(i in 1:dim(sel.colM)[2]) {
    if(is.factor(sel.colM[,i])) {
      this.fac=as.numeric.factor(sel.colM[,i])
      sel.colM[,i] = this.fac
    }
    if(is.character(sel.colM[,i])) sel.colM[,i] = as.numeric(sel.colM[,i])
  } 
  
  #restrain outliers by limit(std.dev) and normalize.
  sel.colM = box.outliers(sel.colM,limit=outlier.lim)
  
  #inflating data by importance
  if(imp.weight && length(cols)>1) {
    if(class(ff) %in% c("forestFloor_regression","forestFloor_multiClass")) {
      sel.imp = ff$importance[cols]
      non.negative.imp = sel.imp+min(sel.imp)
      sumnorm.imp =  non.negative.imp / sum(non.negative.imp)
      exp.imp = sumnorm.imp ^ imp.exp #included weight exponent
      impM = t(replicate(dim(colM)[1],exp.imp))
      sel.colM = sel.colM*impM #inflate by importance
      sel.colM = sel.colM / max(sel.colM)
    } else {warning("importance weighting only possible for class 'forestFloor'")}
  }
  
  #Setting up ranges for colours
  if(any(!c(class(hue),class(saturation),class(brightness)) %in% c("numeric","integer"))){
    stop("hue, saturation and brightness must be of class numeric or integer")
  }
  #correct input to be within [0,1]
  hue = hue - floor(hue)
  saturation = max(min(saturation,1),0)
  brightness = max(min(brightness,1),0)
  
  ###################
  ###colours system A:  1-way gradient Red-Green-BLUE scale
  
  if(RGB==TRUE) {
    if(is.null(bri.range)) bri.range=0.05
    if(is.null(alpha)) alpha=.7
    len.colM = box.outliers(sel.colM,limit=Inf)
    if(dim(len.colM)[2]==1) nX = as.numeric(len.colM[,1]) else nX = as.numeric(apply(len.colM,1,mean))
    hsvcol    = t(sapply(nX,function(x) rgb2hsv(x^RGB.exp,
                                                1-x^RGB.exp-(1-x)^RGB.exp,
                                                (1-x)^RGB.exp)))
    hue.vec = hsvcol[,1] * hue.range + hue
    hue.vec[hue.vec>1] = hue.vec[hue.vec>1] - floor(hue.vec[hue.vec>1])
    hsvcol[,1] = hue.vec
    sat.range = auto.range(saturation)
    hsvcol[,2] = span(hsvcol[,2],saturation,sat.range)
    hsvcol[,2] = contain(hsvcol[,2])
    bri.range = auto.range(brightness)
    hsvcol[,3] = span(hsvcol[,3],brightness,bri.range)
    hsvcol[,3] = contain(hsvcol[,3])
    colours = apply(hsvcol,1,function(x) hsv(x[1],x[2],x[3],alpha=alpha))
    #     a = mget(ls())
    #     print(str(a))
    return(colours) #function terminates with these colours
  }
  
  ############
  ##Colour system B: Hue, saturation, value, consist of a 1D, 2D and 3D scale
  
  #if maxPC is less than n selected coloumns
  #centering, no scaling and PCA is applied
  #output scores is transformed to range [0,1]
  #cols are correect to lower manifold number maxPC
  col.df = length(cols)
  if(col.df>max.df) {
    len.colM = box.outliers(prcomp(sel.colM)$x[,1:max.df],limit=Inf)
    col.df = max.df
  } else {
    len.colM = box.outliers(sel.colM,limit=Inf)
  }
  
  #define ranges if not defined for different dimensions
  if(is.null(hue.range)) {
    if(col.df==1) hue.range = .85
    if(col.df==2) hue.range = 1 #circular no range lim needed
    if(col.df==3) hue.range = 1 #circular no range lim needed
  }
  if(is.null(sat.range)) {
    if(col.df==1) sat.range = "not used"
    if(col.df==2) sat.range = auto.range(saturation)
    if(col.df==3) sat.range = auto.range(saturation)
  } 
  if(is.null(bri.range)) {
    if(col.df==1) bri.range = "not used"
    if(col.df==2) bri.range = "not used"
    if(col.df==3) bri.range = auto.range(brightness)
  }
  if(is.null(alpha)) alpha = min(1,400/dim(len.colM)[1])
  
  
  ##writing colour scale dependent on colour degrees of freedom(col.df)
  #one way gradient
  if(col.df==1) {
    hue.vec = as.numeric(len.colM[,1]) * hue.range + hue
    hue.vec[hue.vec>1] = hue.vec[hue.vec>1] - floor(hue.vec[hue.vec>1])
    colours = hsv(h = hue.vec,
                  s = saturation,
                  v = brightness,
                  alpha = alpha) #defining colour gradient along X3)
  }
  
  #two way gradient
  if(col.df==2) {
    hsvcol = t(rgb2hsv(len.colM[,1],len.colM[,2],1-apply(len.colM,1,mean)))
    hue.vec = hsvcol[,1] * hue.range + hue
    hue.vec[hue.vec>1] = hue.vec[hue.vec>1] - 1
    hsvcol[,1] = hue.vec
    #saturation is proportional with distance to center
    hsvcol[,2] = ((len.colM[,1]-mean(len.colM[,1]))^2
                  +(len.colM[,2]-mean(len.colM[,2]))^2)^sat.range * saturation
    hsvcol[,2] = hsvcol[,2] / max(hsvcol[,2])
    hsvcol[,3] = brightness
    colours = hsv(hsvcol[,1],hsvcol[,2],hsvcol[,3],alpha=alpha)
  }  
  
  #three-way gradient
  if(col.df==3) {  
    hsvcol      = t(rgb2hsv(len.colM[,1],len.colM[,2],len.colM[,3]))
    #set hue
    hue.vec     = hsvcol[,1] * hue.range + hue
    hue.vec[hue.vec>1] = hue.vec[hue.vec>1] - 1
    hsvcol[,1]  = hue.vec
    #set sat
    span.sat    = span(hsvcol[,2],saturation,sat.range)
    hsvcol[,2]  = contain(span.sat)
    #set bri
    mean.bri    = apply(len.colM,1,mean)
    span.bri    = span(mean.bri,brightness,bri.range)
    hsvcol[,3]  = contain(span.bri)
    colours     = hsv(hsvcol[,1],hsvcol[,2],hsvcol[,3],alpha=alpha)
  }
  
  return(colours)
}

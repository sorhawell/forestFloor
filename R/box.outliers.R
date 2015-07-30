#sf6  reduce outliers to within limit of 1.5 std.dev and/or output as normalized 
box.outliers = function(x,limit=1.5,normalize=TRUE) {
  
  sx=scale(x)
  if(limit!=FALSE) {
    sx[ sx>limit] =  limit
    sx[-sx>limit] = -limit
  }
  
  if(normalize) { 
    sx.span = max(sx) - min(sx)
    sx = sx - min(sx)
    sx = sx / sx.span
  } else {
    obs=attributes(sx)$"dim"[1]
    if(dim(sx)[2]>1) {
      sx = sx * t(replicate(obs,attributes(sx)$"scaled:scale")) +
        t(replicate(obs,attributes(sx)$"scaled:center"))
    } else {
      sx  = sx * attributes(sx)$"scaled:scale" + attributes(sx)$"scaled:center" 
    }
  }
  
  if(class(x)=="data.frame") {
    sx = as.data.frame(sx,row.names=row.names(x))
    names(sx) = names(x)
  }
  return(sx)
}

#sub function to fix categorical features
as.numeric.factor <- function(x,drop.levels=TRUE) {
  if(is.numeric(x)) return(x) ## if already numeric, do onothing
  if(drop.levels) x = match(x,levels(droplevels(x))) else x = match(x,levels(x))
  return(x)
}

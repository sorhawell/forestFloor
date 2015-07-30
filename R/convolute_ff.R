
#f3 input a forestFloor object, computes convoluted feature contributions with kknn
#outout a new ff object as input with attached ff$FCfit
#kknn arguments can be accessed directly from this wrapper by userArgs.kknn
#if conflicting with wrappe
convolute_ff = function(ff,
                        these.vars=NULL,
                        k.fun=function() round(sqrt(n.obs)/2),
                        userArgs.kknn = alist(kernel="gaussian")
) {
  
  n.obs=dim(ff$X)[1]
  n.vars=dim(ff$X)[2]
  k=k.fun()
  if(is.null(these.vars)) these.vars = 1:n.vars
  
  
  #merge user and wrapper args
  Data = "I only exist to satisfy R cmd CHECK" #dummy declaration
  defaultArgs.kknn = alist(formula=fc~.,data=Data,kmax=k,kernel="gaussian")
  kknn.args=append.overwrite.alists(userArgs.kknn,defaultArgs.kknn)
  
  #iterate for seleceted variables
  ff$FCfit = sapply(these.vars, function(this.var) {
    #force factors into numeric levels
    Xcontext = ff$X[,this.var]
    if(!is.numeric(Xcontext)) Xcontext = as.numeric(Xcontext)
    #print(Xcontext)
    #construct data.frame of FCs and context X
    Data = data.frame(fc=ff$FCmatrix[,this.var],x=Xcontext)
    #train and predict FC as function of X, LOO crosvalidated
    knn.obj = do.call("train.kknn",kknn.args)$fitted.values
    knn.obj[[length(knn.obj)]] #extract crosvalidated predictions
  })
  ff  
}

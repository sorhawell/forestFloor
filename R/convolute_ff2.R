#f3 input a forestFloor object, as convolute_ff but do not iterate all variables. Instead wrapper will use
#kknn to convolute a designated featureContribution with designated features - oftenly the corresponding features.
convolute_ff2 = function(ff,
                         Xi,
                         FCi = NULL,
                         k.fun=function() round(sqrt(n.obs)/2),
                         userArgs.kknn = alist(kernel="gaussian")
) {
  if(is.null(FCi)) FCi = Xi
  n.obs=dim(ff$X)[1]
  k=k.fun()
  
  #merge user and wrapper args
  defaultArgs.kknn = alist(formula=fc~.,data=Data,kmax=k,kernel="gaussian")
  kknn.args=append.overwrite.alists(userArgs.kknn,defaultArgs.kknn)
  
  #collect coloumns
  if(length(FCi)>1) {
    fc = apply(ff$FCmatrix[,FCi],1,sum)
  } else {
    fc = ff$FCmatrix[,FCi]
  }
  x = ff$X[,Xi]
  
  #compute topology
  Data = data.frame(fc=fc,x=x)
  knn.obj = do.call("train.kknn",kknn.args)$fitted.values
  out = knn.obj[[length(knn.obj)]]
}

#f4 input a forestFloor object, as convolute_ff but do not iterate all variables. Instead wrapper will use
#kknn to convolute a designated featureContribution with designated features - oftenly the corresponding features.
convolute_grid = function(ff,
                          Xi,
                          FCi = NULL,
                          grid = 30,
                          limit = 3,
                          zoom = 3,
                          k.fun=function() round(sqrt(n.obs)/2),
                          userArgs.kknn = alist(kernel="gaussian")
) {
  
  #input defaults
  if(is.null(FCi)) FCi = Xi
  n.obs=dim(ff$X)[1]
  k=k.fun() # will be overided if a "k=" argument is provided in userArgs.kknn-list
  
  #collect coloumns of data
  if(length(FCi)>1) {
    fc = apply(ff$FCmatrix[,FCi],1,sum)
  } else {
    fc = ff$FCmatrix[,FCi]
  }
  X = ff$X[,Xi]
  
  #make grid, or use external grid if provided
  if(length(grid)==1) {
    X = box.outliers(X,limit=limit,normalize=F)
    get.seq = function(x) {
      upper = (max(x) - mean(x)) * zoom
      lower = (min(x) - mean(x)) * zoom
      seq(lower+mean(x), upper+mean(x),length.out=grid)
    }
    ite.val=lapply(1:dim(X)[2],function(i) get.seq(X[,i])) #for each variable define span of grid
    gridX = as.data.frame(expand.grid(ite.val))
    names(gridX) = names(X)
    
    ##WOUPS NEVERMIND it worked...
    #     #could not make list of vectors work with expand.grid as doc promises
    #     #this hack calls expand.grid with specified arguments
    #     
    #     #hack - make charecter string of appropiate code
    #     run.this = "alist("
    #     for(i in 1:dim(X)[2]) run.this = paste(run.this,names(X)[i]," = ite.val[,",i,"],",sep="")
    #     run.this = substr(run.this,1,nchar(run.this)-1)
    #     run.this = paste(run.this,")")
    #     #hack - parse and evaluate string into a argument list of one argument for each dimension
    #     arg.list = eval(parse(text=run.this))
    #     #call expand.grid with specified arguments
    #     gridX=as.data.frame(do.call(expand.grid,arg.list)) #grid coordinates
    
  } else {
    gridX = grid
  }
  
  #prepare data
  Data = data.frame(fc=fc,x=ff$X[,Xi])
  gridX = data.frame(x=gridX)
  
  #merge user args and args of this wrapper function, user args have priority
  defaultArgs.kknn = alist(formula=fc~.,train=Data,k=k,kernel="gaussian",test=gridX)
  kknn.args=append.overwrite.alists(userArgs.kknn,defaultArgs.kknn)
  
  #execute kknn function and retrive convolution
  gridFC = do.call("kknn",kknn.args)$fitted.values
  
  #gather results in data frame, with convoluted feature contributinos in grid
  return(data.frame(fc = gridFC,gridX))
}

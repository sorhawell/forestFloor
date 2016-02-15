#test Xtestmerger
library(randomForest)
library(forestFloor)
#X y could be a training set
X = data.frame(numeric = c(1,5,2,7,-4.3),
               factor1 = factor(c("jim","freddy","marley","marley","alfred")),
               factor2 = factor(c("jill","ann","liz","leila","vicky")))
y = factor(1:5)

set.seed(1)
rf = randomForest(X,y,keep.inbag=T,ntree=7)


#should fail, as nathalia is not a level in X$factor1
test = data.frame(numeric = rnorm(5),
                  factor1 = factor(c("jim","jim","jim","nathalia","freddy")),
                  factor2 = factor(c("jill","jill","vicky","leila","vicky")))
err = tryCatch(Xtestmerger(X,test,rf$inbag), error=function(e) e)
if(err[1]!=paste0("unmatched levels, please check following levels[] of",
                  " following factors$: \n $factor1 [1] \"nathalia\"  \n ",
                  "Info: any level in test must match X, as a forest cannot ",
                  "score a new level after training"))
  stop("Xtestmerger did not correctly detect factor levels mismatch")
  
#should fail as names miss match, factor0  
test = data.frame(numeric = rnorm(5),
                  factor1 = factor(c("jim","jim","jim","freddy","freddy")),
                  factor0 = factor(c("jill","jill","vicky","leila","vicky")))
err = tryCatch(Xtestmerger(X,test,rf$inbag), error=function(e) e)
if(err[1]!=paste0( "column(s) 3 in Xtest and X has names mismatch. Col names must match"))
  stop("error Xtestmerger did not correcty detect names miss match")

#should fail as different column class, factor2 is char  
test = data.frame(numeric = rnorm(5),
                  factor1 = factor(c("jim","jim","jim","freddy","freddy")),
                  factor2 = as.character(c("jill","jill","vicky","leila","vicky")),
                  stringsAsFactors = FALSE)
#Xtestmerger(X,test,rf$inbag)
err = tryCatch(Xtestmerger(X,test,rf$inbag), error=function(e) e)
if(err[1]!="column(s) 3 in test and X has class mismatch. Must be numeric-numeric,factor-factor")
  stop("error Xtestmerger did not correctly detect class mismatch")


#should not raise any error
test = data.frame(numeric = rnorm(5),
                  factor1 = factor(c("jim","jim","jim","freddy","freddy")),
                  factor2 = factor(c("jill","jill","vicky","leila","vicky"))
                  )

#testbigIinbag and bigy is null when input inbag and y is null 
if(all(is.null(Xtestmerger(X,test)[c("bigInbag","bigy")]))) stop("should only return NULL")


out = Xtestmerger(X,test,inbag=rf$inbag,y=y)

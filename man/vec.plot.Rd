\name{vec.plot}
\alias{vec.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute and plot vector effct characteristics for a given multivariate model
}
\description{
vec.plot visulizes the vector effect charecteristics of a given model. One(2D plot) or two(3D plot) variables are screened within the range of the training data, while remaining variables are fixed the univariate means of each them(as default). If remaining variables do not interact strongly with plotted variable(s), vec.plot is a good tool to break up a high-dimensional model topology in separate components.
}

\usage{
vec.plot(model,X,i.var,grid.lines=100,VEC.function=mean,
         zoom=1,limitY=F,col="#20202050")
}

\arguments{
  \item{model}{
  model, S3 or S4 object
  model_object who have a defined method predict.model, which can accept arguments as showed for
  randomForest e.g.
  library(randomForest)
  model = randomForest(X,Y)
  predict(model,X)
  
  where X is the training features and Y is the training response vector(numeric)
  
  }
  \item{X}{
  matrix or data.frame being the same as input to model
  
  }
  \item{i.var}{
  vector, of column_numbers of variables to scan. No plotting is available for more than two variables.
  
  }
  \item{grid.lines}{
  scalar, number of values by each variable to be predicted by model. Total number of combinations = grid.lines^length(i_var).

  }
  \item{VEC.function}{
  function, method univariately a fixed value for any remaining variables(those not chosen by i.var). Default is mean.
  }
  
  \item{zoom}{
  scalar, number defining the size.factor of the VEC.surface compared to data range of scanned variables. Bigger number is bigger surface.
  }
  
  \item{limitY}{
  boleen, if TRUE Y-axis is standardised for any variable. Useful for composite plots as shown in example.
  }
  
  \item{col}{
one colour or vector of colours of points passed to rgl::plot3d
  }
  
}
\details{
vec.plot visulizes the vector effect charecteristics of a given model. One(2D plot) or two(3D plot) variables are screened within the range of the training data, while remaining variables are fixed at the univariate means of each them(as default). If remaining variables do not interact strongly with plotted variable(s), vec.plot is a good tool to break up a high-dimensional model topology in separate components.
}

\value{
no value
}

\author{
Soren Havelund Welling
}

\examples{
\dontrun{
#simulate data
obs=5000
vars = 6 
X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + 2*sin(X2*pi) + 2 * X3 * (X4+.5))
Yerror = 1 * rnorm(obs)
var(Y)/var(Y+Yerror)
Y= Y+Yerror

#grow a forest, remeber to include inbag
rfo2=randomForest(X,Y,keep.inbag=TRUE,ntree=1000,sampsize=800)

#plot partial functions of most important variables first
pars=par(no.readonly=TRUE) #save previous graphical paremeters
par(mfrow=c(2,3),mar=c(2,2,1,1))
for(i in 1:vars) vec.plot(rfo2,X,i,zoom=1.5,limitY=TRUE)
par(pars) #restore

#plot partial functions of most important variables first
for(i in 1:vars) vec.plot(rfo2,X,i,zoom=1.5,limitY=TRUE)

#plotvariable X3 and X4 with vec.plot
vec.plot(rfo2,X,c(3,4),zoom=1,grid.lines=100)
}
}


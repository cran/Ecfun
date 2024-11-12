\name{simulate.glm}
\alias{simulate.glm}
\title{
  A "simulate" method for a glm object
}
\description{
  Simulate predictions for \code{newdata} for a
  model of class \code{\link{glm}} with mean 
  \code{coef(object)} and variance 
  \code{vcov(object)}. 

  NOTES: The \strong{stats} package has a 
  \code{\link[stats]{simulate}} method for 
  "\code{lm}" objects which is used for 
  \code{\link{lm}} and \code{\link{glm}} objects. 
  It differs from the current \code{simulate.glm}
  function in two fundamental and important ways:  

  \enumerate{
    \item
      \code{stats::\link[stats]{simulate}}
      returns simulated data consistent with the 
      model fit assuming the estimated model 
      parameters are true and exact, i.e., 
      ignoring the uncertainty in parameter 
      estimation.  Thus, if \code{family = 
      poisson}, 
      \code{stats::\link[stats]{simulate}}
      returns nonnegative integers.  
    
      By contrast the \code{simulate.glm}
      function documented here returns optionally 
      simulated \code{coef (coefficients)} plus 
      simulated values for the \code{link} and / 
      or \code{response} but currently \emph{NOT} 
      pseudo-random numbers on the scale of the 
      response.  
    \item 
      The \code{simulate.glm} function documented
      here also accepts an optional \code{newdata}
      argument, not accepted by 
      \code{stats::\link[stats]{simulate}}.  The 
      \code{stats::\link[stats]{simulate}} 
      function only returns simulated values for 
      the cases in the training set with no 
      possibilities for use for different sets 
      of conditions.  
  }  
}
\usage{
\method{simulate}{glm}(object, nsim = 1, 
    seed = NULL, newdata=NULL, 
    type = c("coef", "link", "response"), ...)
}
\arguments{
  \item{object}{
    an object representing a fitted model 
    of class \code{\link{glm}}.  
  }
  \item{nsim}{
    	number of response vectors to simulate. 
    	Defaults to 1.
  }
  \item{seed}{
    Argument passed as the first argument to 
    \code{\link{set.seed}} if not NULL.  
  }
  \item{newdata}{
    optionally, a \code{\link{data.frame}} 
    in which to look for variables with 
    which to predict. If omitted, predictors 
    used in fitting are used.  
  }
  \item{type}{
    the type of simulations required. 
    \itemize{
      \item \code{coef}: 
        Simulated coefficents using 
        \code{mvtnorm::\link[mvtnorm]{rmvnorm}(nsim,
        coef(object), vcov(object))}.  
      \item \code{link}: 
        The default \code{type}='link' is on the 
        scale of the linear predictors using 
        \code{\link[mvtnorm]{rmvnorm}} applied  
        to randomly selected components of the 
        mixture with \code{mean} = 
        \code{\link{coef}} and \code{sigma} = 
        \code{\link{vcov}} for that component.  
        For a default binomial model, these are 
        of log-odds (probabilities on logit 
        scale).
      \item \code{response}: 
        \code{object[['linkinv']]} of \code{
        type = 'link'}.  For a binomial model, 
        these are predicted probabilities.  
    }
  }
  \item{...}{
    further arguments passed to or from other
    methods.
  }
}
\details{
  1.  Save current \code{seed} and optionally set 
  it using code copied from 
  \code{stats:::simulate.lm}.  
  
  2.  \code{if(is.null(newdata))newdata} gets the
  data used in the call to \code{\link{glm}}.
  
  3.  \code{newMat <- model.matrix(~., newdata)}

  4.  \code{simCoef <- mvtnorm::rmvnorm(nsim, 
  coef(object), vcov(object))}
  
  5.  \code{sims <- tcrossprod(newMat, simCoef)}
  
  6.  If \code{length(type)} == 1:  return a
  \code{\link{data.frame}} with one column for 
  each desired simulation, consistent with the 
  behavior of the generic \code{\link{simulate}}
  applied to objects of class \code{lm} or 
  \code{glm}.  Otherwise, return a list of 
  \code{\link{data.frame}}s of the desired types.  
}
\value{
  Returns either a \code{\link{data.frame}} or a
  list of  \code{\link{data.frame}}s depending 
  on 'type':  
   
  \item{\code{coef}}{a \code{\link{data.frame}} 
    with \code{nsim} columns giving simulated
    parameters generated using 
    \code{mvtnorm::\link[mvtnorm]{rmvnorm}(nsim, 
    coef(object), vcov(object))}.  
  }
  \item{\code{link}}{
    a \code{\link{data.frame}} with \code{nsim} 
    columns of \code{nobs} values each giving 
    the simulations on the \code{link} scale by
    applying each set of simulated coefficients 
    to \code{newdata} (or to the training set of
    \code{newdata} is not supplied).  
  }
  \item{response}{
    a \code{\link{data.frame}} with \code{nsim}
    columns of \code{nobs} values each giving the
    simulations on the \code{response} scale, 
    being \code{linkinv} of the simulations on 
    the \code{link} scale.  
  }
  \item{if length(type)>1}{
    a list with simulations on the desired 
    scales.  
  }
  
  The value also has an attribute "\code{seed}". 
  If argument \code{seed} is \code{NULL}, the
  attribute is the value of 
  \code{\link{.Random.seed}} before the 
  simulation started.  Otherwise it is the value 
  of the argument with a \code{kind} attribute 
  with value \code{as.list(RNGkind())}.  
  
  NOTE:  This function currently may not work
  with a model fit that involves a multivariate 
  \code{link} or \code{response}.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link[stats]{simulate}}
  \code{\link{glm}}
  \code{\link{predict.glm}}
  \code{\link{set.seed}}
}
%\references{}
\examples{
library(mvtnorm)
##
## 1.  a factor and a numeric 
##
PoisReg2 <- data.frame(y=1:6, 
    x=factor(rep(0:2, 2)), x1=rep(1:2, e=3))
GLMpoisR2 <- glm(y~x+x1, poisson, PoisReg2)

newDat. <- data.frame(
  x=factor(rep(c(0, 2), 2), levels=0:2), 
  x1=3:6)
# NOTE:  Force newDat2['x'] to have the same levels
# as PoisReg2['x']

GLMsim2n <- simulate(GLMpoisR2, nsim=3, seed=2,
  newdata=newDat.)

##
## 2.  One variable:  BMA returns
##     a mixture of constant & linear models
##
PoisRegDat <- data.frame(x=1:2, y=c(5, 10))
GLMex <- glm(y~x, poisson, PoisRegDat)

# Simulate for the model data 
GLMsig <- simulate(GLMex, nsim=2, seed=1)  

# Simulate for new data
newDat <- data.frame(x=3:4, 
      row.names=paste0('f', 3:4))
GLMsio <- simulate(GLMex, nsim=3, seed=2, 
                      newdata=newDat)

##
## 2a.  Compute the correct answers manually 
##
newMat <- model.matrix(~., newDat)
RNGstate <- structure(2, kind = as.list(RNGkind()))
set.seed(2)

sim <- mvtnorm::rmvnorm(3, coef(GLMex), 
                         vcov(GLMex))
rownames(sim) <- paste0('sim_', 1:3)
simDF <- data.frame(t(sim))

GLMsim.l <- tcrossprod(newMat, sim)
colnames(GLMsim.l) <- paste0('sim_', 1:3)
GLMsim.r <- exp(GLMsim.l) 
GLMsim2 <- list(coef=simDF, 
  link=data.frame(GLMsim.l), 
  response=data.frame(GLMsim.r) )
attr(GLMsim2, 'seed') <- RNGstate  

\dontshow{stopifnot(}
all.equal(GLMsio, GLMsim2)
\dontshow{)}
}
\keyword{datagen}

simulate.glm <- function(object, nsim = 1, 
        seed = NULL, newdata=NULL, 
        type = c("coef", "link", "response"), ...){
##  
## 1.  seed?
##  
# copy code from stats:::simulate.lm  
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  if (is.null(seed)){ 
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
##
## 2.  newdata?  
##  
  cl <- as.list(object[['call']])
  if(is.null(newdata)){
    if('x' %in% names(object)){
      newMat <- object$x 
    } else {
      if(is.null(cl$data)){
        newMat <- model.matrix(cl$formula, 
                data = environment(object), ...)
      } else {
        fmla <- eval(cl$formula)
        dat <- eval(cl$data)
        newMat <- model.matrix(fmla, data = dat, ...) 
      }
    }
  } else newMat <- model.matrix(~., newdata)
##
## 3.  simCoef
##
  nobs <- NROW(newdata)
  vc <- vcov(object)
  simCoef <- mvtnorm::rmvnorm(nsim, 
          coef(object), vc)
  rownames(simCoef) <- paste0('sim_', 1:nsim)
  sims <- tcrossprod(newMat, simCoef)
#  colnames(sims) <- paste0('sim_', 1:nsim)
## 
## 9.  need "response"?
##
# if('response' %in% type){
  if(any(!is.na(pmatch(type, 'response')))){
    fam <- cl$family
    if(is.character(fam)){
      fam <- get(fam, mode = "function", 
                 envir = parent.frame())
    }
    if(class(fam) %in% c('call', 'name')) fam <- eval(fam)
    if(is.function(fam)) 
      fam <- fam()
    if(is.null(fam$family)) {
      print(fam)
      stop("'family' not recognized")
    }
    linkinv <- fam$linkinv
    if(!is.function(linkinv)){
      print(fam)
      stop("linkinv from 'family' is not a function.")
    }
  }
##
## 10.  return type(s) desired
##
#  10.1.   only 1 type
  if(length(type)<1)stop('No "type" requested.')
  if(length(type)<2){
    if(!is.na(pmatch(type, 'coef'))){
      out <- data.frame(t(simCoef))
      attr(out, 'seed') <- RNGstate
      return(out)
    } else if(!is.na(pmatch(type, 'link'))){
      out <- data.frame(sims)
      attr(out, 'seed') <- RNGstate
      return(out)
    } else if(is.na(pmatch(type, 'response'))){
      stop('Not a recognized type.  type = ', type)
    } else {
      out <- data.frame(linkinv(sims))
      attr(out, 'seed') <- RNGstate
      return(out)
    }
  }
#  10.2. more than 1 type requested
  out <- vector('list', length(type))
  names(out) <- type 
  if(any(!is.na(pmatch(type, 'coef')))){
      out[['coef']] <- data.frame(t(simCoef))
  }
  if(any(!is.na(pmatch(type, 'link')))){
    out[['link']] <- data.frame(sims)
  } 
  if(any(!is.na(pmatch(type, 'response')))){
    out[['response']] <- data.frame(linkinv(sims))
  } 
  attr(out, 'seed') <- RNGstate
#  cat('print(type)\n')
#  print(type)
  out
}
simulate.bic.glm <- function(object, nsim = 1, 
        seed = NULL, newdata=NULL, 
        type = c("coef", "link", "response"), ...){
##  
## 1.  seed?
##  
# copy code from stats:::simulate.lm  
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  if (is.null(seed)) 
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
##
## 2.  local copies of components of object
##  
  postprob <- object[['postprob']]
  x <- object[['x']]
  y <- object[['y']]
  mle <- object[['mle']]
  linkinv <- object[['linkinv']]
##
## 3.  wt and family from call
##
  cl <- as.list(object[['call']])
  wt <- cl[['wt']]
  fam <- cl[['glm.family']]
#  convert fam to the form required by glm.fit 
  family <- get(as.character(fam), 
      mode = "function", envir = parent.frame())
  if (is.function(family)) 
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
##
## 4.  newdata?
##
  if(is.null(newdata)){
    newdata <- x 
  } else {
#   ensure that newdata has the same variables as x
#   Number of columns the same?  
    if(ncol(newdata) != ncol(x)){
      colEr <- paste0("The columns of newdata ", 
        "must match object$x:\nncol(newdta) = ", 
        ncol(newdata), " != ncol(object$x) = ", 
        ncol(x) )
      stop(colEr)
    }
#   Column names the same?      
    oopsNx <- which(names(newdata) != names(x))
    if(length(oopsNx)>0){
#   Does the names problem disappear 
#   by suppressing trailing ".x"?  
      namesx <- sub('\\.x$', '', names(x))
      oopsNx2 <- which(names(newdata) != namesx)
      if(length(oopsNx2)>0){
        erMsg <- paste0(
          "names(newdata) do not match those of", 
            " object$x.\nLook at column")
        if(length(oopsNx)>1){
          erMsg <- paste0(erMsg, "s")
        }
        stop(erMsg, " ", paste(oopsNx, collapse=", "))
      }
    }
#   Factors in the same places? 
    Facs <- which(sapply(x, class)=="factor")
    newFacs <- which(sapply(newdata, class)
          == "factor")
    if(length(Facs) != length(newFacs))
      stop("newdata does not have the same ", 
           "number of factors as object$x: ", 
           "newdata has ", length(newFacs), 
           "; object$x has ", length(Facs))
#   Factors have the same levels?  
    for(i in Facs){
      lvlNi <- levels(newdata[[i]])
      lvlxi <- levels(x[[i]])
      if(length(lvlNi) != length(lvlxi))
        stop("levels(newdata[[", i, 
            "]]) are different from levels(x[[", 
            i, "]])")
      oopsLi <- which(lvlNi != lvlxi)
      if(length(oopsLi)>0)
        stop("levels(newdata[[", i, 
            "]]) are different from levels(x[[", 
            i, "]])")
    }
  }
##
## 5.  model.matrices 
##
  xMat <- model.matrix(~., x)
  newMat <- model.matrix(~., newdata)
##
## 6.  sims = matrix to hold ouputs
##
  nComponents <- length(postprob)
  nobs <- NROW(newdata)
  sims <- matrix(NA, nobs, nsim)
  colnames(sims) <- paste0("sim_", 1:nsim)
  if(!is.null(rownames(newMat))) 
    rownames(sims) <- rownames(newMat)
#  if(is.null(wt))wt <- rep(1, nobs)
##
## 7.  Which models where? 
##
  rmdl <- sample(1:nComponents, nsim, TRUE, 
      postprob)
#  
  nVars <- NCOL(mle)    
  Coefs <- matrix(0, nVars, nsim)
  dimnames(Coefs) <- list(
    colnames(newMat), colnames(sims) )
##
## 8.  Simulate in link space
##
  for(Comp in 1:nComponents){

    nsimComp <- sum(rmdl==Comp)
    if(any(rmdl==Comp)){
      xMatC <- xMat[, mle[Comp,]!=0, drop=FALSE]
      refitComp <- glm.fit(x=xMatC, y=y, 
          weights=wt, start=mle[Comp, mle[Comp,]!=0], 
            family=family)
      class(refitComp) <- 'glm'
      vc <- vcov(refitComp)
      simCoef <- mvtnorm::rmvnorm(nsimComp, 
                    coef(refitComp), vc)
      Coefs[mle[Comp,]!=0, rmdl==Comp] <- t(simCoef)
#      
      newM <- newMat[, mle[Comp,]!=0, drop=FALSE]
      predComp <- tcrossprod(newM, simCoef)
      sims[, rmdl==Comp] <- predComp
    }
  }
## 
## 9.  return type(s) desired 
##
#  9.1.   only 1 type
  if(length(type)<1)stop('No "type" requested.')
  if(length(type)<2){
    if(!is.na(pmatch(type, 'coef'))){
      out <- Coefs
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
#  9.2. more than 1 type requested
  out <- vector('list', length(type))
  names(out) <- type 
  if(any(!is.na(pmatch(type, 'coef')))){
      out[['coef']] <- Coefs
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
#
  attr(out, 'seed') <- RNGstate
  out
}
compareOverlap <- function(y=2, yRef=y, x=1, 
                 xRef=x, newDat, refDat, 
                 ignoreCase=TRUE, ...){
##
## 1.  Match names
##  
  yNew <- pmatchIC(y, names(newDat))
  yref <- pmatchIC(yRef, names(refDat))
  xNew <- pmatchIC(x, names(newDat))
  xref <- pmatchIC(xRef, names(refDat))
##
## 2.  Find overlap 
##  
  newInRef <- (newDat[, xNew] %in%
                refDat[, xref])
  refInNew <- (refDat[, xref] %in% 
                newDat[, xNew])
##
## 3.   Select overlap
##  
  XYnew <- newDat[newInRef, c(xNew, yNew)]
  XYref <- refDat[refInNew, c(xref, yref)]
  Ynew <- XYnew[order(XYnew[, xNew]), yNew]
  Yref <- XYref[order(XYref[, xref]), yref]
##
## 4.  Dif
##
  dy <- Ynew-Yref
  dyRef <- dy/Yref
  out <- data.frame(x=sort(XYnew[, xNew]), 
            yNew=Ynew, yRef=Yref, dy=dy, 
            dyRef=dyRef)
  if(any(is.na(dyRef))){ 
    plot(dy~x, out, type='b', ...)
  } else {
    plot(dyRef~x, out, type='b', ...)
  }
  names(out)[1] <- xNew
  names(out)[2] <- paste0(yNew, 'New')
  names(out)[3] <- paste0(yref, 'Ref')
  invisible(out)
}
\name{compareOverlap}
\alias{compareOverlap}
\title{
  Compare y between newDat and 
  refDat for shared values of x
}
\description{
  Compute 
  \code{dy <- (y - yRef)} for all 
  cases where \code{x == xRef}, 
  where \code{x} and \code{y} are 
  columns of \code{newDat} and 
  \code{xRef} and \code{yRef} are 
  columns of \code{refDat}.  
  
  Also compute 
  \code{dyRef <- dy / yRef}.  
  
  Return silently a 
  \code{data.frame} with columns 
  \code{x}, \code{y}, \code{yRef}, 
  \code{dy}, and \code{dyRef}. 
  
  Also if 
  \code{min(yRef)*max(yRef)>0} 
  \code{plot(dyRef)} else
  \code{plot(dy)}.  
}
\usage{
compareOverlap(y=2, yRef=y, x=1, 
      xRef=x, newDat, refDat, 
      ignoreCase=TRUE, ...)
}
\arguments{
  \item{y, yRef}{
    columns of \code{newDat}, 
    \code{refDat}, respectively, 
    to compare, ignoring case in the 
    names unless \code{ignoreCase} is 
    \code{FALSE}.  
  }
  \item{x, xRef}{
    columns of \code{newDat}, 
    \code{refDat}, respectively, 
    to match when comparing 
    \code{y} with \code{yRef}.
    
    As with \code{y} and \code{yRef}, 
    ignore case in name matching 
    unless \code{ignoreCase} is 
    \code{FALSE}.  
  }
  \item{newDat, refDat}{
    \code{\link{data.frame}}s of new 
    and reference data in which to 
    search for overlap, i.e., common 
    values of \code{newD[, x]} and 
    \code{refDat[, xRef]}, and for 
    those observations to compare 
    \code{newDat[, y]} to 
    \code{refDat[, yRef]}.  
  }
  \item{ignoreCase}{
    logical:  If \code{TRUE}, 
    ignore case when searching 
    for columns of \code{newDat} 
    and \code{refDat} to match 
    \code{y}, \code{yRef}, 
    \code{x}, and \code{xRef}.
  }
  \item{...}{
    optional arguments to pass 
    to \code{plot}
  }
}
\details{
  This function is particularly useful 
  for updating datasets that are obtained 
  from sources like the 
  \href{https://bjs.ojp.gov}{Bureau of Justice Statistics}, 
  which publish many series with 
  each update including the most recent 
  11 years.  This function can be used 
  to evaluate the extent of equivalence
  between, e.g., historical data in 
  \code{refDat} with the latest data
  in \code{newDat}.
  
}
\value{
  Invisibly return a 
  \code{data.frame} with columns 
  \code{x}, \code{paste0(y, 'New')}, 
  \code{past0(yRef, 'Ref')}, 
  \code{dy}, and \code{dyRef}
  of the data compared.  
}
%\references{}
\author{Spencer Graves}
%\note{}
%\section{Warning }{....} ~
%\seealso{}
\examples{
nDat <- data.frame(yr=2000:2015, 
          Y=0:15)
rDat <- data.frame(Yr=2018:2011, 
          y=c(17:13, 13:11))
nrDat <- compareOverlap(
  newDat=nDat, refDat=rDat)

# Correct answer
NRdat <- data.frame(yr=2011:2015, 
  YNew=11:15, yRef=c(11:13, 13:14), 
  dy=c(0,0,0, 1, 1), 
  dyRef=c(0,0,0, 1,1) / 
        c(11:13, 13:14))

\dontshow{stopifnot(}
all.equal(nrDat, NRdat)
\dontshow{)}
}
\keyword{hplot}
% \concept{ ~cpt1 }

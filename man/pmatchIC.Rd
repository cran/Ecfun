\name{pmatchIC}
\alias{pmatchIC}
\title{
  pmatch ignoring case
}
\description{
  \code{pmatch} with an additional 
  \code{ignoreCase} argument, returning 
  a name not an index like 
  \code{\link{pmatch}} (and returning 
  a name if supplied a number, unlike
  \code{\link{pmatch}}, which coerces 
  the input to numeric).  
}
\usage{
pmatchIC(x, table, nomatch = NA_integer_, 
  duplicates.ok = FALSE, 
  ignoreCase=TRUE)
}
\arguments{
  \item{x}{
    the values to be matched.  
    If \code{is.numeric(x)}, 
    \code{pmatch2} returns 
    \code{table[x]}.  
    
    This is different from 
    \code{\link{pmatch}}, which 
    matches \code{as.character(x)}. 
    
    Otherwise, if \code{ignoreCase}
    is \code{TRUE}, \code{pmatchIC}
    returns 
    \code{pmatch(tolower(x), tolower(table))}.  
  }
  \item{table}{
    the values to be matched against: 
    converted to a character vector, 
    per \code{\link{pmatch}}. 
  }
  \item{nomatch}{
    the value to be returned at 
    non-matching or multiply partially 
    matching positions. 
  }
  \item{duplicates.ok}{
    should elements be in table be used 
    more than once? (See \code{pmatch}
    for an example.)
  }
  \item{ignoreCase}{
    logical:  if \code{TRUE} and 
    \code{x} is character, 
    \code{pmatchIC} returns 
    \code{pmatch(tolower(x), tolower(table))}.  
  }
}
%\details{}
\value{
  A character vector of matches.  
}
%\references{}
\author{Spencer Graves}
%\note{}
%\section{Warning }{....} ~
\seealso{\code{\link{pmatch}}}
\examples{
yr <- pmatchIC('Yr', c('y1', 'yr', 'y2'))
\dontshow{stopifnot(}
all.equal('yr', yr)
\dontshow{)}

# integer
m2 <- pmatchIC(2, table=letters)
\dontshow{stopifnot(}
all.equal(m2, 'b')
\dontshow{)}
}
\keyword{manip}
% \concept{ ~cpt1 }

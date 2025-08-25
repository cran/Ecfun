\name{grepInTable}
\alias{grepInTable}
\title{
  Find a 3-letter country code in rworldmap::countrySynonyms
}
\description{
  Search for a specific country name on different columns 
  of alternative names in all but the first column of 
  \code{referenceTable} and return the contents of the 
  first column if found or "No match found for country", 
  followed by the value of \code{string} for that case.
  
  By default, \code{referenceTable} = 
  \code{\link[rworldmap]{countrySynonyms}}[, -1]. 
  
  NOTE: This code will be offered to the maintainer of the 
  \code{rworldmap} package. If they like it, it may not stay 
  in \code{Ecfun}. 
}
\usage{
grepInTable(pattern, referenceTable = 
        rworldmap::countrySynonyms[, -1], 
        ignore.case=TRUE, collapse=', ', ...)
}
\arguments{
  \item{pattern}{ 
    Character vector of matches of pattern[i] in 
    different columns of 
    \code{referenceTable[, -1]} until a match is found.  
  }
  \item{referenceTable}{
    \code{\link{data.frame}} with the first column
    containing reference codes to be returned if found. 
    
    if(\code{ignore.case}) force \code{pattern} and 
    \code{referenceTable} \code{\link{toupper}} 
    before searching for matches. 
  }
  \item{ignore.case}{
    If \code{TRUE}, force all \code{\link{toupper}}. 
  }
  \item{collapse}{
    Ignored if only one match is found. Otherwise, 
    return 
    \code{paste0(referenceTable[ix, 1], collapse=collapse)} 
    where \code{ix} = the indices of all matches for 
    \code{pattern[i]}. 
  }
  \item{\dots}{
    optional arguments to pass to \code{\link{grep}}. 
  }
}
\details{
  1. if(\code{ignore.case}) Force \code{pattern} and 
  all columns of 
  \code{referenceTable} \code{\link{toupper}}. 
  
  2. \code{\link{grep}} for \code{string[i]} in 
  \code{referenceTable[-1} and return the 
  corresponding element of 
  \code{referenceTable[1]} if found and an 
  appropriate message otherwise. 
}
\value{
  A character vector of the contents of 
  \code{referenceTable[1]} for all matches
  or not-found messages that include 
  \code{string[i]} when \code{string[i]}
  is not found. 
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{grep}}, \code{\link{toupper}}, 
  \code{\link[rworldmap]{countrySynonyms}}
}
%\references{}
\examples{
tstCodes <- grepInTable(
    c('Iran', 'Christmas Island', 'eSwatini'))
    
answer <- c(Iran='IRN', 'Christmas Island' =
  paste("Found row 53 of referenceTable with column 1 = ''"),  
  'eSwatini'="No match found for eSwatini")    

\dontshow{
stopifnot(
}
all.equal(tstCodes, answer)
\dontshow{)}
}    
\keyword{manip}

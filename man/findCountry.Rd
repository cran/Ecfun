\name{findCountry}
\alias{findCountry}
\title{
  Find a 3-letter country code in rworldmap::countrySynonyms
}
\description{
  Search for a specific country name on 8 different columns 
  of alternative names in 
  \code{\link[rworldmap]{countrySynonyms}}. Return either a 
  3-character code or a longer character string saying, e.g., 
  "No match found for country" followed by the name of the 
  country not found. 
  
  NOTE: This code will be offered to the 
  maintainer of the \code{rworldmap} package. If they
  like it, it may not stay in \code{Ecfun}. 
}
\usage{
findCountry(string, referenceTable = 
        rworldmap::countrySynonyms)
}
\arguments{
  \item{string}{ 
    Character vector of strings to look for matches in 
    \code{rworldmap::countrySynonyms[, 3:10]}. 
  }
  \item{referenceTable}{
    \code{\link{data.frame}} with the second column
    containing reference codes to be returned if found. 
    \code{referencTable[-(1:2)]} are forced 
    \code{\link{toupper}} then presented in sequence
    in \code{\link{grep}} until a match for 
    \code{pattern = toupper(string[i])} is found. 
  }
}
\details{
  1. Force \code{string} and 
  \code{rworldmap::countrySynonyms[, 3:10]}
  \code{\link{toupper}}. 
  
  2. \code{\link{grep}} for \code{string[i]} in 
  \code{rworldmap::countrySynonyms[, 3:10]} and
  return the element of 
  \code{rworldmap::countrySynonyms$ISO3}
  that corresponds to anything found if it's 
  3 characters and an appropriate message 
  otherwise. 
}
\value{
  A character of 3-character codes or longer 
  not-found messages with names = \code{string}. 
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
tstCodes <- findCountry(
    c('Iran', 'Christmas Island', 'eSwatini'))
    
answer <- c(Iran='IRN', 
  'Christmas Island'="referenceTable[53, 2] = ''", 
  'eSwatini'="No match found for country eSwatini")    

\dontshow{
stopifnot(
}
all.equal(tstCodes, answer)
\dontshow{)}
}    
\keyword{manip}

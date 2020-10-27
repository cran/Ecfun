\name{readDates3to1}
\alias{readDates3to1}
\title{
  read.csv with Dates in 3 columns 
}
\description{
  \code{\link{read.csv}}, converting 3-column dates 
  into vectors of class \code{Date}.  
}
\usage{
readDates3to1(file, YMD=c('Year', 'Month', 'Day'), 
                ...)
}
\arguments{
  \item{file}{
    the name of a file from which the data are to 
    be read.
  }
  \item{YMD}{
    Character vector of length 3 passed to 
    \code{\link{dateCols}}
  }
  \item{\dots}{
    optional arguments for \code{\link{read.csv}}
  }
}
\details{
  Some files (e.g., from the 
  \href{https://correlatesofwar.org/data-sets/COW-war}{Correlates 
  of War} project) have dates specified in 
  three separate columns with names like 
  \code{startMonth1}, \code{startDay1}, 
  \code{startYear1}, \code{endMonth1}, ..., 
  \code{endYear2}.  This function looks for 
  such triples and replaces each found with 
  a single column with a name like, 
  \code{start1}, \code{end1}, ..., \code{end2}.  

  ALGORITHM

  1.  \code{dat <- read.csv(file, ...)}

  2.  \code{Dates3to1(dat, YMD)}
}
%\source{}
\value{
  a \code{\link{data.frame}} with 3-column 
  dates replace by single-column vectors of class 
  \code{Date}.  
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{read.csv}}
  \code{\link{Dates3to1}}
  \code{\link{dateCols}}
}
\examples{
##
## 1.  Write a file to be read
##
cow0 <- data.frame(rec=1:3, startMonth=4:6, 
    startDay=7:9, startYear=1971:1973, 
    endMonth1=10:12, endDay1=13:15, 
    endYear1=1974:1976, txt=letters[1:3])

cowFile <- tempfile('cow0')    
write.csv(cow0, cowFile, row.names=FALSE)
##
## 2.  Read it 
##
cow0. <- readDates3to1(cowFile)

# check 
cow0x <- data.frame(rec=1:3, txt=letters[1:3], 
    start=as.Date(c('1971-04-07', '1972-05-08', '1973-06-09')), 
    end1=as.Date(c('1974-10-13', '1975-11-14', '1976-12-15')) )

\dontshow{stopifnot(}
all.equal(cow0., cow0x)
\dontshow{)}

\dontshow{
# delete the temp file written above 
file.remove(cowFile)
}
}
\keyword{IO}
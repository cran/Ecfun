\name{deletedFunctions}
\alias{deletedFunctions}
\alias{readFinancialCrisisFiles}
\alias{readCookPVI}
\alias{readCookPVI.}
\alias{readUShouse}
\alias{readUSsenate}
\alias{readUSstateAbbreviations}
\alias{UShouse.senate}
\alias{USsenateClass}
\alias{mergeUShouse.senate}
\title{
  Functions deleted from the Ecfun package
}
\description{
  Several functions were deleted from \code{Ecfun} 
  0.2-5, because they no longer worked, and 
  it was not clear if there was demand for 
  them.  
  
  If you need them, you can get the 
  documentation and code for them from CRAN 
  > Packages > Archive (near the bottom 
  center) > \code{Ecfun} > 
  \code{Ecfun_0.2-0.tar.gz}.  
  I don't expect the code to work.  
  However, I might be willing to 
  collaborate in restoring the 
  functionality to \code{Ecfun}.  
  
  \code{readFinancialCrisisFiles}
  was a companion to a book.  This 
  function required the \code{gdata}
  package, which was scheduled to be 
  removed from CRAN.  
  
  \code{USsenateClass} called by default 
  \code{readUSsenate}.  \code{UShouse.senate} 
  and \code{mergeUShouse.senate} called by 
  default both \code{readUSsenate} and 
  \code{readUShouse}.  The latter two and the 
  remaining functions deleted did web scraping, 
  and the web sites from which they scraped 
  information changed, and it did not seem worth 
  the work required to continue to maintain them.  
}
\keyword{misc}

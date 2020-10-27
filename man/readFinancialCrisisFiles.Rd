\name{readFinancialCrisisFiles}
\alias{readFinancialCrisisFiles}
\title{
  banking crisis data and function to read financial crisis files
}
\description{
  DELETED FUNCTION TO:  
  Read financial crisis data in files described by an
  object of class \code{financialCrisisFiles}. 
  This was designed to read Excel files describing
  financial crises since 1800 downloaded from
  \url{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}.

  \code{bankingCrises} is a \code{data.frame} created by
  \code{readFinancialCrisisFiles()} using 3 files
  downloaded from \url{http://www.reinhartandrogoff.com}
  and 1 obtained from Prof. Reinhart in January 2013.
  
  THIS FUNCTION HAS BEEN MADE INOPERATIVE, BECAUSE 
  IT REQUIRED THE \code{gdata} PACKAGE, WHICH WAS
  SCHEDULED TO BE REMOVED FROM CRAN.  IF THAT 
  HAPPENED, \code{Ecfun} would also be removed, 
  and it wasn't clear that it was worth the effort 
  to rewrite it so it worked without \code{gdata}.  
  If you need it please either get it from a version
  that has it, e.g., \code{Ecfun 0.2-2} and fix it
  or write to the \code{Ecfun} maintainer, and 
  ask that it be fixed.  It's not clear that 
  Reinhart and Rogoff are maintaining their 
  data set.  If not, this function should not be 
  needed.  
  
  THIS HELP PAGE WILL LIKELY BE DELETED FROM A 
  FUTURE VERSION OF \code{Ecfun}.  2020-10-26.  
}
\usage{
readFinancialCrisisFiles(files, crisisType=7, ...)
}
\arguments{
  \item{files}{
    an object of class \code{financialCrisisFiles}.
  }
  \item{crisisType}{
    an integer (vector) between 1 and 8 indicating the
    type of data to be retrieved:  1=independence year
    (not a crisis but an indicator), 2=currency, 
    3=inflation, 4=stock market, 5=domestic sovereign 
    debt crisis, 6=external sovereign debt crisis, 
    7=banking, 8=tally.  ("Type" 1 = year.)  These are 
    all 0 or 1 indicating the presence of the event in 
    the given year.  Type 8 = sum of types 2 through 7.
  }
  \item{\dots}{
    arguments to pass with file and sheet name to
    \code{\link[gdata]{read.xls}} when reading a sheet 
    of an MS Excel file.  This is assumed to be the same
    for all sheets of all files.  If this is not the 
    case, the resulting \code{financialCrisisFiles} 
    object will have to be edited manually before using 
    it to read the data.
  }
}
\details{
  Reinhart and Rogoff provided numerous data sets 
  analyzed in their book, "This Time Is Different:  
  Eight Centuries of Financial Folly".  Of interest 
  here are data on financial crises of various types 
  for 70 countries spanning the years 1800 - 2010. 
  
  These files were removed from the web site for the 
  book some time between 2015-04-19 and 
  2015-04-26.  They are still available from the 
  Internet Archive at
  \url{https://web.archive.org/web/20150419090824/http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}
  
  Sometime between 2019-09-19 and 2020-02-02, the 
  web site for the book itself was inactivated.  
  For the web site as of 2019-09-19 see:  
  \url{https://web.archive.org/web/20190919224018/http://www.reinhartandrogoff.com/}.  This includes a link to 
  "Full Dataset to All Figures and Tables in the Book", 
  which an interested user may wish to consult and 
  compare with the data as of 2015-04-19, cited above.  
  
  The function \code{financialCrisisFiles}
  produces a list of class \code{financialCrisisFiles}
  describing different Excel files in very similar 
  formats with one sheet per Country and a few extra 
  descriptor sheets.  The data object 
  \code{FinancialCrisisFiles} is the default output 
  of that function.

  \code{readFinancialCrisisFiles} reads the sheets for the 
  individual countries.
}
\source{ \url{http://www.reinhartandrogoff.com} }
\value{
  If \code{length(crisisType)} == 1, a 
  \code{data.frame} is returned with the first 
  column being \code{year}, and with one other 
  column containing the data for that 
  \code{crisisType} for each country.

  If \code{length(crisisType)} > 1, a list is
  returned containing a \code{data.frame} for 
  each country.
}
\references{
  Carmen M. Reinhart and Kenneth S. Rogoff (2009) This Time 
  Is Different:  Eight Centuries of Financial Folly, Princeton 
  U. Pr.
}
\author{Spencer Graves}
%\seealso{\code{\link[gdata]{read.xls}},
% \code{\link{financialCrisisFiles}} }
\examples{
##
## Recreate / update the data object BankingCrises
##
library(Ecdat)
%data(FinancialCrisisFiles)
\dontrun{
bankingCrises <- readFinancialCrisisFiles(FinancialCrisisFiles)

##
## Toy example using local data to check the code
## and illustrate returning all the data not just one
## crisisType
##
Ecdat.demoFiles <- system.file('demoFiles', package='Ecdat')
Ecdat.xls <- dir(Ecdat.demoFiles, pattern='xls$',
                 full.names=TRUE)
if(FALSE){
  tst <- financialCrisisFiles(Ecdat.xls)

# optional tests if not CRAN   
  if(!fda::CRAN()){
  bankingCrises.tst <- readFinancialCrisisFiles(tst)
  allCrises.tst <- readFinancialCrisisFiles(tst, 1:8)

# Manually construct tst from allCrises.tst
  tst2 <- data.frame(year=1800:1999)
  tst2$Algeria <- as.numeric(allCrises.tst$Algeria[-(1:12), 8])
  tst2$CentralAfricanRep <- as.numeric(
                    allCrises.tst$CentralAfricanRep[-(1:12), 8])
  tst2$Taiwan <- as.numeric(allCrises.tst$Taiwan[-(1:11), 8])
  tst2$UK <- as.numeric(allCrises.tst$UK[-(1:11), 8])

\dontshow{stopifnot(}
all.equal(bankingCrises.tst, tst2)
\dontshow{)}

# check
data(bankingCrises)
\dontshow{stopifnot(}
all.equal(bankingCrises.tst,
   bankingCrises[1:200, c('year', 'Algeria', 'CentralAfricanRep',
                           'Taiwan', 'UK')])
\dontshow{)}
}
}
}
}
\keyword{datasets}
\keyword{IO}
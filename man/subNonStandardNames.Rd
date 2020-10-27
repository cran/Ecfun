\name{subNonStandardNames}
\alias{subNonStandardNames}
\title{
  sub for nonstandard names
}
\description{
  \code{sub(nonStandardNames[, 1], 
  nonStandardNames[, 2], x)}
  
  Accented characters common in non-English 
  languages often get mangled in different 
  ways by different software.  For example, 
  the "e" in "Andre" may carry an accent that 
  gets replaced by other characters by different 
  software.  
  
  This function first converts \code{"Andr*"} 
  to \code{"Andr_"} for any character "*" not 
  in \code{standardCharacters}.  It then looks 
  for \code{"Andr_"} in \code{nonStandardNames}.  
  By default, it will find that and replace it 
  with "Andre".  
}
\usage{
subNonStandardNames(x,
  standardCharacters=c(letters, LETTERS, ' ', 
    '.', '?', '!', ',', 0:9,   '/', '*', '$', 
    '\%', '\"', "\'", '-', '+', '&', '_', ';', 
    '(', ')', '[', ']', '\n'),
  replacement='_',
  gsubList=list(list(pattern=
        '\\\\\\\\\\\\\\\\|\\\\\\\\',
      replacement='\"')),
  removeSecondLine=TRUE,
  nonStandardNames=Ecdat::nonEnglishNames, 
  namesNotFound="attr.replacement", ...)
}
\arguments{
  \item{x}{
    character vector or matrix or a 
    \code{data.frame} of character vectors in 
    which it is desired replace
    \code{nonStandardNames[, 1]} in 
    \code{subNonStandardCharacters(x, ...)} 
    with the corresponding element of 
    \code{nonStandardNames[, 2]}.
  }
  \item{standardCharacters, replacement, 
    gsubList, \dots}{
      arguments passed to 
      \code{\link{subNonStandardCharacters}}
  }
  \item{removeSecondLine}{
    logical:  If \code{TRUE}, delete anything
    following \code{"\\n"} and return it as 
    an attribute \code{secondLine}.  
  }
  \item{nonStandardNames}{
    \code{data.frame} or character matrix with 
    two columns: 
    Replace any substring of \code{x} matching 
    \code{nonStandardNames[, 1]} with the 
    corresponding element of 
    \code{nonStandardNames[, 2]}
  }
  \item{namesNotFound}{
    character vector describing how to treat 
    substitutions not found in 
    \code{nonStandardNames[, 1]}:  
  
    \itemize{
      \item{
        \code{attr.replacement}: Return an 
        attribute \code{namesNotFound} with 
        \code{grep(replacement, 
          subNonStandardCharacters(...))}, 
        if any.
      }
      \item{\code{attr.notFound}:  
        Return an attribute \code{namesNotFound}  
        with \code{x != 
        subNonStandardCharacters(...)}, 
        if any.
      }
      \item{"print":  Print the elements of 
        \code{x} \code{notFound} per either
        \code{attr.replacement} or 
        \code{attr.notFound}, as requested.
      }  
      \item{
        "":  Do not report any \code{notFound} 
        elements of \code{x}.
      }
    }
    
    NOTE:  x = "_" will be identified by 
    \code{attr.replacement} but not by 
    \code{attr.notfound} assuming the default 
    value for \code{replacement}. 
  }
}
\details{
  1.  \code{removeSecondLine}s

  2.  \code{x. <- subNonStandardCharacters(x, 
  standardCharacters, replacement, ...)}

  3.  Loop over all rows of 
  \code{nonStandardNames} substituting 
  anything matching 
  \code{nonStandardNames[i, 1]} with 
  \code{nonStandardNames[i, 2]}.

  4.  Eliminate leading and trailing blanks.
  
  5.  \code{if(is.matrix(x))} return a matrix;  
  \code{if(is.data.frame(x))} return a 
  \code{data.frame(..., stringsAsFactors=FALSE)} 
  
  NOTE: On 13 May 2013 Jeff Newmiller at the
  University of California, Davis, wrote, 'I 
  think it is a fools errand to think that you 
  can automatically "normalize" arbitrary Unicode
  characters to an ASCII form that everyone will
  agree on.'  (This was a reply on 
  \code{r-help@r-project.org}, subject:  "Re: [R]
  Matching names with non-English characters".) 
  Doubtless someone has software to do a better 
  job of this than what this function does, but 
  I've so far been unable to find it in R.  If 
  you know of a better solution to this problem, 
  I'd be pleased to hear from you.  Spencer Graves
}
\value{
  a character vector with all 
  \code{nonStandardCharacters} replaced first by
  \code{replacement} and then by the second 
  column of \code{nonStandardNames} for any that
  match the first column.  If a \code{secondLine} 
  is found on any elements, it is returned as a
  \code{secondLine} attribute. 
  
  If any names with \code{nonStandardCharacters} 
  are not found in \code{nonStandardNames[, 1]},
  they are identified in an optional attribute 
  per the \code{namesNotFound} argument.    
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{sub}}
  \code{\link[Ecdat]{nonEnglishNames}}
  \code{\link{subNonStandardCharacters}}
  \code{\link[tis]{stripBlanks}}
}
%\references{}
\examples{
##
## 1.  Example 
##
tstSNSN <- c('Raul', 'Ra`l', 'Torres,Raul', 
    'Torres, Ra`l', "Robert C. \\\\Bobby\\\\\\\\", 
    'Ed  \\n --Vacancy', '', '  ')
% '\\' is converted to '\' before testing this in R CMD check            
#  confusion in character sets can create
#  names like Names[2]
##
## 2.  subNonStandardNames(vector)
##
%library(Ecdat)
SNS2 <- subNonStandardNames(tstSNSN)
SNS2

# check 
SNS2. <- c('Raul', 'Raul', 'Torres,Raul', 'Torres, Raul',
            'Robert C. "Bobby"', 'Ed', '', '')
attr(SNS2., 'secondLine') <- c(rep(NA, 5), ' --Vacancy',
        NA, NA)

\dontshow{stopifnot(}
all.equal(SNS2, SNS2.)
\dontshow{)}
##
## 3.  subNonStandardNames(matrix)
##
tstmat <- parseName(tstSNSN, surnameFirst=TRUE)
submat <- subNonStandardNames(tstmat)

# check 
SNSmat <- parseName(SNS2., surnameFirst=TRUE)
\dontshow{stopifnot(}
all.equal(submat, SNSmat)
\dontshow{)}

##
## 4.  subNonStandardNames(data.frame)
##
tstdf <- as.data.frame(tstmat)
subdf <- subNonStandardNames(tstdf)

# check 
SNSdf <- as.data.frame(SNSmat, stringsAsFactors=FALSE)
\dontshow{stopifnot(}
all.equal(subdf, SNSdf)
\dontshow{)}

##
## 5.  namesNotFound 
##
noSub <- subNonStandardNames('xx_x')

# check 
noSub. <- 'xx_x'
attr(noSub., 'namesNotFound') <- 'xx_x'
\dontshow{stopifnot(}
all.equal(noSub, noSub.)
\dontshow{)}
}
\keyword{manip}
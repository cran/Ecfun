\name{trimImage}
\alias{trimImage}
\title{
  Trim zero rows or columns from an object of 
  class \code{Image}.
}
\description{
  Identify rows or columns of a matrix or 
  3-dimensional array that are all 0 and 
  remove them.
}
\usage{
trimImage(x, max2trim=.Machine$double.eps, 
      na.rm=TRUE, returnIndices2Keep=FALSE, 
      ...)
}
\arguments{
  \item{x}{
   a numeric matrix or 3-dimensional array 
   or an object with subscripting defined so
   it acts like such.
  }
  \item{max2trim}{
    a single number indicating the max 
    absolute numeric value to trim.
  }
  \item{na.rm}{
    logical:  If \code{TRUE}, \code{NA}s 
    will be ignored in determining the 
    max absolute value for the row.  If a
    row or column is all \code{NA}, it 
    will be treated as all 0 in deciding 
    whether to trim.

    If \code{FALSE}, any row or column 
    containing an \code{NA} will be 
    retained.
  }
  \item{returnIndices2Keep}{
    if \code{TRUE}, return a list with 2 
    integer vectors giving row and column 
    indices to use in selecting the 
    desired subset of \code{x}.  This 
    allows an array \code{y} to be 
    trimmed to match \code{x}.

    If \code{FALSE}, return the desired 
    trimmed version of \code{x}.

    If this is a list with two two 
    integer vectors, use them to trim 
    \code{x}.
  }
  \item{\dots}{
    Optional arguments;  not currently 
    used.
  }
}
\details{
  1.  Check arguments:  2 <= 
  \code{length(dim(x))} <= 3?  
  \code{is.logical(na.rm)}?  
  \code{returnIndices2Keep} = logical 
  or list of 2 integer vectors, all 
  the same sign, not exceeding 
  \code{dim(x)}?

  2.  \code{if(is.list(returnIndices2Keep))} 
  check that \code{returnIndices2Keep} is a 
  list with 2 integer vectors, all the same 
  sign, not exceeding \code{dim(x)}.
  If yes, return \code{x} appropriately 
  subsetted.

  3.  \code{if(!is.logical(returnIndices2Keep))}
  throw an error message.

  4.  Compute \code{indices2Keep}.

  5.  \code{If(returnIndices2Keep)} return 
  (\code{indices2Keep}) else return \code{x}
  appropriately subsetted.
}
\value{
  \code{if(returnIndices2Keep==TRUE)} return 
  a list with 2 integer vectors to use as 
  subscripts in trimming objects like 
  \code{x}.

  Otherwise, return an object like \code{x}
  appropriately trimmed.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link[raster]{trim}} trims raster 
  images, similar to \code{trimImage}.

  \code{\link{trimws}} trims leading and 
  trailing spaces from character strings 
  and factors.  Similar \code{trim}
  functions exist in other packages but 
  without obvious, explicit consideration 
  of \code{factors}.
}
%\references{}
\examples{
##
## 1.  trim a simple matrix
##
tst1 <- matrix(.Machine$double.eps, 3, 3,
    dimnames=list(letters[1:3], LETTERS[1:3]))
tst1[2,2] <- 1
tst1t <- trimImage(tst1)

# check
tst1. <- matrix(1, 1, 1,
          dimnames=list(letters[2], LETTERS[2]))
\dontshow{stopifnot(}
all.equal(tst1t, tst1.)
\dontshow{)}

##
## 2.  returnIndices2Keep
##
tst2i <- trimImage(tst1, returnIndices2Keep=TRUE)
tst2a <- trimImage(tst1, returnIndices2Keep=tst2i)

tst2i. <- list(index1=2, index2=2)


# check
\dontshow{stopifnot(}
all.equal(tst2i, tst2i.)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(tst2a, tst1.)
\dontshow{)}

##
## 3.  trim 0's only
##
tst3 <- array(0, dim=3:5)
tst3[2, 2:3, ] <- 0.5*.Machine$double.eps
tst3[3,,] <- 1

tst3t <- trimImage(tst3, 0)

# check
tst3t. <- tst3[2:3,, ]

# check
\dontshow{stopifnot(}
all.equal(tst3t, tst3t.)
\dontshow{)}

##
## 4.  trim NAs
##
tst4 <- tst1
tst4[1,1] <- NA
tst4[3,] <- NA

tst4t <- trimImage(tst4)
# tst4o == tst4
tst4o <- trimImage(tst4, na.rm=FALSE)

# check
\dontshow{stopifnot(}
all.equal(tst4t, tst1[2, 2, drop=FALSE])
\dontshow{)}

\dontshow{stopifnot(}
all.equal(tst4o, tst4)
\dontshow{)}

##
## 5.  trim all
##
tst4a <- trimImage(tst1, 1)

tst4a. <- matrix(0,0,0,
     dimnames=list(NULL, NULL))

\dontshow{stopifnot(}
all.equal(tst4a, tst4a.)
\dontshow{)}


}
\keyword{manip}

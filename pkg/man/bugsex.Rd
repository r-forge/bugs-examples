\name{bugsex}
\docType{data}
\alias{bugsex}
\title{Standard BUGS examples}
\description{
This data set gives the canonical names of 62 standard BUGS examples and
indicates the BUGS engines under which the examples can be run.
}
\format{
  A data frame with 62 rows and 4 columns:
  \describe{
    \item{name}{A character vector containing the canonical names of the
      examples as required by \code{runExample}.
    }
    \item{WinBUGS}{Logical vector indicating whether the example can be
      run under WinBUGS.
    }
    \item{OpenBUGS}{Logical vector indicating whether the example can be
      run under OpenBUGS.
    }
    \item{JAGS}{Logical vector indicating whether the example can be
      run under JAGS.
    }
  }
}
\keyword{datasets}
\seealso{\code{\link{runExample}}}

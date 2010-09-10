\name{runExample}
\alias{runExample}

\title{Run standard BUGS example}

\description{This function runs one of the standard BUGS examples stored
  in the \code{bugs-examples} package using its canonical name}
\usage{
runExample(name, engine=c("OpenBUGS", "WinBUGS"))
}
\arguments{
  \item{name}{Canonical name of the example, as given by the
    \code{bugsex} data set.
  }  
  \item{engine}{Name of the engine to use for running the example
  }
}
\value{
  An \code{mcmc.list} object.
}
\seealso{
  \code{\link{bugsex}}
}


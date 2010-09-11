\name{runExample}
\alias{runExample}

\title{Run standard BUGS example}

\description{This function runs one of the standard BUGS examples stored
  in the \code{bugs-examples} package using its canonical name}
\usage{
runExample(example, engine=c("OpenBUGS", "WinBUGS"), exDir)
}
\arguments{
  \item{example}{A list containing sufficient information to run the 
    example. See Details below}
  \item{engine}{Name of the engine to use for running the example}
  \item{exDir}{Name of the directory where the model files will be
    found. By default the \file{examples} directory of the installed
    \pkg{bugs-examples} package is used}
}
\value{
  An list with the following components
  \describe{
  \item{coda}{An \code{mcmc.list} object containing output for the
    monitored variables}
  \item{runtime}{The run time as returned by \code{system.time}}
  \item{engine}{The name of the MCMC engine}
  \item{name}{The name of the example}
  }
}
\details{
  The example list must have the following components
  \describe{
    \item{name}{The name of the example}
    \item{parameters}{A character vector of the names of parameters to
      save}
    \item{nIter}{Number of iterations after burnin}
    \item{nBurnin}{Number of burnin iterations to discard}
    \item{nThin}{Thinning interval to use for storing samples}
    \item{nChain}{Number of parallel chains to run}
  }

  The \code{runExamples} function will search for model files in the
  \code{exDir} directory. These files must have the same base name as
  the example with extensions ``model.txt'', ``data.txt'', and
  ``inits.txt''.

}
\seealso{\code{\link{bugsex}}}


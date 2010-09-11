\name{batchBUGS}
\alias{batchBUGS}

\title{Run OpenBUGS from R in batch mode}

\description{This function takes model, data, starting values, and a
  properly formatted OpenBUGS script file as input and executes OpenBUGS
  returning its output to R.  OpenBUGS execution occurs in a temporary
  directory with a unique name to avoid conflicts if multiple OpenBUGS
  runs are initiated from the same directory.}

\usage{
batchBUGS(modelFile, data, inits=NULL, cmdTemplate, 
          numChains=1, digits=5, 
          baseDir=getwd(), workName="temp", index= 1, 
          codaStem=FALSE,
          delWorkFolders=TRUE,
          stripComments=TRUE, seed=NULL,
          useWINE = FALSE, WINE = NULL, newWINE = FALSE, WINEPATH = NULL)
}
\arguments{
  \item{modelFile}{Filename of the BUGS model code. 
    A full path to the model is needed unless the
    file is saved in the \code{baseDir} directory.} 
  
  \item{data}{A list with R variables to become the input data for the
    BUGS model.  The list variable names must be explicitly assigned in
    the list and correspond to the names in the BUGS modelFile.
    Alternatively, \code{data} can be the name of an external file (full
    path if not in the \code{baseDir} directory) with the input data in
    a valid BUGS data format.  The data are written to the file
    'data.txt' in the BUGS execution directory.}

  \item{inits}{A list of starting values for the BUGS model, or a
    function creating initial values. The inits are written to files
    with names 'inits1.txt' ... "initsmax.txt" in the the BUGS execution
    directory. Each set of starting values must be in a list, and the
    starting lists must be contained in a list, even if there is a
    single list of starting values (i.e., a list with 1 list element).
    Alternatively, inits can be a character vector of length numChains,
    with the names of files containing the starting lists.  If inits is
    NULL, no init files are written and the script file must contain a
    generate inits command.}

  \item{cmdTemplate}{Filename of the BUGS script meta-commands. A
    full pathname is needed unless the file is in the \code{baseDir}
    directory.  A sample template is included below.  It is important to
    include modelQuit('yes') as the last command when executing in
    Windows, or OpenBUGS will not return control to R.  In linux, the
    final command must be modelQuit().  If OpenBUGS encounters a trap in
    Windows, control of execution will not return to R until an 'esc' is
    typed in R.  If OpenBUGS encounters a trap in linux, control returns
    to R.}
  
  \item{numChains}{Number of Markov chains (default:1). If this number
    is not equal to the default, corresponding changes have to made in the
    BUGS command template.}

  \item{digits}{Number of significant digits used for BUGS data input
    (default=5). It has no effect if data or inits files are supplied.}

  \item{baseDir}{The default is the current R work directory, but the
       default search path for input files can be specified.}

  \item{workName}{The name of the directory for all BUGS input and
    output (execution directory).  The name is appended to
    \code{baseDir} and the directory is created.  If the directory
    already exists, it is reused without warning.  The default is
    'temp'.}

  \item{index}{An integer combined with a data/time stamp appended to
    the name of \code{BUGSWorkName} so it has a unique identifiable name
    with Monte Carlo iterations.  When Index=0, no number is appended.}

  \item{codaStem}{When FALSE, the coda output are read and returned as a
    \code{coda} class R object.  When TRUE, the names of the coda output
    are returned which can be read by the function \code{read.openbugs}
    in the coda R package.}

  \item{delWorkFolders}{Logical, whether to delete the temporary
    folder (default =TRUE).}

  \item{stripComments}{When TRUE, comments are removed from the model
    and data files.  Versions 3.0.3 and earlier of OpenBUGS fail when
    executed in linux if comments are present.  The problem has been
    corrected in later versions.}

  \item{seed}{A seed for the random number generator can be
    specified.  The value of the seed must be one of 1, 2, 3, ..., 14.
    OpenBUGS uses the same starting seed by default (1) for all models,
    so it is not required to set the seeds, but it does make
    reproducibility more explicit.  Note that seed set commands must be
    added to the BUGS command template as indicated below.  Note also
    that setting seeds is not the same as setting initial values for
    different chains.}

  \item{useWINE}{logical; attempt to use the Wine emulator}

  \item{WINE}{character, path to \file{wine} binary file, it is tried
    hard (by a guess and the utilities \code{which} and \code{locate})
    to get the information automatically if not given.}

  \item{newWINE}{Use new versions of Wine that have \file{winepath}
    utility}

  \item{WINEPATH}{character, path to \file{winepath} binary file, it is
    tried hard (by a guess and the utilities \code{which} and
    \code{locate}) to get the information automatically if not given.}
}

\value{
  A list with two elements.  The first is codaCreated, which is 1
  if CODA files were successfully created, and 0 otherwise.  The second
  element, named codaOut is a mcmc.list ("coda") object containing the
  simulated values of the parameters monitored in the BUGS simulation.
  Alternatively, the second element can be a text string with the
  location (in the execution directory, which is not removed) of the
  output coda ascii files.  The codaOut object is a text string with the
  temporary directory (not removed) containing the input files if BUGS
  failed to create coda files.
}

\note{
  Before executing \code{batchBUGS}, place the model file and the BUGS
  command template in the R working directory. Otherwise, the full path
  to their locations must be explicitly specified in the arguments.

  Filenames in the BUGS command script must have a pre-specified
  symbolic pathname.  The user must give a symbolic path named $TEMPDIR
  in the command script, and this symbolic name is replaced with the
  full path name by batchBUGS.  The seed can also be set by including
  the symbolic name $SEED.  An example command script and model file for
  use with the example R code follow.  The code below must be stored in
  the R working directory with names "cmds.txt" and "emaxmodel.txt" to
  execute the example in the example section.

\preformatted{
cmds.txt

modelCheck("$TEMPDIR/model.txt")
modelData("$TEMPDIR/data.txt")
modelCompile(3)
modelInits("$TEMPDIR/inits1.txt",1)
modelInits("$TEMPDIR/inits2.txt",2)
modelInits("$TEMPDIR/inits3.txt",3)
modelSetRN($SEED)
modelGenInits()
modelUpdate(1000)
samplesSet("e0")
samplesSet("emax")
samplesSet("ed50")
samplesSet("lambda")
samplesSet("sigma")
modelUpdate(1000)
samplesCoda("*","$TEMPDIR/")
modelSaveLog("$TEMPDIR/log.txt")
modelQuit('yes')

emaxmodel.txt

model {
   for (i in 1:N) { 
       y[i] ~ dnorm(emx[i], tau) 
       emx[i] <- e0 +    
       (emax*pow(dose[i],lambda))/(pow(ed50,lambda)+pow(dose[i],lambda))  
   } 
   e0 ~ dnorm(0, 0.04) 
   emax  ~ dnorm(10, 0.01) 
   tau ~ dgamma(0.01, 0.001) 
   ed50~ dgamma(1.15, 0.02) 
   p~dgamma(1.5,0.8333333) 
   lambda<- 0.5+p 
   sigma <- 1 / sqrt(tau) 
   y100mg<-(emax * pow(100,lambda))/(pow(ed50,lambda)+pow(100,lambda)) 
} 


}} 

\details{ 

  The BRugs package does not currently work properly in linux, and it
  executes an old version of OpenBUGS in windows.
  \code{batchBUGS}executes OpenBUGS from R using system calls that
  automatically detect the operating system.

  \code{batchBUGS} also executes under WINE for use on Apple Computers.
  One objective is to execute BUGS from R in all of these computing
  environments with a minimal number of changes.
  
  The user must supply a BUGS script meta-file supplying the OpenBUGS
  commands that correspond to menu selections.  All input files should
  be stored in the R working directory, or full pathnames must be
  supplied.  OpenBUGS includes documentation for the scripting commands.

  BatchBUGS creates uniquely named directories for BUGS execution needed
  for parallel executions common with Monte Carlo simulation. By
  default, these work directories are not saved.

  Older versions of OpenBUGS(<=3.0.3) fail when executed under linix if
  a model or data file contains comments.  The batchBUGS function
  removes all comments and places copies of these modified files in the
  BUGS working directory by default.

  The \code{batchBUGS} code includes the \code{bugsData},
  \code{bugsInits}, \code{write.datafile}, and \code{formatdata}
  functions from the \pkg{BRugs} package.  It includes the
  \code{winedriveMap}, \code{winedriveTr}, \code{winedriveRTr},
  \code{win2native}, and \code{native2win}, functions from the
  \pkg{R2WinBUGS} package.
  
}
\seealso{
  \code{\link{read.openbugs}}
}

\examples{
\dontrun{
##### create files cmds.txt and emaxmodel.txt from the note section

doselev<-c(0,5,25,50,100)
n<-c(78,81,81,81,77)
dose<-rep(doselev,n)

### population parameters for simulation
e0<-2.465375 
ed50<-67.481113 
emax<-15.127726
sdy<-7.967897
pop.parm<-c(ed50,e0,emax)    
meanresp<-e0+ emax*dose/(dose+ed50)
N<-sum(n)

inits <- list(list(e0 =0, emax=10, ed50=25, tau = .5, p=1.5),
              list(e0 =5, emax=20, ed50=10, tau = .1, p=2),
              list(e0 =5, emax=5, ed50=50, tau = 1, p=1))

set.seed(12357)

y<-rnorm(N,meanresp,sdy)

##### successful run

outb <- batchBUGS('emaxmodel.txt', data=list(N=N,y=y,dose=dose),
                  inits=inits, 'cmds.txt', 
                  numChains=3, digits=5, 
                  delWorkFolders=FALSE, seed=2,
                  stripComments=FALSE)
outb[[1]]

summary(outb[[2]])

}}
\author{Neal Thomas, Jim Rogers, Khriss Toto}
%\keyword{humbleBRugsFit, BRugs, Bugs,  Linux}

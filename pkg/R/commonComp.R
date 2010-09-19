# commonCompBillG.R 4/9/2009 -- William Gillespie, Metrum Institute
# Modified from commonComp.R by Neal Thomas & Jim Rogers

# Applies WinBUGS 1.4.3 and OpenBUGS to the examples provided with
# OpenBUGS. The script uses the ASCII text versions of the model, data
# and initial estimate files. The numbers of iterations for burn-in
# and analysis are the same (or should be) as those reported in the
# example documentation. The results are saved in subdirectories of
# the working directory.

# commonCompBillG.R differs from commonComp.R primarily by adding the
# ability to run under Mac OS X with Wine. It should also be possible
# to run this in other Unix flavors with minor changes.


read.bugsdata <- function(file)
{
    bugs.dat <- dget(file)
    for (n in names(bugs.dat)) {
        if (!is.null(dim(bugs.dat[[n]]))) {
            dim(bugs.dat[[n]]) <- rev(dim(bugs.dat[[n]]))
            bugs.dat[[n]] <- aperm(bugs.dat[[n]])
        }
    }
    return(bugs.dat)
}

fixDirichlet <- function(modelFile, pattern)
{
    ## Substitute "ddirich" for "ddirch" in model file.

    m <- readLines(modelFile, warn=FALSE)
    if (length(grep("ddirch", m)) > 0) {
        modelFile <- tempfile(pattern)
        m <- gsub("ddirch", "ddirich", m)
        writeLines(m, modelFile)
    }
    return(modelFile)
}

fixTruncation <- function(modelFile, pattern)
{
    ## Substitute I(,) WinBUGS construct for the T(,) JAGS construct,
    ## but only if the parameters of the distribution and the I(,)
    ## construct are numeric constants.

    ## Essentially this usage is restricted to truncating prior distributions,
    ## e.g. X ~ dnorm(0.0, 1.0E-3) I(0, )
    ## Under these circumstances, both I and T work the same way.

    m <- readLines(modelFile, warn=FALSE)
    ## Note that this pattern matches expressions that are not valid
    ## BUGS expressions, but the BUGS parser will take care of them
    expr <- "(\\([ \\.0-9,Ee-]*\\))[[:space:]]*I[[:space:]]*(\\([ \\.0-9Ee-]*,[ \\.0-9Ee-]*\\))"
    if (length(grep(expr, m)) > 0) {
        modelFile <- tempfile(pattern)
        m <- gsub(expr, "\\1 T\\2", m)
        writeLines(m, modelFile)
    }
    return(modelFile)
}

runWinBUGS <- function(modelFile, modelData, inits, example, n.chains)
{
    wkDir <- file.path(getwd(), "WinBUGS", example$name)
    unlink(wkDir, recursive=TRUE)
    dir.create(wkDir, showWarnings=FALSE)
    file.copy(modelData, file.path(wkDir,"data.txt"), overwrite=TRUE)

    useWINE <- getOption("useWINE")
    if (is.null(useWINE)) {
        ## There is no native WinBUGS 1.4 on Unix
        useWINE <- .Platform$OS.type == "unix"
    }
    if (useWINE) {
        wine.dir <- getOption("wineBin")
        if (is.null(wine.dir))
            stop("Cannot use wine: option \"wineBin\" not set")
    }
    bugs.dir <- getOption("WinBUGSDir")
    if (is.null(bugs.dir))
        stop("Option \"WinBUGSDir\" not set")

    nBurnin <- example$nBurnin
    nSample <- example$nSample
    nThin <- example$nThin

    runtime <-
        system.time(bugs(data="data.txt",
                         inits=inits,
                         parameters.to.save=example$parameters,
                         model.file=modelFile,
                         DIC=FALSE,
                         n.chains=n.chains,
                         n.iter=(nSample + nBurnin)*nThin,
                         n.burnin=nBurnin*nThin,
                         n.thin=nThin,
                         clearWD=FALSE,
                         program="WinBUGS",
                         codaPkg=TRUE,
                         working.directory=wkDir,
                         bugs.directory = bugs.dir,
                         useWINE=useWINE,
                         WINE=file.path(wine.dir,"wine"),
                         newWINE=useWINE,
                         WINEPATH=file.path(wine.dir, "winepath"))
                    )

    codafiles <- file.path(wkDir, paste("coda",1:n.chains,".txt",sep=""))
    ans <- vector("list", n.chains)
    for (i in seq_along(ans)) {
        ans[[i]] <- try(read.coda(codafiles[i],
                                  file.path(wkDir, "codaindex.txt"),
                                  quiet=TRUE), silent=TRUE)
        if (inherits(ans[[i]], "try-error")) {
            ans <- NULL
            break
        }
    }

    out.coda <- if(is.null(ans)){
        NULL
    }
    else {
        mcmc.list(ans)
    }

    return(list("coda" = out.coda, "runtime" = runtime, engine="WinBUGS",
                name = example$name))
}

dojags <- function(modelFile, modelData, inits, example, n.chains)
{
    nThin <- example$nThin
    nBurnin <- example$nBurnin * nThin
    nUpdate <- example$nSample * nThin

    if (is.null(inits)) {
        ##FIXME. Allow NULL initial values in jags.model
        m <- jags.model(modelFile, data=read.bugsdata(modelData),
                        n.chains=n.chains, n.adapt=0)
    }
    else {
        m <- jags.model(modelFile, data=read.bugsdata(modelData),
                        inits=inits, n.chains=n.chains, n.adapt=0)
    }
    ## Burnin
    ## Following the lead of runOpenBUGS we take the whole of the
    ## burnin period for adaptation.  For non-adapting models a call to
    ## adapt will do nothing, so we have to explicitly call update to
    ## complete the burnin
    adapt(m, nBurnin, progress.bar="none")
    if (m$iter() == 0)
        update(m, nBurnin, progress.bar="none")
    ## Sample monitoring
    coda.samples(m, example$parameters, n.iter=nUpdate, thin = nThin,
                 progress.bar="none")
}

runJAGS <- function(modelFile, modelData, inits, example, n.chains)
{
    modelFile <- fixTruncation(modelFile, example$name)
    if (any(example$parameters == "deviance")) {
        load.module("dic")
    }

    runtime <- system.time(out.coda <- dojags(modelFile, modelData, inits,
                                              example, n.chains))
    return(list("coda" = out.coda, "runtime" = runtime, engine="JAGS",
                name = example$name))
}

writeTemplate <- function(scriptfile, example, n.chains)
{
    ## Writes a template script file for OpenBUGS. The variable
    ## $TEMPDIR, which gives the working directory, is substituted in
    ## the call to batchBUGS

    nAdapt <- max(0, example$nBurnin - 1)
    nThin <- example$nThin
    nBurn <- example$nBurnin
    nUpdate <-example$nSample
    params <- example$parameters
    sep <- .Platform$file.sep

    initfiles <- file.path("$TEMPDIR/", paste("inits",1:n.chains,".txt",sep=""))

    cat("modelCheck('$TEMPDIR",sep,"model.txt')\n",
        "modelData('$TEMPDIR",sep,"data.txt')\n",
        "modelCompile(",n.chains,")\n",
        paste("modelInits('",initfiles,"',",1:n.chains,")\n", sep=""),
        "modelGenInits()\n",
        "modelSetAP('chain graph block hybrid'",",",nAdapt,")\n",
        "modelSetAP('normal block hybrid'",",",nAdapt,")\n",
        "modelSetAP('logit/log-linear block hybrid'",",",nAdapt,")\n",
        "modelSetAP('normal blcok hybrid'",",",nAdapt,")\n",
        "modelSetAP('block hybrid'",",",nAdapt,")\n",
        "modelSetAP('non conjugate dirichlet'",",",nAdapt,")\n",
        "modelSetAP('slice'",",",nAdapt,")\n",
        "modelSetAP('random walk (delayed) metropolis'",",",nAdapt,")\n",
        "modelSetAP('random walk metropolis'",",",nAdapt,")\n",
        "modelSetAP('descrete metropolis'",",",nAdapt,")\n",
        "modelSetAP('hybrid metropolis'",",",nAdapt,")\n",
        "modelSetAP('over-relaxed metropolis'",",",nAdapt,")\n",
        "modelSetAP('descrete slice'",",",nAdapt,")\n",
        "modelUpdate(",nBurn,",",nThin,")\n",
        paste("samplesSet('", params, "')\n", sep=""),
        "modelUpdate(",nUpdate,",",nThin,")\n",
        "samplesCoda('*','$TEMPDIR",sep,"')\n",
        ifelse(.Platform$OS.type == "windows",
               paste("modelSaveLog('$TEMPDIR",sep,"log.txt')\n",sep=""),"#\n"),
        ifelse(.Platform$OS.type == "windows",
               "modelQuit('yes')\n","modelQuit()\n"),
        file=scriptfile, sep="")
}

runOpenBUGS <- function(modelFile, modelData, inits, example, n.chains)
{
    wkDir <- file.path(getwd(), "OpenBUGS", example$name)
    scriptfile <- file.path(wkDir, "script.txt")
    writeTemplate(scriptfile, example, n.chains)

    modelFile <- fixDirichlet(modelFile, example$name)

    useWINE <- getOption("useWINE")
    if (is.null(useWINE)) {
        useWINE <- FALSE
    }
    if (useWINE) {
        if(is.null(getOption("wineBin"))) {
            stop("Cannot use wine: wine binary not found")
        }
    }

    runtime <-
        system.time(ans <-
                    batchBUGS(modelFile,
                              modelData,
                              inits,
                              scriptfile,
                              baseDir=file.path(getwd(),"OpenBUGS"),
                              workName=example$name,
                              index= 0, codaStem=TRUE,
                              delWorkFolders=FALSE,
                              stripComments=FALSE,
                              useWINE=useWINE,
                              WINE=file.path(getOption("wineBin"),"wine"),
                              newWINE=useWINE,
                              WINEPATH=file.path(getOption("wineBin"),
                              "winepath"))
                    )

    out.coda <- if(!ans[[1]]){
        NULL
    }
    else {
        read.openbugs(ans[[2]], quiet=TRUE)
    }

    return(list("coda" = out.coda, "runtime" = runtime, engine="OpenBUGS",
                "name" = example$name))
}

runExample <- function(example, engine=c("OpenBUGS", "WinBUGS", "JAGS"), exDir)
{
    engine <- match.arg(engine)

    ## Set up working directory
    if (engine != "JAGS") {
        wkDir <- file.path(getwd(), engine, example$name)
        if (file.exists(wkDir))
            unlink(wkDir, recursive = TRUE)
        dir.create(wkDir, recursive = TRUE, showWarnings = FALSE)
    }

    ## Find model files
    if (missing(exDir))
        exDir <- system.file("examples", package="BUGSExamples")

    ## Some models need to be rewritten for JAGS syntax, e.g. if they
    ## contain data transformations, truncation, or censoring. 
    jmodel <- file.path(exDir, paste(example$name, "model-jags.txt", sep=""))
    modelFile <- if (engine == "JAGS" && file.exists(jmodel)) {
        jmodel
    }
    else {
        file.path(exDir, paste(example$name, "model.txt", sep=""))
    }
    dataFile <- file.path(exDir, paste(example$name, "data.txt", sep=""))
    initFile <- file.path(exDir, paste(example$name, "inits.txt", sep=""))
    inits <- if (file.exists(initFile)) {
        list(read.bugsdata(file.path(initFile)))
    }
    else {
        NULL
    }

    switch(engine,
           "WinBUGS"=runWinBUGS(modelFile, dataFile, inits, example, 1),
           "OpenBUGS"=runOpenBUGS(modelFile, dataFile, inits, example, 1),
           "JAGS"=runJAGS(modelFile, dataFile, inits, example, 1)
           )
}


writeResults <- function(out)
{
    ## summarize results and save output

    mkFile <- function(suffix) {
        wkDir <- file.path(getwd(), out$BUGS, out$name)
        file.path(wkDir, paste(out$name, ".", suffix, sep=""))
    }

    if(is.null(out$coda)) {
        write("coda file creation failed", file=mkFile("fail.txt"))
    }
    else {
        sum.coda <- summary(out$coda)
        coda.stats <- if(is.null(dim(sum.coda$statistics))){
            c(sum.coda$statistics, sum.coda$quantiles)
        }
        else {
            cbind(sum.coda$statistics, sum.coda$quantiles)
        }
        write.csv(signif(coda.stats, 5), mkFile("summary.csv"))
        write(out$runtime[1:3], file=mkFile("runtime.txt"))
        out.coda <- out$coda
        save("out.coda", file=mkFile("RData"), env=out)
        pdf(file=mkFile("plots.pdf"), width=6, height=6)
        plot(out$coda)
        dev.off()
    }
}

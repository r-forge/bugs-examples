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

runWinBUGS <- function(modelFile, modelData, inits, example, n.chains)
{
    wkDir <- file.path(getwd(), "WinBUGS", example$name)
    dir.create(wkDir, showWarnings=FALSE)
    file.copy(modelData, file.path(wkDir,"data.txt"), overwrite=TRUE)

    useWINE <- getOption("useWINE")
    if (is.null(useWINE)) {
        useWINE <- .Platform$OS.type == "unix"
    }
    if (useWINE) {
        wine.dir <- getOption("wineBin")
        if (is.null(wine.dir))
            stop("Cannot use wine: option \"wineBin\" not set")
        bugs.dir <- getOption("WinBUGSDir")
        if (is.null(bugs.dir))
            stop("Cannot use wine: option \"WinBUGSDir\" not set")
    }

    runtime <-
        system.time(bugs(data="data.txt",
                         inits=inits,
                         parameters.to.save=example$parameters,
                         model.file=modelFile,
                         DIC=FALSE,
                         n.chains=n.chains,
                         n.iter=example$nIter,
                         n.burnin=example$nBurnin,
                         n.thin=example$nThin,
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
        ans[[i]] <- try(read.coda(codafiles[i], "codaindex.txt",
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

writeTemplate <- function(scriptfile, example, n.chains)
{
    ## Writes a template script file for OpenBUGS. The variable
    ## $TEMPDIR, which gives the working directory, is substituted in
    ## the call to batchBUGS

    nAdapt <- max(0, example$nBurnin - 1)
    nBurn <- ceiling(example$nBurnin/example$nThin)
    nThin <- example$nThin
    nUpdate <-ceiling((example$nIter - nBurn)/nThin)
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
        ### FIXME: need a better way of identifying mac
        useWINE <- (.Platform$OS.type == "unix" &&
                    .Platform$pkgType == "mac.binary")
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

runExample <- function(example, engine=c("OpenBUGS", "WinBUGS"), exDir)
{
    engine <- match.arg(engine)

    ## Set up working directory
    wkDir <- file.path(getwd(), engine, example$name)
    if (file.exists(wkDir))
        unlink(wkDir, recursive = TRUE)
    dir.create(wkDir, recursive = TRUE, showWarnings = FALSE)

    ## Find model files
    if (missing(exDir))
      exDir <- system.file("examples", package="BUGSExamples")

    modelFile <- file.path(exDir, paste(example$name, "model.txt", sep=""))
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
           "OpenBUGS"=runOpenBUGS(modelFile, dataFile, inits, example, 1)
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


###options(error = expression(NULL)) # prevents stopping for errors when running in batch

.onLoad <- function(pkg, lib)
{
    ## Always use WINE on Mac OS X
    if (is.null(getOption("useWINE")) && Sys.info()[1] == "Darwin") {
        options(useWINE = TRUE)
    }
    
    ## Find wine binary directory

    if (is.null(getOption("wineBin"))) {

        wine.search <- c("/usr/local/MacPorts/bin",
                         "/usr/local/bin",
                         "/usr/bin")

        for (i in seq_along(wine.search)) {
            ## Verify existence of both wine and winepath
            wine <- file.path(wine.search[i], "wine")
            winepath <- file.path(wine.search[i], "winepath")
            if (file.exists(wine) && file.access(wine, 1) == 0 &&
                file.exists(winepath) && file.access(winepath, 1) == 0)
            {
                options("wineBin" = wine.search[i])
                break
            }
        }
    }

    ##Find WINE drive
    wine.prefix <- Sys.getenv("WINEPREFIX")
    if (nchar(wine.prefix) == 0)
        wine.prefix <- file.path(Sys.getenv("HOME"), ".wine")

    if (file.exists(wine.prefix) && file.info(wine.prefix)$isdir) {

        cdrive <- file.path(wine.prefix, "drive_c")

        if (is.null(getOption("OpenBUGSDir"))) {
            ## Find latest OpenBUGS installation within wine

            base.path <- file.path(cdrive, "Program Files/OpenBUGS")
            if (file.exists(base.path) && file.info(base.path)$isdir) {
                ob <- list.files(base.path)
                ob <- ob[tolower(substr(ob, 1, 8)) == "openbugs"]
                ob.version <- substring(ob, 9)
                ob.version <- suppressWarnings(as.integer(ob.version))
                if (!all(is.na(ob.version))) {
                    ob.latest <- ob[which.max(ob.version)]
                    options("OpenBUGSDir" = file.path(base.path, ob.latest))
                }
            }
        }


        if (is.null(getOption("WinBUGSDir"))) {
            ## Find WinBUGS 1.4 installation within wine

            wb.path <- file.path(cdrive, "Program Files/WinBUGS14")
            if (file.exists(wb.path) && file.info(wb.path)$isdir) {
                options("WinBUGSDir"="C:/Program Files/WinBUGS14")
            }
        }
    }

}

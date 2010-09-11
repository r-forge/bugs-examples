.onLoad <- function(pkg, lib)
{
    if (is.null(getOption("WinBUGSDir"))) {
        if (file.exists("C:/Program Files/WinBUGS14")) {
            options("WinBUGSDir" = "C:/Program Files/WinBUGS14")
        }
    }
    if (is.null(getOption("OpenBUGSDir"))) {
        ## Try to identify and use the latest OpenBUGS installation
        base.path <- "C:/Program Files/OpenBUGS"
        if (file.exists(base.path) && file.info(base.path)$isdir) {
            ob <- list.files(base.path)
            ob <- ob[tolower(substr(ob, 1, 8)) == "openbugs"]
            ob.version <- substring(ob, from=9)
            ob.version <- suppressWarnings(as.integer(ob))
            if (!all(is.na(ob.version))) {
                ob.latest <- ob[which.max(ob.version)]
                options("OpenBUGSDir" = file.path(base.path, ob.latest))
            }
        }
    }
}


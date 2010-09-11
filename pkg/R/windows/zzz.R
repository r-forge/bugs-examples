.onLoad <- function(pkg, lib)
{
    if (is.null(getOption("WinBUGSDir"))) {
        ## Try to find WinBUGS 1.4 installation
        wb.path <- "C:/Program Files/WinBUGS14"
        if (file.exists(wb.path) && file.info(wb.path)$isdir) {
            options("WinBUGSDir" = wb.path)
        }
    }

    if (is.null(getOption("OpenBUGSDir"))) {
        ## Try to identify and use the latest OpenBUGS installation
        base.path <- "C:/Program Files/OpenBUGS"
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
}


.onLoad <- function(pkg, lib)
{
    wine.search <- c("/usr/local/MacPorts/bin",
                     "/usr/local/bin",
                     "/usr/bin")
    wineBin <- NULL
    
    if (file.exists(file.path(Sys.getenv("HOME"), ".wine"))) {

        for (i in seq_along(wine.search)) {
            if (file.exists(file.path(wine.search[i], "wine"))) {
                wineBin <- wine.search[i]
                break
            }
        }
    }

    if (is.null(getOption("wineBin")))
        options("wineBin"=wineBin)
    if (is.null(getOption("WinBUGSDir")))
        options("WinBUGSDir"="C:/Program Files/WinBUGS14")
}

.onLoad <- function(pkg, lib)
{
    if (is.null(getOption("WinBUGSDir"))) {
        options("WinBUGSDir" = "C:/Program Files/WinBUGS14")
    }
    if (is.null(getOption("WinBUGSDir"))) {
        options("OpenBUGSDir" = "C:/Program Files/OpenBUGS/OpenBUGS307")
    }
}

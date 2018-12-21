#Region "Microsoft.ROpen::95f3a024d841421ef04d9d000e54b03f, Exception.R"

    # Summaries:

    # GetCurrentFunc <- function(offset = 0) {...

#End Region

#' Get the name of current function
#'
#' @description This helper tools is not working
#'    for the closure function.
#'
GetCurrentFunc <- function(offset = 0) {
    stacks <- sys.calls();
    frame  <- length(stacks) - 1 - offset;

    if (frame < 1) {
      name <- "<global>";
    } else {
      name <- stacks[[frame]];
      name <- Strings.Split(toString(name), "\\(")[1];
      name;
    }
}

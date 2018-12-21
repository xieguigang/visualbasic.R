#Region "Microsoft.ROpen::de107d04e22c7af34066172340ba7695, Exception.R"

    # Summaries:

    # GetCurrentFunc <- function() {...

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

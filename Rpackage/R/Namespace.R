#' Print namespace help
#'
#' @description \code{\link{Imports}}
#'
#' @param namespace The VisualBasic namespace
#'
#' @return Nothing
MyHelp <- function(namespace) {
  module <- get(namespace)();
  str(module);
  invisible(NULL);
}

#' List all VisualBasic namespace
#'
#' @return Returns a function name character vector which its name is stared by token like:
#'     \code{Microsoft.VisualBasic}.
MyList <- function() {
  index <- base::system.file("INDEX", package="VisualBasic.R") %=>% readLines %=>% Enumerator;

  # Linq pipeline
  index$
    Where(function(line) InStr(line, "Microsoft.VisualBasic") > 0)$
    Select(function(line) Strings.Split(line)[1])$
    ToArray() %=>% unlist;
}
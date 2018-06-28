#' Parse package DESCRIPTION file
#'
#' @param file \code{DESCRIPTION} file handle
#'
#' @return Returns a list of named value that parsed from the \code{DESCRIPTION}
#'      file in a specific package.
parse.package.description <- function(file = base::system.file("DESCRIPTION", package="VisualBasic.R")) {
    description <- list();
    last <- "";
    name <- "";

    for (line in file %=>% readLines) {
        if (InStr(line, " ") == 1) {
            # line continute
            last <- sprintf("%s %s", last, line %=>% Trim);
        } else {
            if (name != "") {
                description[[name]] = last;
            }
            c(name, last) := GetTagValue(line, ":");
        }
    }

    if (name != "") {
        description[[name]] = last;
    }

    description;
}

DESCRIPTION <- function(packageName) {
    parse.package.description(base::system.file("DESCRIPTION", package=packageName));
}

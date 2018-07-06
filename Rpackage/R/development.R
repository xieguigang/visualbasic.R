#' Parse DESCRIPTION file
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
            last <- sprintf("%s \n%s", last, line %=>% Trim);
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

#' Parse package DESCRIPTION file
#'
#' @param packagename The R package name
#'
#' @description This function is an alias of function
#'     \code{\link{parse.package.description}}
DESCRIPTION <- function(packageName) {
    parse.package.description(base::system.file("DESCRIPTION", package=packageName));
}

#' Load \code{rda} data in a unified method
#'
#' @param rda The \code{*.rda} file path or package data name
#'
xLoad <- function(rdaName, envir = globalenv(), verbose = FALSE) {

  search.path <- c(".", "./data", "../data");

  for(directory in search.path) {
    rda <- sprintf("%s/%s", directory, rdaName);

    if (file.exists(rda)) {
      load(rda, envir = envir);

      if (verbose) {
        printf(" -> load_from_file::%s", rda);
      }

      return(NULL);
    }
  }

  name <- basename(rdaName);
  data(list = name, envir = envir);

  if (verbose) {
    printf(" => data_from_dataset::%s", name);
  }

  invisible(NULL);
}

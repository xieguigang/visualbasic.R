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

  # ./
  if (file.exists(rdaName)) {
    load(rdaName, envir = envir);

    if (verbose) {
      printf(" -> load_from_file::%s", rdaName);
    }

  # ./data/
  } else if (file.exists(sprintf("data/%s", rdaName))) {
    load(sprintf("data/%s", rdaName), envir = envir);

    if (verbose) {
      printf(" -> load_from_file::data/%s", rdaName);
    }

  # ../R/
  # ../data/
  } else if (file.exists(sprintf("../data/%s", rdaName))) {
    load(sprintf("../data/%s", rdaName), envir = envir);

    if (verbose) {
      printf(" -> load_from_file::../data/%s", rdaName);
    }

  } else {
    name <- basename(rdaName);
    data(list = name, envir = envir);

    if (verbose) {
      printf(" -> data_from_dataset::%s", name);
    }
  }

  invisible(NULL);
}

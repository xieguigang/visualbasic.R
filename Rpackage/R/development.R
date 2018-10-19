#Region "Microsoft.ROpen::a36e4beda6f6567a04c09abde8b40321, development.R"

    # Summaries:

    # parse.package.description <- function(file = base::system.file("DESCRIPTION", package="VisualBasic.R")) {...
    # package.version <- function(package = "VisualBasic.R") {if (package %=>% package.is_missing) {...
    # package.is_missing <- function(package) {...
    # package.missing <- function(package) {...
    # DESCRIPTION <- function(packageName) {...
    # xLoad <- function(rdaName, envir = globalenv(), verbose = FALSE) {load.file <- function(rda) {...
    # Eval <- function(module, ...) {if (module %=>% is.list) {...

#End Region

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

#' Get version string for package
#'
#' @description Due to the reason of the systen function \code{package_version} using
#'    integer value for represent the package version string. So this may cause bugs.
#'    This function using string for represent the package version to avoid the bugs.
#'
#'    This function require the package should be loaded into environment before you
#'    calling this function.
#'
#' @param package The package name
#'
#' @return Function returns the package version string value. If the package is not exists,
#'    then this function returns \code{NA} value.
#'
package.version <- function(package = "VisualBasic.R") {
  if (package %=>% package.is_missing) {
    return(NA);
  } else {
    # 2018-08-20

    # Loading required package: package
    #
    # Warning message:
    #   In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,:
    #   there is no package called ‘package’

    # require(package);
  }

  loaded <- sessionInfo()$otherPkgs;
  package <- loaded[[package]];
  package[["Version"]];
}

#' Determine a given package is missing?
#'
#' @param package A package name string
#'
#' @return A logical value to indicate that the given package is missing from
#'    current environment or not?
#'
package.is_missing <- function(package) {
  !(package %in% (installed.packages()[,"Package"] %=>% as.character));
}

#' Determine the missing packages
#'
#' @param package A character vector of the package names.
#'
#' @return Function returns the name list of the missing packages.
#'
package.missing <- function(package) {
  installed <- installed.packages()[,"Package"] %=>% as.character;
  package[!(package %in% installed)];
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
#' @details The file path string value of the \code{rdaName} could be
#' a relative path or an absolute path.
#' When the file path is a relative path, then the R script will try to
#' search the given rda file in directories:
#'
#' \enumerate{
#'   \item \code{.} Current workspace.
#'   \item \code{./data} The \code{data} folder in the current workspace.
#'   \item \code{../data} The \code{data} folder in current workspace's parent directory.
#' }
#'
xLoad <- function(rdaName, envir = globalenv(), verbose = FALSE) {

  load.file <- function(rda) {
    load(rda, envir = envir);

    if (verbose) {
      printf(" -> load_from_file::%s", rda);
    }

    NULL;
  }

  if (file.exists(rdaName)) {
    # Try it as absolute path at first
    return(rdaName %=>% load.file);
  }

  # And then search as relative path mode
  # If the file is not found in absolute path mode.
  search.path <- c(".", "./data", "../data");

  for(directory in search.path) {
    rda <- sprintf("%s/%s", directory, rdaName);

    if (file.exists(rda)) {
      return(rda %=>% load.file);
    }
  }

  # It is a package internal data file.
  name <- basename(rdaName);
  data(list = name, envir = envir);

  if (verbose) {
    printf(" => data_from_dataset::%s", name);
  }

  invisible(NULL);
}

#' list module helper
#'
#' @param module A function to create a \code{list} object a \code{list} object.
#'
#' @return If the module argument is a list, then it will returns directly,
#'   or the function will be evaluate for produce a list value.
Eval <- function(module, ...) {
  if (module %=>% is.list) {
    module;
  } else {
    module(...);
  }
}

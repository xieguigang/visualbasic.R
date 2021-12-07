#Region "Microsoft.ROpen::2ad7e04f8e4564c3c9aaf4e416f44cbd, Global.R"

    # Summaries:

    # Imports <- function(namespace, overrides = FALSE, silent = TRUE, frame = parent.frame()) {...
    # MyHelp <- function(namespace) {...
    # MyList <- function() {...
    # global <- function(name = NULL, value = NULL) {...
    # global <- function(name) {...
    # "global<-" <- function(...) {...
    # Push <- function(envir = parent.frame()) {...
    # IsNothing <- function(x, stringAsFactor = FALSE) {...
    # Size <- function(x) {...
    # printf <- function(...) {...
    # LogException <- function(ex, logFile, append = FALSE) {...
    # GetType <- function(x) {...
    # primitiveTypes <- function() {...
    # using <- function(proc) {...

#End Region

#' Set variable in global
#'
#' @description Set variables into the global environment.
#'
#' @seealso \code{\link{Push}}
#'
#' @examples
#' Set(a = 500, b = TRUE, c = list(a= 50, b = FALSE));
#' list(x11 = 15555, y22 = FALSE) %=>% Set
Set <- function(...) (globalenv() %=>% Push)(...);

#' Set variable in global
#'
#' @description Set variables into the global environment.
#'
#' @examples global(name, value);
#'
#' @return \code{NULL}
#'
# global <- function(name = NULL, value = NULL) {
#   assign <- list(name, value);
#   do.call(`=`, assign, envir = globalenv());
#   invisible(NULL);
# }

#' Get variable in global environment
#'
#' @param name The variable name string that going to
#'             get its value in global environment.
#'
#' @return The variable value of the given variable name. If the variable is
#'         not exists in global environment, then value \code{NULL} will be
#'         return.
global <- function(name) {
  if (!base::exists(name, envir = .GlobalEnv)) {
    NULL;
  } else {
    base::get(name, envir = .GlobalEnv);
  }
}

#' Set variable in global
#'
#' @description Set variables into the global environment.
#'
#' @examples global(name) <- value;
#'
#' @return \code{NULL}
#'
"global<-" <- function(...) {
  # https://stackoverflow.com/questions/10449366/levels-what-sorcery-is-this
  #

  assign <- list(...);
  do.call(`=`, assign, envir = .GlobalEnv);
  name <- assign[[1]];
  name;
}

#' Push variable to a given environment
#'
#' @return This function returns a lambda function that can used for assign variable value
#'    to a given environment.
#'
#' @details For push to current environment, then you can using code for assign \code{envir}
#'   parameter: \code{curEnv=environment()}
Push <- function(envir = parent.frame()) {
  function(...) {
    x <- list(...);

    if ((length(x) == 1)                      &&
        (GetType(x) == primitiveTypes()$list) &&
        (names(x) %=>% IsNothing)) {

      x <- x[[1]];
    }

    for (var in names(x)) {
      assign <- list(var, x[[var]]);
      do.call(`=`, assign, envir = envir);
    }

    invisible(NULL);
  }
}

#' Determine that target is Nothing in VB way
#'
#' @description Determine that target is Nothing in VB way.
#'              \code{is.null} or \code{is.na} or \code{length} equals to ZERO?
#'              All of these situation will be treated as Nothing.
#'
#' @param x Object with any type for determine that its value is null
#'           or empty or not?
#' @param stringAsFactor A logical flag that indicated that should treated
#'                       the string value like \code{NULL}, \code{NA}, etc
#'                       as Nothing?
#'
#' @details Determine that target object is nothing or not, by using predicates: \code{is.null},
#'    \code{is.na}, \code{length(x) == 0}. And if the function parameter \code{stringAsFactor} is
#'    true, then string comparision for \code{x == ""}, \code{x == "NULL"}, \code{x == "NA"} will
#'    be applied.
#'
#' @seealso This function has an alias name: \code{\link{is.nothing}}.
#'
IsNothing <- function(x, stringAsFactor = FALSE) {

  if (is.null(x) || length(x) == 0) {
    TRUE;
  } else if (length(x) == 1 && is.na(x)) {
    # fix bugs for the c(NA, NA, NA, NA) is true.
    TRUE;

    # vector have multiple elements, is not nothing
  } else if (length(x) > 1) {
    FALSE;

    # 2018-6-25 Empty string object can not compare with S4 directly.
    # So this function will determine the data type of X at first and
    # then perfermance the compares.
    # Error in x == "" : Only compares (1)vector or list type
  } else if (!is.character(x)) {
    FALSE;
  } else {

    if (!stringAsFactor) {
      FALSE;
    } else if (x == "") {
      TRUE;
    } else {
      x %in% c("NULL", "null", "na", "NA");
    }
  }
}

#' Determine that target is Nothing in VB way
#'
#' @seealso \code{\link{IsNothing}}
#'
#' @details This function is an alias of the \code{\link{IsNothing}} function.
#'
is.nothing <- function(...) IsNothing(...);

#' Returns the object size
#'
#' @param x R object in any type
#'
#' @return A list with element: \code{rows} and \code{cols} to indicate the object size.\cr\cr
#'
#' \enumerate{
#'   \item for x is \code{data.frame}, these two element value will be \code{\link{nrows}} and \code{\link{ncols}}\cr
#'   \item for x is \code{list} or \code{vector}, these two element value will be
#'         \code{rows = 1} and \code{cols = \link{length}(x)}\cr
#'   \item for x is object like S4 class, these two element value will be \code{[1,1]}\cr
#' }
#'
Size <- function(x) {
  type <- GetType(x);

  if (type == primitiveTypes()$data.frame) {
    list(rows = nrows(x), cols = ncols(x));
  } else if (type == primitiveTypes()$list || type == primitiveTypes()$vector) {
    list(rows = 1, cols = length(x));
  } else {
    list(rows = 1, cols = 1);
  }
}

#' Simulate the C \code{printf} function
#'
#' @param ... Function parameter for function \code{\link{sprintf}}
#'
printf <- function(...) {
  cat(sprintf(...));
  cat("\n");

  invisible(NULL);
};

#' Logging error log file.
#'
#' @param ex Can be error exception or error string text
#' @param logFile File path for save the log data
#' @param append a logical flag that indicate clear the content
#'               before write the log file or not.
#'
#' @return Nothing
LogException <- function(ex, logFile, append = FALSE) {
  dir <- dirname(logFile);

  if (!file.exists(dir)) {
    dir.create(dir, recursive = TRUE);
  }

  if (is.character(ex)) {
    msg <- ex;
  } else {
    msg <- toString(ex);
  }

  cat(msg,
      file   = logFile,
      sep    = "\n",
      append = append
  );

  invisible(NULL);
}

#' Determine the R object type
#'
#' @description This function returns on of the member value from enumeration function
#' \code{\link{primitiveTypes}}
#' based on the type of the variable \code{x}
#'
#' @param x Any variable
#'
#' @return One of the value member from enumeration function
#'         \code{\link{primitiveTypes}}.
GetType <- function(x) {
  types <- primitiveTypes();

  if (is.data.frame(x) || is.matrix(x)) {
    types$data.frame;
  } else if (is.list(x)) {
    types$list;
  } else if (is.vector(x)){
    types$vector;
  } else {
    types$object;
  }
}

#' Enumerate some primitive type in R
#'
#' @description Enumerate some primitive type in R language, these enumeration value have:
#' \enumerate{
#' \item \code{object} = 0
#' \item \code{data.frame} = 1
#' \item \code{list} = 2
#' \item \code{vector} = 3
#' }
primitiveTypes <- function() {
  list(object = 0, data.frame = 1, list = 2, vector = 3);
}

#' Auto dispose
#'
#' @param proc This proc object should contains
#'    a \code{dispose} function for release resource and
#'    a \code{invoke} function to run your code.
#'
using <- function(proc) {
  out <- proc$invoke();
  proc$dispose();

  out;
}

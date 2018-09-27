#Region "Microsoft.ROpen::44f52c4fd3034a98592508489ae64b76, Global.R"

    # Summaries:

    # Imports <- function(namespace, overrides = FALSE, silent = TRUE, frame = parent.frame()) {...
    # MyHelp <- function(namespace) {...
    # MyList <- function() {...
    # global <- function(name = NULL, value = NULL) {...
    # Push <- function(envir = parent.frame()) {function(...) {...
    # IsNothing <- function(x, stringAsFactor = FALSE) {if (is.null(x) || is.na(x) || length(x) == 0) {...
    # Size <- function(x) {...
    # printf <- function(...) {...
    # LogException <- function(ex, logFile, append = FALSE) {...
    # GetType <- function(x) {...
    # primitiveTypes <- function() {...

#End Region

#' VisualBasic namespace imports helper
#'
#' @param namespace The namespace function name
#' @param overrides A logical flag to indicate that should the imported function
#'                  can overrides the previous function which they have the same
#'                  name. By default is can not. If this parameter is set to true,
#'                  then a warning message will be generated for mention you that
#'                  which functions are overrided.
#' @param frame The envrionment location, by default is current function scope
#'
#' @return A string vector contains all of the function names from this namespace
#'         function that imported to current environment.
Imports <- function(namespace, overrides = FALSE, silent = TRUE, frame = parent.frame()) {
	module <- get(namespace)();

	if (!("methods" %in% names(module))) {
		func.list <- module;
	} else {
		func.list <- module$methods;
	}

	overrideMsg <- "overrides '%s' from namespace `%s`";

	for (name in names(func.list)) {
		if (exists(name, envir = frame)) {
			if (overrides) {
				warning(sprintf(overrideMsg, name, namespace));
			} else {
				next;
			}
		}

		assign <- list(name, func.list[[name]]);
		do.call(`=`, assign, envir = frame);
	}

	if ("modules" %in% names(module)) {
	  # Is also contains export variable modules
    modules <- module$modules;

    for(name in names(modules)) {
      if (exists(name, envir = frame)) {
        if (overrides) {
          warning(sprintf(overrideMsg, name, namespace));
        } else {
          next;
        }
      }

      assign <- list(name, modules[[name]]);
      do.call(`=`, assign, envir = frame);
    }
	}

	if (!silent) {
		cat(sprintf("Imports VisualBasic.R::{%s}\n\n", namespace));
		print(names(func.list));
	}

	# invisible(NULL);
	if (silent) {
	  invisible(NULL);
	} else {
	  names(func.list);
	}
}

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

global <- function(name = NULL, value = NULL) {
  assign <- list(name, value);
  do.call(`=`, assign, envir = globalenv());
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

	if (is.null(x) || is.na(x) || length(x) == 0) {
		TRUE;

		# 2018-6-25 空字符串无法直接和S4对象进行比较
		# 所以下面会需要先进行一次类型比较再判断空字符串
		# Error in x == "" : 只能比较(1)基元或串列种类
	} else if (!is.character(x)) {
		FALSE;
	} else {

		if (!stringAsFactor) {
			FALSE;
		} else if (x == "") {
			TRUE;
		} else {
			return (x %in% c("NULL", "null", "na", "NA"));
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
#'         for x is \code{data.frame}, these two element value will be \code{\link{base::nrows}} and \code{\link{base::ncols}}\cr
#'         for x is \code{list} or \code{vector}, these two element value will be \code{rows = 1} and \code{cols = \link{length}(x)}\cr
#'         for x is object like S4 class, these two element value will be \code{[1,1]}\cr
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

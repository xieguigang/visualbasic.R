#' VisualBasic namespace imports helper
#'
#' @param namespace The namespace function name
#' @param overrides A logical flag to indicate that should the imported function
#'                  can overrides the previous function which they have the same
#'                  name. By default is can not. If this parameter is set to true,
#'                  then a warning message will be generated for mention you that
#'                  which functions are overrided.
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

	if (!silent) {
		cat(sprintf("Imports VisualBasic.R::{%s}\n", namespace));
		print(names(func.list));
	}

	# invisible(NULL);
	if (silent) {
	  invisible(NULL);
	} else {
	  names(func.list);
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
#' @details 判断对象是否为空，在这个函数里面，空值，NA值，长度为零的向量，
#'          列表等都会被当作为空值
IsNothing <- function(x, stringAsFactor = FALSE) {

	if (is.null(x) || is.na(x) || length(x) == 0) {
		TRUE;

		# 2018-6-25 空字符串无法直接和S4对象进行比较
		# 所以下面会需要先进行一次类型比较再判断空字符串
		# Error in x == "" : 只能比较(1)基元或串列种类
	} else if (!is.character(x)) {
		FALSE;
	} else {

		if (x == "") {
			TRUE;
		} else if (!stringAsFactor) {
			FALSE;
		} else {
			return (x %in% c("NULL", "null", "na", "NA"));
		}
	}
}

#' Returns the object size
#'
#' @param x R object in any type
#'
#' @return A list with element: \code{rows} and \code{cols} to indicate the object size.
#'         for x is \code{data.frame}, these two element value will be \code{\link{base::nrows}} and \code{\link{base::ncols}}
#'         for x is \code{list} or \code{vector}, these two element value will be \code{rows = 1} and \code{cols = \link{length}(x)}
#'         for x is object like S4 class, these two element value will be \code{[1,1]}
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

#' Simulate the C printf function
#'
#' @param ... Function parameter for function \code{\link{sprintf}}
printf <- function(...) invisible(print(sprintf(...)));

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

#' Enumerate some primitive type in R language
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

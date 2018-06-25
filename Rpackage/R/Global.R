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
imports <- function(namespace, overrides = FALSE, silent = TRUE) {
	frame       <- parent.frame();
	module      <- get(namespace, envir = frame);
	func.list   <- module();
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

	# invisible(NULL);
	if (silent) {
	  invisible(NULL);
	} else {
	  names(func.list);
	}
}

#' Determine that target \code{is.null} or \code{is.na} or \code{length}
#' equals to ZERO?
#'
#' @param x Object with any type for determine that its value is null
#'           or empty or not?
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

#' syntax tweaks
microsoft.visualbasic.language <- function() {

	# 如果对象为空则取默认值
	`%||%` <- function(x, y) if(IsNothing(x)) y else x;

	## @param path: list(obj, "/path/ref");
	`%<=%` <- function(path, value) {

		# get parent environment
		.globalEnvir <- parent.frame();

		# set an invisible temp variable
		t        = "tmp_0x2C34B_assign_Helper";
		do.call(`=`, list(t, value), envir = .globalEnvir);

		x       <- path[[1]];
		x.value <- get(x);
		path    <- Strings.Split(path[[2]], "/");

		# Build the dynamics expression
		v   <- Strings.Join(path[2:length(path)], "$");
		exp <- parse(text = sprintf("%s$%s <- %s", x, v, t));
		eval(exp, envir = .globalEnvir);

		# removes the temp helper variable
		do.call(`rm`, list(t), envir = .globalEnvir);

		return(invisible(NULL))
	}

	# tuple syntax helper
	#
	# example:
	#
	# f <- function() list(TRUE, c(1,1,1,1,1));
	#
	# c(x,y) := f();
	#
	# > x
	# [1] TRUE
	# > y
	# [1] 1 1 1 1 1
	#
	# https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
	#
	':=' <- function(lhs, rhs) {
		frame <- parent.frame();
		lhs   <- as.list(substitute(lhs))

		if (length(lhs) > 1)
			lhs <- lhs[-1];
		if (length(lhs) == 1) {
			do.call(`=`, list(lhs[[1]], rhs), envir=frame)
			return(invisible(NULL));
		}

		if (is.function(rhs) || is(rhs, 'formula'))
			rhs <- list(rhs);
		if (length(lhs) > length(rhs))
			rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)));

		for (i in 1:length(lhs))
			do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame);

		invisible(NULL);
	}

	# Extension method operator
	`%=>%` <- function(x, y) {
		y(x);
	}

	list("%||%" = get("%||%"),
		 "%<=%" = get("%<=%"),
		 ":="   = get(":="),
		 "%=>%" = get("%=>%")
	);
}

#' This function returns on of the member value from enumeration function
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

#' Enumerate some primitive type in R language, these enumeration value have:
#'
#' \enumerate{
#' \item \code{object} = 0
#' \item \code{data.frame} = 1
#' \item \code{list} = 2
#' \item \code{vector} = 3
#' }
primitiveTypes <- function() {
	list(object = 0, data.frame = 1, list = 2, vector = 3);
}

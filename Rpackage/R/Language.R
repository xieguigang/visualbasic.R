#Region "Microsoft.ROpen::d319b22f5f299d8a5eaf0bc413007960, Language.R"

    # Summaries:

    # Microsoft.VisualBasic.Language <- function() {...
    # `%<=%` <- function(path, value) {...
    # ':=' <- function(lhs, rhs) {...
    # `%=>%` <- function(x, y) {...
    # `%is%` <- function(x, y) {...
    # `%+%` <- function(x, y) {...

#End Region

#' Syntax tweaks helpers
#'
#' @return Namespace module with exports:
#'     \enumerate{
#'        \item \code{\%||\%} The \code{OR} default value operator
#'        \item \code{\%:=\%} The tuple syntax
#'        \item \code{\%=>\%} Forward pipeline operator
#'        \item \code{\%+\%}  Set union operator
#'     }
#'
#' @details
#'
#' @examples
#'
#' 1. Tuple syntax:\cr
#'
#' \code{
#' f <- function() list(TRUE, c(1,1,1,1,1));
#' c(x,y) := f();
#'
#' # > x
#' # [1] TRUE
#' # > y
#' # [1] 1 1 1 1 1
#' }
Microsoft.VisualBasic.Language <- function() {

	#' The \code{OR} default expression
	#'
	#' @return \code{\%||\%}: The target given value or default value is
	#'     target value \code{\link{IsNothing}}.
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

		invisible(NULL);
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
		lhs   <- as.list(substitute(lhs));

		if (length(lhs) > 1)
			lhs <- lhs[-1];
		if (length(lhs) == 1) {
			do.call(`=`, list(lhs[[1]], rhs), envir=frame);
			return(invisible(NULL));
		}

		# 2018-12-07
		# When call from R terminal, the `is` function works fine
		# But exceptions will happened when running script from Rscript tool:
		#
		# Error in is(rhs, "formula") : could not find function "is"
		#
		if (is.function(rhs) || methods::is(rhs, 'formula')) {
		  rhs <- list(rhs);
		}
		if (length(lhs) > length(rhs)) {
		  d   <- length(lhs) - length(rhs);
		  rhs <- c(rhs, rep(list(NULL), d));
		}

		for (i in 1:length(lhs))
			do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame);

		invisible(NULL);
	}

	#' Extension method operator
	#'
	#' @param x Function parameter
	#' @param y Function with one parameter
	#'
	#' @return \code{\%=>\%}: The function value.
	#'
	`%=>%` <- function(x, y) {
		y(x);
	}

	#' \code{\%is\%} have the same function as \code{\%=>\%}, but this function is more
	#' tend to express of make a assertions.
	#'
	`%is%` <- function(x, y) {
	  y(x);
	}

	#' Union two collection
	#'
	#' @param x vectors (of the same mode) containing a sequence
	#'        of items (conceptually) with no duplicated values.
	#' @param y vectors (of the same mode) containing a sequence
	#'        of items (conceptually) with no duplicated values.
	#'
	#' @return \code{\%+\%}: A vector of the same mode as x or y for a common mode for \code{union}.
	#'
	`%+%` <- function(x, y) {
    union(x, y);
	}

	list(namespace = GetCurrentFunc(),
		 description = "R language syntax helpers",
		 methods = list(
			 "%||%" = get("%||%"),
		   "%<=%" = get("%<=%"),
		   ":="   = get(":="),
		   "%=>%" = get("%=>%"),
			 "%+%"  = get("%+%"),
			 "%is%" = get("%is%")
	));
}

#Region "Microsoft.ROpen::8bab0eec0d9db7fdf1f1eafc557eb2fc, Helpers.R"

    # Summaries:

    # as.index <- function(keys) {...
    # user <- function() {...
    # log.open <- function(file.path) {...
    # Now <- function() {...
    # log.close <- function(print.warnings = FALSE) {if (print.warnings) {...
    # log.echo <- function(...) {...
    # unix.timestamp <- function() {...

#End Region

#' Swap two variable
#'
#' @param a variable 1
#' @param b variable 2
#'
#' @details
#'
#' example as:
#'
#'    a <- 1;
#'    b <- "Hello World!";
#'    c(b, a) := swap(a, b);
#'
swap <- function(a, b) list(a = b, b = a);

#' Key index helper
#'
#' @description Create a index object for determine that a given id
#' is exists in the keys collection or not.
#'
#' @param keys This function parameter should be a string character vector.
#'
as.index <- function(keys) {
	keys  <- unique(keys);
	index <- lapply(keys, function(key) 1);
	names(index) <- keys;
	
	function(test) !is.null(index[[test]])
}

#' Get current Linux user
#'
#' @description Get user name of current linux login user by bash shell
#'   This function only works on Linux platform.
user <- function() {
  cli = "echo \"echo $USER\" | bash";
  system(cli, intern = TRUE);
}

#' R logging helper by \code{sink}
#'
#' @description This function can create the parent dir for the given
#'    log file safely.
#'
#' @param file.path The \code{*.log} log file path.
#'
log.open <- function(file.path) {
  file.path %=>% dirname %=>% ensure_dir_exists;
  sink(file.path, append = FALSE, split = TRUE);

  cat(sprintf("\nStart @ %s\n\n", Now()));
}

#' Gets current time string value
#'
Now <- function() {
  format(Sys.time(), "%d/%b/%Y, %a %X");
}

#' Close current log file
#'
#' @param print.warnings Print all of the warnings before close
#'        the log writer? Default is not print warnings.
#'
log.close <- function(print.warnings = FALSE) {

	if (print.warnings) {
		cat("\n\n");
		print(warnings());
		cat("\n\n");
	}

	cat(sprintf("\n---------------EndOfLog @ %s-----------------\n\n", Now()));
	sink();
}

#' Print a log text onto screen
#'
#' @param ... Parameters for \code{sprintf} function.
#'
log.echo <- function(...) {
  msg <- sprintf(...);
  msg <- sprintf("[%s] %s\n", Now(), msg);

  cat(msg);
}

#' Current unix timestamp in milliseconds
#'
#' @description https://stackoverflow.com/questions/40059573/r-get-current-time-in-milliseconds
#'
#' @return timestamp in milliseconds
#'
unix.timestamp <- function() {
  as.numeric(Sys.time()) * 1000;
}

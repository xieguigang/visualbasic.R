#Region "Microsoft.ROpen::986df87c2fcb663742e2c97ccd57d7ed, Helpers.R"

    # Summaries:

    # as.index <- function(keys) {...
    # user <- function() {...
    # argv <- function() {...
    # is.argName <- function(x) {if (x %=>% IsNothing) {...
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
	index <- list();
	
	for(key in keys) {
		index[[key]] = 1;
	}
	
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

#' Get commandline
#'
#' @note
#'
#' Commandline parser for cli expression pattern like:
#'
#' \code{Rscript script.R /name /arg1 value1 /arg2 value2 /boolean1 /arg3 value3}
#'
argv <- function() {

  cli <- commandArgs();

  if (.Platform$OS.type == "windows") {
    # commandline on windows
    #
    # [1] "D:\\R\\bin\\x64\\Rterm.exe"
    # [2] "--slave"
    # [3] "--no-restore"
    # [4] "--file=D:\\smartnucl_integrative\\biodeepDB\\internal/Rscripts/mz_calculator.R"
    # [5] "--args"
    # [6] "-1"
    # [7] "745.0911"
    # [8] "./data/temp/mz_calculator_TMgNO9CQ"

    cli <- cli[6:length(cli)];
  } else {
    # commandline on linux
    #
    # [1] "/usr/local/software/R-3.4.3/lib64/R/bin/exec/R"
    # [2] "--slave"
    # [3] "--no-restore"
    # [4] "--file=./mz_calculator.R"
    # [5] "--args"
    # [6] "-1"
    # [7] "745.0911"
    # [8] "./mz_calculator_TMgNO9CQ"

    cli <- cli[6:length(cli)];
  }

  name <- cli[1];
  args <- list();
  i    <- 2;
  is.argName <- function(x) {
    if (x %=>% IsNothing) {
      return (FALSE);
    }

    base::startsWith(x, "/")  ||
    base::startsWith(x, "--") ||
    base::startsWith(x, "-");
  }

  while(i < length(cli)) {
    argName = cli[i];

    if (cli[i + 1] %=>% is.argName) {
      # If the next element is the command argument name
      # then the current element is a logical flag
      args[argName] = TRUE;
    } else {
      args[argName] = cli[i + 1];
      i = i + 1;
    }
  }

  list(argv = cli, commandName = name, args = args);
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

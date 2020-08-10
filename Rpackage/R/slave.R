
#' Run a closure string
#'
#' @description The closure string could be content from stdin or user input text.
#'
#' @param closure A string vector that can represent as a function, which should have no parameter.
#'    By default, the closure text is read from stdin.
#'
slave_closure <- function(closure = "stdin") {
  if (closure == "stdin") {
    closure <- ReadAllText("stdin");
  }
  if (argv()$nextToken("--debug") == "TRUE") {
    print(closure);
  }

  # probably contains R Environment information, like
  # <bytecode: 0x1872cbb0>\n<environment: 0x241cc710>
  #
  eval(parse(text = stripREnvironmentInfo(closure)))();
}

#' Removes R environment info
#'
#' @description A function is convert to string by \code{capture.output} function
#'    may contains some R environment information like:
#'    \code{<bytecode: 0x1872cbb0><environment: 0x241cc710>}. The information could make
#'    the \code{eval(parse(text = closure))} process failure. This helper tools
#'    will try to removes such environment information text.
#'
#' @param closure A function tostring result.
#'
stripREnvironmentInfo <- function(closure) {
  gsub("[<][a-z]+[:].+?[>]", "", closure, perl=TRUE);
}

#' Run closure in a new R process
#'
#' @description This is usually using for memory size optimization.
#'
#' @param closure should be a function without any parameter. And have no function returns.
#' @param arguments The additional commandline arguments that will pass to the child process.
#'     This parameter should be a named list or character vector. If the argument list object
#'     contains a \code{commandName} member, then it will generates a VisualBasic style
#'     commandline arguments, which can be processed by \code{\link{argv}} function.
#' @param parallel This not parallel, the slave closure function calls will block running of
#'     R program at here. By default is not running in parallel mode.
#'
slave <- function(closure, arguments = NULL, parallel = FALSE, debug = FALSE) {
  closure   <- capture.output(closure);
  slave_cli <- "R -q --no-restore --no-save --slave -e \"VisualBasic.R::slave_closure();\"";

  if (IsNothing(arguments)) {
    # Do nothing
    # arguments <- "";
    commandName <- "";
  } else if (is.character(arguments)) {
    # string concatenations directly for strings
    arguments <- arguments %=>% cliToken %=>% Strings.Join;
    commandName <- "";
  } else {
    # key-value pairs arguments
    commandName <- arguments[["commandName"]] %||% "";
    arguments[["commandName"]] <- NULL;
    arguments <- sapply(names(arguments), function(key) {
      sprintf("%s %s", key, arguments[[key]] %=>% cliToken);
    }) %=>% as.vector %=>% Strings.Join;
  }

  if (!IsNothing(arguments)) {
    slave_cli <- sprintf("%s --args %s %s", slave_cli, commandName, arguments);
  }

  if (debug) {
    print(slave_cli);
  }

  # arguments 'show.output.on.console', 'minimized' and 'invisible' are for Windows only
  system(slave_cli, intern = FALSE,
         ignore.stderr = FALSE,
         wait = !parallel,
         input = closure
  );
}

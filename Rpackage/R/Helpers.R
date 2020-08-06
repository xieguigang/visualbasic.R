#Region "Microsoft.ROpen::6ae65b1c62ea166c23b53642165af4ce, Helpers.R"

    # Summaries:

    # as.index <- function(keys) {if (keys %=>% IsNothing) {...
    # user <- function() {...
    # memory <- function() {...
    # log.open <- function(file.path) {...
    # Now <- function() {...
    # log.close <- function(print.warnings = FALSE) {if (print.warnings) {...
    # log.echo <- function(...) {...
    # unix.timestamp <- function() {...
    # vector.fill <- function(list, baselist) {if (is.integer(baselist) && length(baselist) == 1) {...
    # alloca <- function(len, list = FALSE) {if (list) {...

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
#'   If this parameter its value is null, then this function will returns all
#'   of the keys that in target index closure.
#'
#' @return If the given \code{keys} is NULL or a single NA value, then NULL value will be returns.
#'         Otherwise, returns a index function.
#'
as.index <- function(keys) {
  if (keys %=>% IsNothing) {
    NULL;
  } else {
    keys  <- unique(keys);
    # create a list at here
    # and then use this list object as index
    index <- lapply(keys, function(key) 1);
    names(index) <- keys;

    function(test = NULL) {
      if (is.null(test)) {
        keys;
      } else {
        !is.null(index[[test]]);
      }
    }
  }
}

#' Get current Linux user
#'
#' @description Get user name of current linux login user by bash shell
#'   This function only works on Linux platform.
#'
user <- function() {
  cli = "echo \"echo $USER\" | bash";
  system(cli, intern = TRUE);
}

#' Get system memory info
#'
#' @return A list contains two slot: \code{Mem} for installed physical memory
#'    and \code{Swap} for swap memory.
#'    All of the memory data that returns from this function is in byte size
#'    unit \code{MB}.
#'
memory <- function() {
  # [biodeepdb@localhost tyr]$ free -m
  #                total        used        free      shared  buff/cache   available
  #  Mem:          64225        2285       37886          39       24053       61283
  #  Swap:         16127        1482       14645
  cli = "free -m";
  std_out <- system(cli, intern = TRUE);
  Mem  <- Strings.Split(std_out[2], "\\s+");
  Mem  <- Mem[2:length(Mem)] %=>% as.numeric;
  Mem  <- list(
    total = Mem[1],
    used  = Mem[2],
    free  = Mem[3],
    shared = Mem[4],
    "buff/cache" = Mem[5],
    available = Mem[6]
  );
  Swap <- Strings.Split(std_out[3], "\\s+");
  Swap <- Swap[2:length(Swap)] %=>% as.numeric;
  Swap <- list(
    total = Swap[1],
    used  = Swap[2],
    free  = Swap[3]
  );

  list(Mem = Mem, Swap = Swap);
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

#' fill data vector
#'
#' @param list Target value vector
#' @param baselist A vector provide length or a vector length integer number.
#'
#' @return A vector that with equals length with \code{baselist} vector.
#'
vector.fill <- function(list, baselist) {
  if (is.integer(baselist) && length(baselist) == 1) {
    baselen <- baselist;
  } else {
    baselen <- length(baselist);
  }

  if (length(list) == 1) {
    rep(list, baselen);
  } else if ( length(list) < baselen ) {
    last = length(list);

    for (i in (last + 1):10000) {
      list[i] = list[last];
    }

    list;
  } else {
    list;
  }
}

#' Memory preallocation
#'
#' @description Memory preallocation for vector or list. Apply this preallocation operation
#'   for each vector or list can make great performance improvements.
#'
#' @param len A integer preallocation size.
#' @param list Generate a empty list? By default is FALSE, means generate a empty vector.
#'
alloca <- function(len, list = FALSE) {
  if (list) {
    lapply(1:len, function(i) NA);
  } else {
    rep(NA, len = len);
  }
}

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

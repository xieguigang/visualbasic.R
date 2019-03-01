#Region "Microsoft.ROpen::ec812ae4f4181c4eebb10d02cb437650, taskHelper.R"

    # Summaries:

    # tick.helper <- function(total, disp.number = TRUE, step = 5 / 100, callback = NULL) {...
    # callback <- function(x) {# do nothing}}return(function() {...
    # tick.each <- function(sequence, action) {...
    # benchmark <- function() {...
    # memory.size.auto <- function() {if (.Platform$OS.type == "windows") {...
    # memory.sample <- function(note = NA) {...
    # write.memory.sample <- function(file) {if (!exists("memory_profiling_pool", envir = globalenv())) {...
    # slave_closure <- function(closure = "stdin") {if (closure == "stdin") {...
    # stripREnvironmentInfo <- function(closure) {...
    # slave <- function(closure, arguments = NULL, parallel = FALSE, debug = FALSE) {...

#End Region

#' Progress tick on console
#'
#' @description Helper function for display the progress in percentage value of a
#'              task which is required very long time to run.
#'
#'              Only avaliable for single thread R app
#'
#' @param total The total elements in the Long time run task that will be processing
#' @param step The tick interval value for report the task progress value, default is \code{5/100}.
#' @param callback Callback function that will be invoked at each \code{progress mod 5} progress.
#'
#' @return A lambda function that using for report the task progress.
#'
tick.helper <- function(total, disp.number = TRUE, step = 5 / 100, callback = NULL) {

  workspace <- environment();

  assign("i", 0, envir = workspace);
  assign("p5", total * step, envir = workspace);
  assign("cur", 0, envir = workspace);

  if (callback %=>% IsNothing) {
    callback <- function(x) {
      # do nothing
    }
  }

	return(function() {
		i <- get("i", envir = workspace) + 1;

		if (i >= get("p5", envir = workspace)) {
			i <- 1;
      cur <- get("cur", envir = workspace) + step * 100;
			assign("cur", round(cur), envir = workspace);

			if (disp.number) {
			  cat(cur);
			  cat(" ");
			} else {
			  cat(".");
			}

      callback(cur);
		}

    assign("i", i, envir = workspace);
	});
}

#' Display progress for elements' action
#'
#' @description Only works on the \code{vector} or \code{list}
#'
#' @param sequence A list or vector object.
#' @param action A function that will applied on each element
#'    in the input sequence.
#'
#' @return action result on each elements
#'
#' @details \code{\link{tick.helper}}
#'
tick.each <- function(sequence, action) {
  tick <- tick.helper(length(sequence));
  i <- 1;

  cat("\n");
  cat("  Progress%: ");

  out <- lapply(1:length(sequence), function(i) {
    tick();
    action(sequence[i]);
  });

  cat("\n");
  cat("\n");

  out;
}

#' Benchmark helper
#'
#' @details Could running in multiple instance
#'
benchmark <- function() {
  start <- unix.timestamp();
  uid   <- sprintf("Tbenchmark_%s", start);
  last  <- sprintf("%s_last", uid);

  global(last) <- start;

  function() {
    t.last  <- global(last);
    d.last  <- unix.timestamp() - t.last;
    d.start <- unix.timestamp() - start;

    global(last) <- unix.timestamp();

    list(since_last      = d.last,
         since_start     = d.start,
         start_timestamp = start,
         last_checkpoint = t.last
    );
  }
}

#' Unify method for get memory size
#'
#' @description Provides a unify method for get current R session its
#' memory usage. For solve the \code{Inf} bug of \code{memory.size}
#' function on linux platform.
#'
memory.size.auto <- function() {
  if (.Platform$OS.type == "windows") {
    # MB
    memory.size;
  } else {

    # Get pid of current R session.
    pid <- base::Sys.getpid();

    function() {
      call  <- sprintf("pmap %s | grep total", pid);
      total <- system(call, intern = TRUE);
      # total           123608K
      total <- Strings.Split(total);
      total <- total[length(total)];
      # convert to MB
      total <- substr(total, 1, nchar(total) - 1);
      total <- as.numeric(total) / 1024;

      total;
    }
  }
}

#' Get current memory sample
#'
#' @details This function is limited one instance
#'
memory.sample <- function(note = NA) {
  p <- "memory_profiling_pool";

  if (!exists(p, envir = globalenv())) {
    memory_profiling_pool <- list(
      benchmark = benchmark(),
      samples   = list(),
      memory_size = memory.size.auto()
    );

    samples <- list();
  } else {
    samples <- memory_profiling_pool[["samples"]];
  }

  # Get current time
  t      <- unix.timestamp();
  uid    <- sprintf("T%s", t);
  sample <- list(
    time        = t,
    memory_size = memory_profiling_pool$memory_size(),
    event       = GetCurrentFunc(offset = 1),
    note        = note,
    profiles    = memory.profile(),
    benchmark   = memory_profiling_pool$benchmark()
  );

  samples[[uid]] <- sample;
  memory_profiling_pool[["samples"]] <- samples;

  # global function returns NULL
  global(p) <- memory_profiling_pool;

  invisible(NULL);
}

#' Save the memory sampling result
#'
#' @description Write the memory sampling data to a csv table.
#' If you didn't doing any memory sampling by \code{memory.sample} yet,
#' then this function will do nothing.
#'
write.memory.sample <- function(file) {
  if (!exists("memory_profiling_pool", envir = globalenv())) {
    return(NULL);
  }

  if (!exists("debug.echo", envir = globalenv())) {
    debug.echo <- FALSE;
  }

  samples <- memory_profiling_pool[["samples"]];
  profile <- NULL;
  d       <- NULL;

  for(name in names(samples)) {
    sample  <- samples[[name]];
    benchmark <- sample$benchmark;
    profile <- rbind(profile, sample$profiles);
    d       <- rbind(d, c(
      sample$time,
      sample$memory_size,
      sample$event,
      sample$note,
      benchmark$since_last,
      benchmark$since_start
    ));
  }

  rownames(d)       <- names(samples);
  colnames(d)       <- c("time", "memory_size", "event", "note", "since_last", "since_start");
  rownames(profile) <- names(samples);

  if (debug.echo) {
    print(head(d));
    print(head(profile));
  }

  d <- cbind(d, profile);

  write.csv(d, file = file, row.names = FALSE);
}

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

#Region "Microsoft.ROpen::b34e53abe1d5900bf506b55542d6d53e, taskHelper.R"

    # Summaries:

    # tick.helper <- function(total, disp.number = TRUE, step = 5 / 100) {...
    # tick.each <- function(sequence, action) {...
    # benchmark <- function() {...
    # memory.sample <- function(note = NA) {if (!exists("memory_profiling_pool", envir = globalenv())) {...
    # write.memory.sample <- function(file) {if (!exists("memory_profiling_pool", envir = globalenv())) {...

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
#'
#' @return A lambda function that using for report the task progress.
#'
tick.helper <- function(total, disp.number = TRUE, step = 5 / 100) {

  progress.i   <<- 0;
	progress.p5  <<- total * step;
	progress.cur <<- 0;

	return(function() {
		progress.i <<- progress.i + 1;

		if (progress.i >= progress.p5) {
			progress.i   <<- 1;
			progress.cur <<- progress.cur + step * 100;
			progress.cur <<- round(progress.cur);

			if (disp.number) {
			  cat(progress.cur);
			  cat(" ");
			} else {
			  cat(".");
			}
		}
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
  out  <- list();
  tick <- tick.helper(length(sequence));
  i <- 1;

  cat("\n");
  cat("  Progress%: ");

  for (x in sequence) {
    out[[i]] <- action(x);
    i = i + 1;
    tick();
  }

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

  global(last, start);

  function() {
    t.last  <- get(last, envir = globalenv());
    d.last  <- unix.timestamp() - t.last;
    d.start <- unix.timestamp() - start;

    global(last, unix.timestamp());

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
      total;
    }
  }
}

#' Get current memory sample
#'
#' @details This function is limited one instance
#'
memory.sample <- function(note = NA) {
  if (!exists("memory_profiling_pool", envir = globalenv())) {
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
  global("memory_profiling_pool", memory_profiling_pool);
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

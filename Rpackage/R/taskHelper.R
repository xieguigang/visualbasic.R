#Region "Microsoft.ROpen::047c173bfa95b6eafec5b4737dc0c561, taskHelper.R"

    # Summaries:

    # tick.helper <- function(total, disp.number = TRUE, step = 5 / 100) {...
    # tick.each <- function(sequence, action) {...
    # benchmark <- function() {...

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
			} else {
			  cat(".");
			}

			cat(" ");
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

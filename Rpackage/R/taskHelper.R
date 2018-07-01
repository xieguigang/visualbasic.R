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
tick.helper <- function(total, step = 5 / 100) {

  h <- Push(envir = environment());
	h(tick.helper.i        = 0,
	  tick.helper.p5       = total * step,
	  tick.helper.progress = 0
	);

	return(function() {
		h(tick.helper.i = tick.helper.i + 1);

		if (tick.helper.i >= tick.helper.p5) {
			h(tick.helper.i  = 1);
			h(tick.helper.progress = tick.helper.progress + step * 100);
			h(tick.helper.progress = round(tick.helper.progress, 0));

			cat(tick.helper.progress);
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

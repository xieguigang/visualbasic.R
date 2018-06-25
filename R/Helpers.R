#' Helper function for display the progress in percentage value of a
#' task which is required very long time to run.
#'
#' Only avaliable for single thread R app
#'
#' @param total The total elements in the Long time run task that will be processing
#' @param step The tick interval value for report the task progress value, default is \code{5%}.
#'
#' @return A lambda function that using for report the task progress.
tick.helper <- function(total, step = 5 / 100) {

	# 因为R语言的closure里面的变量都是局部变量，所以修改局部变量并不会影响全局的记录进度的变量
	# 所以在这里必须要使用全局变量赋值来实现进度的记录

	tick.helper.i  <<- 0;
	tick.helper.p5 <<- total * step;
	tick.helper.progress <<- 0;

	return(function() {
		tick.helper.i <<- tick.helper.i + 1;

		if (tick.helper.i >= tick.helper.p5) {
			tick.helper.i <<- 1;
			tick.helper.progress <<- tick.helper.progress + step * 100;
			tick.helper.progress <<- round(tick.helper.progress, 0);

			cat(tick.helper.progress);
			cat(" ");
		}
	});
}

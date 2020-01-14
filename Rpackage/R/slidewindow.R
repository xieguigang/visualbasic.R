
#' Create slide windows
#'
#' @param ... A list of named variable which should be vectors and theirs vector size
#'   should also be equals to each other.
#'
slide.windows <- function(win_size = 2, step = 1, ...) {
  data <- list(...);
  keys <- names(data);
  out <- list();

  for(i in seq(1, length(data[[1]])-step, by= step)) {
    range <- i:(i + win_size -1);
    element <- list();

    for(key in keys) {
      element[[key]] <- data[[key]][range];
    }

    out[[sprintf("X%s", i)]] <- element;
  }

  out;
}

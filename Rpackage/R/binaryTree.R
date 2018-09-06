
#' This function build a binary tree object
#'
#' @param src the input data sequence, which can be \code{data.frame}, \code{list} or
#'            \code{vector}
#' @param key An object indexer for get the key value for the elements
#' @param key.numeric Convert the key value to numeric value for comparasions.
#'
#' @return A binary tree S4 class object
binaryTree <- function(src, key, key.numeric = as.numeric) {

}

numeric.group <- function(seq, assert = function(x, y) abs(x - y) <= 1) {
  seq    <- sort(seq);
  groups <- list();
  a      <- seq[1];
  block  <- c(a);

  for (i in 2:length(seq)) {
    x <- seq[i];

    if (assert(a, x)) {
      block <- append(block, x);
    } else {
      key           <- mean(block) %=>% as.character;
      groups[[key]] <- block;
      block         <- c(x);
      a             <- x;
    }
  }

  if (length(block) > 0) {
    key           <- mean(block) %=>% as.character;
    groups[[key]] <- block;
  }

  groups;
}

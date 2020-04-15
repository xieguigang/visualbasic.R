#Region "Microsoft.ROpen::abeedd04349faa108015c12e345dbdd4, linearCluster.R"

    # Summaries:

    # numeric.group <- function(seq, assert = function(x, y) abs(x - y) <= 1) {...
    # numeric.group.impl <- function(seq, assert) {...
    # numeric.block <- function(seq, assert = function(x,y) abs(x-y) <= 1) {...
    # numeric.block.impl <- function(seq, assert) {...

#End Region

#' Group a numeric vector
#'
#' @description Group a numeric vector elements by a given test condition
#'
#' @param seq A numeric sequence
#' @param assert A given test condition for test if a number is a member
#'               of the current group or not?
#'
numeric.group <- function(seq, assert = function(x, y) abs(x - y) <= 1) {
  len = seq %=>% length;

  if ((len == 0) || (seq %=>% is.na)) {
    list();
  } else if (len == 1) {
    single <- list();
    single[[as.character(seq)]] = seq;
    single;
  } else {
    numeric.group.impl(seq, assert);
  }
}

#' Group a numeric vector without check
#'
#' @param seq Please ensure that this numeric sequence is not
#'       \code{NULL} or \code{NA} value, and it must contains
#'       more than 1 elements.
#'
numeric.group.impl <- function(seq, assert) {
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

  rm(seq);

  groups;
}

numeric.block <- function(seq, assert = function(x,y) abs(x-y) <= 1) {
  len = seq %=>% length;

  if ((len == 0) || (seq %=>% is.na)) {
    list();
  } else if (len == 1) {
    single <- list();
    single[[as.character(seq)]] = seq;
    single;
  } else {
    numeric.block.impl(seq, assert);
  }
}

numeric.block.impl <- function(seq, assert) {
  seq    <- sort(seq);
  blocks <- list();
  a      <- seq[1];
  block  <- c(a);

  for(i in 2:length(seq)) {
    x <- seq[i];

    if (assert(a, x)) {
      block <- append(block, x);
    } else {
      key           <- tabulate.mode(block) %=>% as.character;
      blocks[[key]] <- block;
      block         <- c(x);
    }

    a <- x;
  }

  if (length(block) > 0) {
    key           <- tabulate.mode(block) %=>% as.character;
    blocks[[key]] <- block;
  }

  rm(seq);

  blocks;
}

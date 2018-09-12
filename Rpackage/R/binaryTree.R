#Region "Microsoft.ROpen::bcd481e9cc0327e8eb0eeeed5ea9e306, binaryTree.R"

    # Summaries:

    # binaryTree <- function(src, key, key.compares) {}#' Group a numeric vector #' #' @description Group a numeric vector elements by a given test condition #' #' @param seq A numeric sequence #' @param assert A given test condition for test if a number is a member #' of the current group or not? #' numeric.group <- function(seq, assert = function(x, y) abs(x - y) <= 1) {...
    # numeric.group.impl <- function(seq, assert) {...

#End Region

#' Build a binary tree list
#'
#' @description This function build a binary tree list.
#'
#' @param src the input data sequence, which can be \code{data.frame}, \code{list} or
#'            \code{vector}
#' @param key An object indexer for get the key value for the elements
#' @param key.compares A function to compare two key, this function
#'     should returns a integer value:
#'
#'     \enumerate{
#'        \item \code{0} The two tree node key is equals to each other.
#'        \item \code{1} The \code{key1} is greater than \code{key2}.
#'        \item \code{-1} The \code{key1} is less than \code{key2}.
#'     }
#'
#' @return A binary tree S4 class object
#'
binaryTree <- function(src, key, key.compares) {
  tree     <- list();
  namelist <- src %=>% names;

  # the root node
  x           <- src[[namelist[1]]];
  tree[["1"]] <- .node(key(x), x, namelist[1]);

  for(i in 2:length(src)) {
    name <- namelist[i];
    x    <- src[[name]];
    xkey <- key(x);

    # The first element is always the
    # root element.
    pnext <- "1";

    while (TRUE) {
      xnext <- tree[[pnext]];
      compare <- key.compares(xnext$key, xkey);

      if (compare == 0) {
        # current element is equals to current tree node.
        # append current tree node value
        list <- xnext$members;
        list[[name]]  <- x;
        xnext$members <- list;
        tree[[pnext]] <- xnext;

        # exit current loop
        break;
      } else if (compare > 0) {
        # left
        if (xnext$left == -1) {
          # append to left;
          node <- .node(xkey, x, name);
          tree[[as.character(i)]] <- node;

          # exit current loop
          break;
        } else {
          pnext <- xnext$left;
        }
      } else {
        # right
        if (xnext$right == -1) {
          # append to right
          node <- .node(xkey, x, name);
          tree[[as.character(i)]] <- node;

          # exit current loop
          break;
        } else {
          pnext <- xnext$right;
        }
      }
    }
  }

  # return the constructed binary tree list.
  tree;
}

#' Create a new tree node
#'
.node <- function(key, x, name) {
  values         <- list();
  values[[name]] <- x;

  list(key     = key,
       members = values,
       left    = -1,
       right   = -1
  );
}

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

  if ((len == 0) || (seq %=>% IsNothing)) {
    list();
  } else if (len == 1) {
    single <- list();
    single[[as.character(seq)]] = seq;
    single;
  } else {
    numeric.group.impl(seq, assert);
  }
}

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

  groups;
}

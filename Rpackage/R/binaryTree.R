#Region "Microsoft.ROpen::765f1ae82bd2b628ceb0a391d798f55e, binaryTree.R"

    # Summaries:

    # binaryTree <- function(src, key, key.compares, debug = FALSE, progressHeader = TRUE) {...
    # popX <- function(i) {...
    # popName <- function(i) {...
    # popX <- function(i) {...
    # popName <- function(index) {...
    # binaryTree.construct.impl <- function(tree, popX, popName, src,key= NULL,key.compares= NULL,debug = TRUE,progressHeader = TRUE) { if (debug) {...
    # binaryTree.node <- function(key, x, name) {...
    # node.keys <- function(tree) {...
    # node.right <- function(tree, node) {if (node$right == -1) {...
    # node.left <- function(tree, node) {if (node$left == -1) {...
    # node.is_leaf <- function(node) {...
    # node.find <- function(tree, search, key.compares) {...

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
binaryTree <- function(src, key, key.compares, debug = FALSE, progressHeader = TRUE) {
  tree    <- list();
  popX    <- NULL;
  popName <- NULL;
  len     <- length(src);

  if (is.list(src)) {
    # is list
    namelist <- src %=>% names;
    popX <- function(i) {
      name <- namelist[i];
      src[[name]];
    }
    popName <- function(i) {
      namelist[i];
    }
  } else {
    # is array
    popX <- function(i) {
      src[i];
    }
    popName <- function(index) {
      sprintf("X%s", index);
    }
  }

  # the root node
  x           <- popX(1);
  tree[["1"]] <- binaryTree.node(key(x), x, popName(1));

  if (len == 1) {
    tree;
  } else {
    binaryTree.construct.impl(tree, popX, popName,
  		src,
  		key,
  		key.compares,
  		debug, progressHeader
  	);
  }
}

#' Binary tree build implementation
#'
binaryTree.construct.impl <- function(tree, popX, popName, src,
  key            = NULL,
  key.compares   = NULL,
  debug          = TRUE,
  progressHeader = TRUE) {

  if (debug) {
    tick <- tick.helper(length(src) - 1);

  	if (progressHeader) {
  		cat("\n");
  		cat(" Progress  ");
  	} else {
  		cat("  ");
  	}
  }

  # The first element is already include as root node
  # so for loop start from the second element.
  for(i in 2:length(src)) {
    x    <- popX(i);
    xkey <- key(x);
    name <- popName(i);

    # The first element is always the
    # root element.
    pnext <- "1";

    while (TRUE) {
      xnext <- tree[[pnext]];
      compare <- key.compares(xnext$key, xkey);

      if (compare == 0) {
        # current element is equals to current tree node.
        # append current tree node value
        #
        # tree[[pnext]] is xnext, the current node.
        #
        tree[[pnext]]$members[[name]] <- x;

        # exit current loop
        break;
      } else if (compare > 0) {
        # left
        if (xnext$left == -1) {
          # append to left;
          node <- binaryTree.node(xkey, x, name);
          xkey <- as.character(i);
          tree[[xkey]] <- node;
          tree[[pnext]]$left <- xkey;

          # exit current loop
          break;
        } else {
          pnext <- xnext$left;
        }
      } else {
        # right
        if (xnext$right == -1) {
          # append to right
          node <- binaryTree.node(xkey, x, name);
          xkey <- as.character(i);
          tree[[xkey]] <- node;
          tree[[pnext]]$right <- xkey;

          # exit current loop
          break;
        } else {
          pnext <- xnext$right;
        }
      }
    }

    if (debug) {
      tick();
    }
  }

  if (debug) {
  	if (progressHeader) {
  		cat("\n\n");
  	} else {
  		cat(" ");
  	}
  }

  # return the constructed binary tree list.
  tree;
}

#' Create a new tree node
#'
#' @param x a node content data object
#'
#' @details the initial node content data object \code{x} is already includes into the
#'     \code{members} slot of the created new binary tree node list object.
#'
binaryTree.node <- function(key, x, name) {
  values         <- list();
  values[[name]] <- x;

  list(key     = key,
       members = values,
       left    = -1,
       right   = -1
  );
}

node.keys <- function(tree) {
  lapply(names(tree), function(name) tree[[name]]$key);
}

#' Get the right node of current node
#'
#' @param node The current tree node
#' @param tree The binary tree list object.
#'
node.right <- function(tree, node) {
  if (node$right == -1) {
    NULL;
  } else {
    pnext <- node$right;
    tree[[pnext]];
  }
}

node.left <- function(tree, node) {
  if (node$left == -1) {
    NULL;
  } else {
    pnext <- node$left;
    tree[[pnext]];
  }
}

node.is_leaf <- function(node) {
  node$left == -1 && node$right == -1;
}

#' Find a node value by a given search key
#'
#' @param search Object key for search a tree node.
#'
node.find <- function(tree, search, key.compares) {
  # The first element is always the
  # root element.
  pnext <- "1";

  while(TRUE) {
    node    <- tree[[pnext]];
    key     <- node$key;
    compare <- key.compares(key, search);

    if (compare == 0) {
      # find target node;
      return(node);
    } else if (compare > 0) {
      # left
      pnext <- node$left;

      if (pnext == -1) {
        # no result
        break;
      } else {
        # continue
      }
    } else {
      # right
      pnext <- node$right;

      if (pnext == -1) {
        # node result
        break;
      } else {
        # continue
      }
    }
  }

  # no result
  NULL;
}

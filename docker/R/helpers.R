
#' create volume binding
#'
#' @param v a list object should in data structure like:
#' \code{list(anyname = list(host = ..., virtual = ...))}
#'
#' @seealso \link{run}
#'
volumeBind = function(v) {
  if (is.null(v) || is.na(v)) {
    NULL;
  } else {
    unlist(sapply(v, function(mapping) {
      sprintf("%s:%s", normalizePath(mapping[["host"]]), normalizePath(mapping[["virtual"]]));
    }));
  }
}

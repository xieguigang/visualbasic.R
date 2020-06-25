
#' create volume binding
#'
#' @param v a list object should in data structure like:
#' \code{list(anyname = list(host = ..., virtual = ...))}
#'
volumeBind = function(v) {
  if (is.null(v) || is.na(v)) {
    NULL;
  } else {
    unlist(sapply(v, function(mapping) {
      sprintf("%s:%s", mapping[["host"]], mapping[["virtual"]]);
    }));
  }
}

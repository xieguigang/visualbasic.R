
#' Swap two variable
#'
#' @param a variable 1
#' @param b variable 2
#'
#' @example
#'
#'    a <- 1;
#'    b <- "Hello World!";
#'
#'    c(b, a) := swap(a, b);
#'
swap <- function(a, b) list(a = b, b = a);



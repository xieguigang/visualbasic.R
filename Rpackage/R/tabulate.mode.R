#Region "Microsoft.ROpen::9f9609233ae6110a21149b29f67566ef, tabulate.mode.R"

    # Summaries:

    # tabulate.mode <- function(x, extends = TRUE) {...
    # tabulate.bin <- function(x, extends = TRUE) {...

#End Region

#' Evaluate mean value
#'
tabulate.mode <- function(x, extends = TRUE) {
  if (length(x) == 0) {
    NA;
  } else if (length(x) == 1) {
    x;
  } else if (length(x) == 2) {
    mean(x);
  } else {
    # returns the mean value which with outlier removed
    mean(tabulate.bin(x, extends));
  }
}

#' get data that removes outlier
#'
tabulate.bin <- function(x, extends = TRUE) {
  # do fixed width bin cuts
  delta <- (max(x) - min(x)) / 5;

  it <- min(x);
  a1 <- x[x >= it & x < (it + delta)];
  it <- it + delta;
  a2 <- x[x >= it & x < (it + delta)];
  it <- it + delta;
  a3 <- x[x >= it & x < (it + delta)];
  it <- it + delta;
  a4 <- x[x >= it & x < (it + delta)];
  it <- it + delta;
  a5 <- x[x >= it & x < (it + delta)];

  # get the max size bin box
  i <- which.max(c(length(a1), length(a2), length(a3), length(a4), length(a5)));
  bin <- list(a1,a2,a3,a4,a5);

  if (extends) {
    if (i == 1) {
      # 1 + 2
      a <- append(bin[[1]], bin[[2]]);
    } else if (i == 5) {
      # 4 + 5
      a <- append(bin[[4]], bin[[5]]);
    } else {
      # n-1, n, n+1
      a <- append(append(bin[[i-1]], bin[[i]]), bin[[i+1]]);
    }
  } else {
    a <- bin[[i]];
  }

  a;
}

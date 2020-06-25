#Region "Microsoft.ROpen::7d4aa40f447910586a8c63f31a388150, reduce.R"

    # Summaries:

    # reduce <- function(x, aggregate = function(a, b) a + b, init.val = 0) {...

#End Region

#' Reduce the vector dataset
#'
reduce <- function(x, aggregate = function(a, b) a + b, init.val = 0) {
  aggregate.val <- init.val;
  results <- x;

  for(i in 1:length(x)) {
    aggregate.val <- aggregate(aggregate.val, x[i]);
    results[i] <- aggregate.val;
  }

  results;
}

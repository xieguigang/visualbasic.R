#Region "Microsoft.ROpen::87208e26d6e966e78433be2f96af171c, parallelHelper.R"

    # Summaries:

    # getClusterCores <- function(level = c("full", "half", "parallel", "single")) {...

#End Region

getClusterCores <- function(level = c("full", "half", "parallel", "single")) {
  if (IsNothing(level)) {
    level = "full";
  } else {
    level = level[1] %=>% Strings.LCase;
  }

  require(parallel);

  cores <- detectCores();

  if (cores == 1) {
    return(1);
  }

  if (level == "full") {
    return(cores);
  } else if (level == "half") {
    set.cores <- cores / 2;
  } else if (level == "parallel") {
    set.cores <- cores / 4;
  } else if (level == "single") {
    return(1);
  } else {
    stop(sprintf("Unknown option: level='%s'", level));
  }

  if (set.cores < 1) {
    set.cores <- 1;
  } else {
    set.cores <- round(set.cores);
  }

  set.cores;
}

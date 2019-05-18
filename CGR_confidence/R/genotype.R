
#' Measure sample range
#'
#' @param samples A numeric vector
#'
#' @return A genotype data model
#'
measure.range <- function(samples, foldchange = genotype.foldchange()) {
  data <- normal(samples %=>% quartile, samples);
  average <- mean(data);
  sd <- sd(data) / sqrt(length(data));
  high <- genotype(average, sd, 1, foldchange);
  low <- genotype(average, sd, -1, foldchange);

  list(low = low, high = high);
}

#' genotype range
#'
#' @param avg Average value of the samples normal data.
#' @param sd Standard error of the samples normal data.
#'
genotype <- function(avg, sd, direction, foldchange = genotype.foldchange()) {
  lapply(foldchange, function(level) {
    avg + direction * level * sd;
  });
}

genotype.foldchange <- function() {
  list(normal = 1.125, moderate = 2.5, critical = 5);
}

quartile <- function(samples) {
  q <- quantile(samples);
  q <- list(
    q1 = q[["25%"]],
    q2 = q[["50%"]],
    q3 = q[["75%"]],
    IQR = q[["75%"]] - q[["25%"]],
    min = min(samples),
    max = max(samples)
  );

  q;
}

outlier <- function(quartile, samples) {
  lower <- quartile$q1 - 1.5 * quartile$IQR;
  upper <- quartile$q3 + 1.5 * quartile$IQR;
  index <- (samples < lower) | (samples > upper);

  samples[index];
}

normal <- function(quartile, samples) {
  lower <- quartile$q1 - 1.5 * quartile$IQR;
  upper <- quartile$q3 + 1.5 * quartile$IQR;
  index <- (samples >= lower) & (samples <= upper);

  samples[index];
}

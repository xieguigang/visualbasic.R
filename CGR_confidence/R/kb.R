
#' Measure sample range
#'
#' @param samples A numeric vector
#'
#' @return A genotype data model
#'
measure.range <- function(samples) {
  data <- normal(samples %=>% quartile, samples);
  average <- mean(data);
  sd <-

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

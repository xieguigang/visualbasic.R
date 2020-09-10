read_table.auto <- function(path, row.names = TRUE, check.names = FALSE) {
  if (is.null(row.names) || is.na(row.names) || !row.names) {
    row.names = NULL;
  } else {
    row.names = 1;
  }

  if (!isTsv(path)) {
    read.csv(
      file        = path,
      row.names   = row.names,
      header      = T,
      check.names = check.names
    );
  } else {
    read.table(
      file        = path,
      header      = TRUE,
      sep         = "\t",
      row.names   = row.names,
      check.names = check.names
    );
  }
}

#' determine file format from file name
#'
#' @param path tsv: \code{*.txt}, \code{*.xls}; csv: \code{*.csv}
#'
isTsv <- function(path) {
  file <- base::basename(path);
  out  <- strsplit(x=file, split="\\.", fixed=FALSE, perl=TRUE, useBytes=FALSE);
  out  <- as.vector(out[[1]]);
  ext  <- tolower(out[length(out)]);
  ext != "csv";
}

#' Write table safely
#'
#' @param path create a file safely, create directory if the parent
#'    directory is not exists, and determine file format by the file
#'    name its extension name.
#'
#' @param data a \code{data.frame} object or matrix data object.
#'
write_table.auto <- function(data, path) {
  dir = dirname(path);
  dir.create(dir, recursive = TRUE);

  if (!isTsv(path)) {
    write.csv(data, file = path, row.names = TRUE);
  } else {
    write.table(data, file = path, sep = "\t");
  }
}

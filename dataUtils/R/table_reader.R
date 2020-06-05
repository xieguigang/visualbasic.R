read_table.auto <- function(path) {
  if (!isTsv(path)) {
    read.csv(path);
  } else {
    read.table(path, header = TRUE, sep = "\t");
  }
}

isTsv <- function(path) {
  file <- base::basename(path);
  out  <- strsplit(x=file, split="\\.", fixed=FALSE, perl=TRUE, useBytes=FALSE);
  out  <- as.vector(out[[1]]);
  ext  <- tolower(out[length(out)]);
  ext != "csv";
}

write_table.auto <- function(data, path) {
  dir = dirname(path);
  dir.create(dir, recursive = TRUE);

  if (!isTsv(path)) {
    write.csv(data, file = path, row.names = TRUE);
  } else {
    write.table(data, file = path, sep = "\t");
  }
}

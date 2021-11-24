#Region "Microsoft.ROpen::dea3937340b6d7c1a8b35cb5d0a02a4e, table_reader.R"

    # Summaries:

    # read_table.auto <- function(path, row.names = TRUE, check.names = FALSE, check.empty = TRUE) {...

#End Region

read_table.auto <- function(path, row.names = TRUE, check.names = FALSE, check.empty = TRUE) {
  options(stringsAsFactors = FALSE);

  if (is.null(row.names) || is.na(row.names) || !row.names) {
    row.names = NULL;
  } else {
    row.names = 1;
  }

  print("rownames:");
  print(row.names);

  if (!isTsv(path)) {
    table = read.csv(
      file        = path,
      row.names   = NULL,
      header      = T,
      check.names = check.names
    );
  } else {
    table = read.table(
      file        = path,
      header      = TRUE,
      sep         = "\t",
      row.names   = NULL,
      check.names = check.names
    );
  }

  if (check.empty) {
    i = sapply(1:nrow(table), function(j) {
      v = as.vector(unlist(table[j, ]));
      v = sapply(v, function(x) is.na(x) || x == "");

      !all(v);
    });

    table = table[i, ];
  }

  if (!is.null(row.names)) {
    rnames = as.vector(table[, row.names]);
    table[, row.names] = NULL;
    rownames(table) = uniqueNames(rnames);
  }

  table;
}

#Region "Microsoft.ROpen::1dd9bd372509b65b661f52e0264aabe7, uniqueNames.R"

    # Summaries:

    # uniqueNames = function(names) {...

#End Region

uniqueNames = function(names) {
  nhits   = list();
  uniques = c();

  for(name in names) {
    if (is.null(nhits[[name]])) {
      nhits[[name]] = 1;
      uniques = append(uniques, name);
    } else {
      uniq = sprintf("%s_%s", name, nhits[[name]]);
      nhits[[name]] = nhits[[name]] + 1;
      uniques = append(uniques, uniq);

      warning(sprintf("found duplicated name: %s", name));
    }
  }

  uniques;
}

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

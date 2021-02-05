
#' build commandline
#'
#' @description a helper function for build a command line string
#' for run docker commands.
#'
commandlineArgs = function(commandName, args = list()) {
  arguments = sapply(args, function(opt) {
    argNames = names(opt);
    tokens = sapply(argNames, function(name) {
      value  = opt[[name]];
      option = commandlineArgs.option(name, value);
      option;
    });

    paste(unlist(tokens), collapse = " ");
  });
  arguments = unlist(arguments);
  arguments = paste(arguments, collapse = " ");

  sprintf("docker %s %s", commandName, arguments);
}

commandlineArgs.option = function(name, value) {
  if (is.null(value) || length(value) == 0) {
    sprintf('%s ""', name);
  } else if (is.logical(value)) {
    if (value) {
      name;
    } else {
      "";
    }
  } else {
    if(length(value) > 0) {
      configs = c();

      for(item in value) {
        configs = append(configs, sprintf('%s "%s"', name, item));
      }

      paste(configs, collapse = " ");
    } else {
      sprintf('%s "%s"', name, toString(value));
    }
  }
}

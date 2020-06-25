
#' build commandline
#'
#' @description a helper function for build a command line string
#' for run docker commands.
#'
commandlineArgs = function(commandName, args = list()) {
  arguments = sapply(args, function(opt) {
    argNames = names(opt);
    tokens = sapply(argNames, function(name) {
      value = opt[[name]];

      if (is.null(value)) {
        sprintf('%s ""', name);
      } else if (is.logical(value)) {
        if (value) {
          name;
        } else {
          "";
        }
      } else {
        sprintf('%s "%s"', name, toString(value));
      }
    });

    paste(unlist(tokens), collapse = " ");
  });
  arguments = unlist(arguments);
  arguments = paste(arguments, collapse = " ");

  sprintf("docker %s %s", commandName, arguments);
}

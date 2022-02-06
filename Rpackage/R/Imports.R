
#' namespace imports helper
#'
#' @param namespace The namespace function name or function itself
#' @param overrides A logical flag to indicate that should the imported function
#'                  can overrides the previous function which they have the same
#'                  name. By default is can not. If this parameter is set to 
#'                  true, then a warning message will be generated for mention 
#'                  you that which functions are overrided.
#' @param frame The envrionment location, by default is current function scope
#' @param silent The function will keeps silent on processing the symbol 
#'         imports. If this parameter is set to \code{false}, then it means 
#'         all of the verbose debug message will print on the console 
#'         screen.
#'
#' @return A string vector contains all of the function names from this 
#'         namespace function that imported to current environment.
#'
Imports <- function(namespace, 
                    overrides = FALSE, 
                    silent    = TRUE, 
                    frame     = parent.frame()) {

  if (is.character(namespace)) {
    module <- get(namespace)();
  } else {
    module <- namespace();
    namespace <- module$namespace;
  }

  func.list <- if (!("methods" %in% names(module))) {
    module;
  } else {
    module$methods;
  }

  overrideMsg <- "overrides '%s' from namespace `%s`";

  for (name in names(func.list)) {
    if (exists(name, envir = frame)) {
      if (overrides) {
        warning(sprintf(overrideMsg, name, namespace));
      } else {
        next;
      }
    }

    assign <- list(name, func.list[[name]]);
    do.call(`=`, assign, envir = frame);
  }

  if ("modules" %in% names(module)) {
    # Is also contains export variable modules
    modules <- module$modules;

    for(name in names(modules)) {
      if (exists(name, envir = frame)) {
        if (overrides) {
          warning(sprintf(overrideMsg, name, namespace));
        } else {
          next;
        }
      }

      assign <- list(name, modules[[name]]);
      do.call(`=`, assign, envir = frame);
    }
  }

  if (!silent) {
    cat(sprintf("Imports VisualBasic.R::{%s}\n\n", namespace));
    print(names(func.list));
  }

  if (silent) {
    invisible(names(func.list));
  } else {
    names(func.list);
  }
}

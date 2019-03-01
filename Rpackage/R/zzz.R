#Region "Microsoft.ROpen::f44ced39ed98e80552879b2833cfd5c5, zzz.R"

    # Summaries:

    # .onLoad <- function(libname, pkgname) {...

#End Region

# https://stackoverflow.com/questions/20223601/r-how-to-run-some-code-on-load-of-package

# There is usually a "processing function" (traditionally called zzz.R) with tasks
# to be performed when the package is loaded, such as loading libraries and compiled
# code. For example you can create a zzz.R file where you create this function:

.onLoad <- function(libname, pkgname) {

  echo <- getOption("echo");

  if (is.null(echo) || is.na(echo)) {
    echo <- FALSE;
  }

  if (echo) {
    cat("\n");
  }

	# Doing some initialize work at here
	Imports("Microsoft.VisualBasic.Language", frame = globalenv(), silent = !echo);

	# https://stackoverflow.com/questions/45983899/getnamespaceexports-called-from-within-onload-package-function
  # .onLoad function is running before current package loaded
  # So a lot of the helper function in current loadding package can
  # not be called directly.
  index <- base::system.file("INDEX", package="VisualBasic.R") %=>% readLines %=>% Enumerator;
  index <- index$
    Where(function(line) InStr(line, "Microsoft.VisualBasic") > 0)$
    Select(function(line) Strings.Split(line)[1])$
    ToArray();

	if (echo) {
	  cat("\n");
	  cat("Namespace modules in current package:\n\n");
	}

	if (echo) {
	  for (namespace in index) {
	    module <- do.call(namespace, list());

	    cat(module$namespace);
	    cat("\t\t");
	    cat(module$description);
	    cat("\n");
	  }
	}

	if (echo) {
	  cat("\n");
	  cat("If any problem with this package, open an issue on github:\n\n");
	  cat("    https://github.com/xieguigang/visualbasic.R");
	  cat("\n\n");
	  cat("Or contact the author: \n\n");
	  cat("    xieguigang <xie.guigang@gcmodeller.org>");
	  cat("\n\n");
	}
}

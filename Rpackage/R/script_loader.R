#Region "Microsoft.ROpen::22d515022ffcbef3657bff2b8c9fb273, script_loader.R"

    # Summaries:

    # include <- function(R) {if (file.exists(R)) {...
    # flash_load <- function(dir = getwd()) {...
    # argv <- function() {...
    # is.argName <- function(x) {if (x %=>% IsNothing) {...
    # getNextToken <- function(flag) {...
    # cliToken <- function(arguments) {sapply(arguments, function(a) {  # Add quote char wrapper for argument token  # which have whitespace inside  if (InStr(a, " ") > -1) {...

#End Region

#' This function is using for solving the code compatibility
#' between the R package and R script.
#'
#' @param R R script file path.
#'
#' @return Nothing
include <- function(R) {
	if (file.exists(R)) {
		source(R);
	} else {
	  warning(sprintf("Missing '%s'", R));
	}

	invisible(NULL);
}

#' Load R script in directory
#'
#' @description Load all of the R script in a given working directory,
#'    by default is load all script in current directory.
#'
#' @param dir The script source directory, by default is current workspace.
#'
#' @return Nothing.
#'
flash_load <- function(dir = getwd()) {

	# Scan all of the *.R script in current workspace
	# And then source load each R script.
	scripts <- list.files(
		dir,
		recursive  = F,
		pattern    = "\\.[Rr]",
		full.names = T
	);

	for (script in scripts) {
		if (basename(script) != "script_loader" && File.WithExtension(script, "R")) {
			tryCatch({
				source(script);
			}, error = function(ex) {
				printf("Error while loading script: %s", script);
				print(toString(ex));
			});
		}

		print(script);
	}

	# run .onload in zzz.R
	zzz.R <- sprintf("%s/zzz.R", dir);

	# If a function which named .flashLoad is exists after source script file
	# Then it means required run the on load function in this zzz.R script file.
	# The function .flashLoad should only contains the direct calls of the
	# function .onLoad.
	# .flashLoad() <- function() .onLoad(NULL, NULL);
	if (file.exists(zzz.R) && exists(".flashLoad")) {
	  # In non-package status, the .onLoad function in zzz.R script file can not
	  # running automatically.
	  # Due to the reason of function .onLoad could be conflicts with the magic
	  # function .onLoad that from other package.
	  # So we call another magic function .flashLoad at here.
	  do.call(".flashLoad", list());
	}

	invisible(NULL);
}

#' Get commandline
#'
#' @note
#'
#' Commandline parser for cli expression pattern like:
#'
#' \code{Rscript script.R /name /arg1 value1 /arg2 value2 /boolean1 /arg3 value3}
#'
#' The \code{nextToken} function in the returns list object is
#' usually using for get value by a given key name.
#'
argv = function(arguments = base::commandArgs()) {
  # get all of the tokens after --args flag
  i <- which(arguments == "--args");

  if (length(i) == 0) {
    # No additional arguments
    .load_argv(cli = c());
  } else {
    if (.Platform$OS.type == "windows") {
      i <- i[1] + 2;
    } else {
      i <- i[1] + 1;
    }

    .load_argv(cli = arguments[i:length(arguments)]);
  }
}

.load_argv = function(cli) {
  name <- cli[1];
  args <- list();
  i    <- 2;
  is.argName <- function(x) {
    if (x %=>% IsNothing) {
      return (FALSE);
    }

    base::startsWith(x, "/")  ||
    base::startsWith(x, "--") ||
    base::startsWith(x, "-");
  }

  while(i < length(cli)) {
    argName = cli[i];

    if (cli[i + 1] %=>% is.argName) {
      # If the next element is the command argument name
      # then the current element is a logical flag
      args[argName] = TRUE;
      offset <- 1;
    } else {
      args[argName] = cli[i + 1];
      offset <- 2;
    }

    i = i + offset;
  }

  getNextToken = function(flag) {
    cli[which(cli == flag) + 1];
  }

  hasArg = function(flag) {
    any(cli == flag);
  }

  list(argv = cli,
       commandName = name,
       args = args,
       nextToken = getNextToken,
       hasArg = hasArg
  );
}

#' Create cli tokens
#'
#' @description Add quote char wrapper for commandline argument token
#' which have whitespace inside.
#'
#' @param arguments A character vector
#'
cliToken <- function(arguments) {
  sapply(arguments, function(a) {
    # Add quote char wrapper for argument token
    # which have whitespace inside
    if ((InStr(a, " ") > -1) || (InStr(a, "[(]") > -1) || (InStr(a, "[)]") > -1)) {
      sprintf("\"%s\"", a);
    } else {
      a;
    }
  }) %=>% as.vector;
}

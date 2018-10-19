#Region "Microsoft.ROpen::a51a9193057d0e1cbe32a58bbf63af9a, script_loader.R"

    # Summaries:

    # include <- function(R) {if (file.exists(R)) {...
    # flash_load <- function(dir = getwd()) {...

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

# flash_load();

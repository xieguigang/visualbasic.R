#' This function is using for solving the code compatibility
#' between the R package and R script.
#'
#' @param R R script file path.
#'
#' @return Nothing
include <- function(R) {
	if (file.exists(R)) {
		source(R);
	}

  invisible(NULL);
}

#' Load all of the R script in current working directory.
#'
#' @return Nothing.
flash_load <- function() {

	# 查找当前文件夹下面的所有的R脚本
	# 然后进行加载操作
	scripts <- list.files(
		getwd(),
		recursive  = F,
		pattern    = "\\.[Rr]",
		full.names = T
	);

	for (script in scripts) {
		if (basename(script) != "script_loader") {
			source(script);
		}

		print(script);
	}

	invisible(NULL);
}

# flash_load();

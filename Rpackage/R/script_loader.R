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

#' Load R script in current directory
#'
#' @description Load all of the R script in current working directory.
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
	zzz.R <- sprintf("%s/zzz.R", getwd());

	# 如果存在.flashLoad这个函数，表示会需要运行该zzz.R脚本之中的初始化过程
	# .flashLoad函数应该是只包含有.onLoad的直接调用代码的
	# .flashLoad() <- function() .onLoad(NULL, NULL);
	if (file.exists(zzz.R) && exists(".flashLoad")) {
	  # 非程序包的状态下，zzz.R之中的.onLoad无法自动运行
	  # 因为.onLoad可能与其他的程序包中的.onLoad函数冲突
	  # 所以在这里改为调用.flashLoad函数
	  do.call(".flashLoad", list());
	}

	invisible(NULL);
}

# flash_load();

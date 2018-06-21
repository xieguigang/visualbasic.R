imports <- function(namespace, overrides = FALSE) {
	frame       <- parent.frame();
	module      <- get(namespace, envir = frame);
	func.list   <- module();
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
	
	invisible(NULL);
}

# 判断对象是否为空
IsNothing <- function(x.) {
	is.null(x.) || is.na(x.) || length(x.) == 0;
}

# 如果对象为空则取默认值
`%||%` <- function(x, y) if(IsNothing(x)) y else x;

# 模拟C语言的打印函数
printf <- function(...) invisible(print(sprintf(...)));
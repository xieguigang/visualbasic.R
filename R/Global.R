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
	
	# invisible(NULL);
	names(func.list);
}

# 判断对象是否为空
IsNothing <- function(x.) is.null(x.) || is.na(x.) || length(x.) == 0;
# 模拟C语言的打印函数
printf <- function(...) invisible(print(sprintf(...)));

# syntax tweaks
microsoft.visualbasic.language <- function() {

	# 如果对象为空则取默认值
	`%||%` <- function(x, y) if(IsNothing(x)) y else x;

	## @param path: list(obj, "/path/ref");
	`%<=%` <- function(path, value) {
		
		# get parent environment
		.globalEnvir <- parent.frame();
		
		# set an invisible temp variable
		t        = "tmp_0x2C34B_assign_Helper"; 
		do.call(`=`, list(t, value), envir = .globalEnvir);
		
		x       <- path[[1]];
		x.value <- get(x);
		path    <- Strings.Split(path[[2]], "/");

		# Build the dynamics expression
		v   <- Strings.Join(path[2:length(path)], "$");
		exp <- parse(text = sprintf("%s$%s <- %s", x, v, t));
		eval(exp, envir = .globalEnvir);

		# removes the temp helper variable
		do.call(`rm`, list(t), envir = .globalEnvir);
		
		return(invisible(NULL)) 
	}

	# tuple syntax helper
	#
	# example:
	#
	# f <- function() list(TRUE, c(1,1,1,1,1));
	#
	# c(x,y) := f();
	#
	# > x
	# [1] TRUE
	# > y
	# [1] 1 1 1 1 1
	#
	# https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
	#
	':=' <- function(lhs, rhs) {
		frame <- parent.frame();
		lhs   <- as.list(substitute(lhs))
		
		if (length(lhs) > 1)
			lhs <- lhs[-1];
		if (length(lhs) == 1) {
			do.call(`=`, list(lhs[[1]], rhs), envir=frame)
			return(invisible(NULL));
		}
		
		if (is.function(rhs) || is(rhs, 'formula'))
			rhs <- list(rhs);
		if (length(lhs) > length(rhs))
			rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)));
			
		for (i in 1:length(lhs))
			do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame);
		
		invisible(NULL);
	}
	
	# Extension method operator
	`%=>%` <- function(x, y) { 
		y(x);   
	}
	
	list("%||%" = get("%||%"), 
		 "%<=%" = get("%<=%"), 
		 ":="   = get(":="),
		 "%=>%" = get("%=>%")		 
	);
}

## 函数返回primitiveTypes枚举之中的某一个类型
GetType <- function(x) {
	types <- primitiveTypes();

	if (is.data.frame(x) || is.matrix(x)) {
		types$data.frame;
	} else if (is.list(x)) {
		types$list;
	} else if (is.vector(x)){
		types$vector;
	} else {
		types$object;
	}
}

## 枚举出R语言之中的一些基础的数据类型，枚举值有：
## object, data.frame, list, vector
primitiveTypes <- function() {
	list(object = 0, data.frame = 1, list = 2, vector = 3);
}
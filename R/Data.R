# data.frame rows to list collection
.as.list <- function(d) {

	.list <- list();
	list  <- .to.list(d);
	names <- colnames(d);
	
	for (i in 1:nrow(d)) {
		l <- list();
	
		for (name in names) {
			l[[name]] <- list[[name]][i];			
		}
		
		.list[[i]] <- l;
	}
	
	.list;
}

.to.list <- function(d) {
	list  <- list();
	names <- colnames(d);
	
	for (name in names) {
		list[[name]] <- as.vector(unlist(d[, name]));
	}
	
	list;
}

# 有些时候dataframe取列的结果得到的是一个list？
# 这是一个什么bug？？？
# 尝试使用这个函数来消除这个需要调用unlist的bug
.as.matrix <- function(d) {

	names   <- colnames(d);
	columns <- lapply(names, function(col) {
		col <- d[, col];
		
		if (is.list(col)) {
			as.vector(unlist(col));
		} else {
			as.vector(col);
		}
	});
	
	         d  <- as.data.frame(columns);
	colnames(d) <- names;
	rownames(d) <- as.character(1:nrow(d));
	
	d;
}

SelectMany <- function(list, project) {
	v <- NULL;
	
	for(x in list) {
		v <- append(v, project(x));
	}
	
	v;
}

list.project <- function(list, fields) {
	l <- list();
	
	for (name in fields) {
		x <- list[[name]];
		
		if (is.null(x)) {
			l[[name]] <- NA;
		} else {
			l[[name]] <- x;
		}
	}
	
	l;
} 

as.dataframe <- function(list) {
	d <- NULL;
	list.names <- names(list);
	all.prop <- SelectMany(list, function(x) names(x)); 
	all.prop <- unique(all.prop);
	
	for (name in list.names) {
		x <- list[[name]];
		x <- list.project(x, all.prop);
		d <- rbind(d, x);
	}
	
	rownames(d) <- list.names;
	colnames(d) <- all.prop;
	d;
}

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
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

## 函数返回primitiveTypes枚举之中的某一个类型
GetType <- function(x) {
	if (is.data.frame(x) || is.matrix(x)) {
		primitiveTypes()$data.frame;
	} else if (is.list(x)) {
		primitiveTypes()$list;
	} else if (is.vector(x)){
		primitiveTypes()$vector;
	} else {
		primitiveTypes()$object;
	}
}

## 枚举出R语言之中的一些基础的数据类型，枚举值有：
## object, data.frame, list, vector
primitiveTypes <- function() {
	list(object = 0, data.frame = 1, list = 2, vector = 3);
}
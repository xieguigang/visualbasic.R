# data.frame rows to list collection
.as.list <- function(d) {	
	list  <- list();
	names <- colnames(d);
	
	for (name in names) {
		list[[name]] <- as.vector(d[, name]);
	}
	
	.list <- list();
	
	for (i in 1:nrow(d)) {
		l <- list();
	
		for (name in names) {
			l[[name]] <- list[[name]][i];			
		}
		
		.list[[i]] <- l;
	}
	
	.list;
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

## @param path: list(obj, "/path/ref");
':=' <- function(path, value) {
	
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
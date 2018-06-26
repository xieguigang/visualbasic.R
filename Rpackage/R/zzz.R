# https://stackoverflow.com/questions/20223601/r-how-to-run-some-code-on-load-of-package

# There is usually a "processing function" (traditionally called zzz.R) with tasks 
# to be performed when the package is loaded, such as loading libraries and compiled 
# code. For example you can create a zzz.R file where you create this function:

.onLoad <- function(libname, pkgname) {
	
	# 在这里执行一些初始化工作	
	Imports("Microsoft.VisualBasic.Language", frame = globalenv(), silent = FALSE);

	func.list    <- getNamespaceExports("VisualBasic.R")
	is.namespace <- sapply(func.list, function(name) {
		x <- (InStr(name, "Microsoft.VisualBasic") > 0);
		y <- is.function(get(name));

		x && y;
	});
	predicted <- as.vector(is.namespace);
	func.list <- as.vector(func.list)[predicted];

	for(namespace in func.list) {
		namespace <- do.call(namespace, list());
		cat(namespace$namespace);
		cat("\t\t");
		cat(namespace$description);
		cat("\n");
	}
}
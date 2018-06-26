# https://stackoverflow.com/questions/20223601/r-how-to-run-some-code-on-load-of-package

# There is usually a "processing function" (traditionally called zzz.R) with tasks 
# to be performed when the package is loaded, such as loading libraries and compiled 
# code. For example you can create a zzz.R file where you create this function:

.onLoad <- function(libname, pkgname) {
	
	# 在这里执行一些初始化工作	
	Imports("Microsoft.VisualBasic.Language", frame = globalenv(), silent = FALSE);

	try({
		list <- Enumerator(getNamespaceExports("VisualBasic.R"))$ 
			Where(function(name) InStr(name, "Microsoft.VisualBasic") > 0)$
			Where(function(name) is.function(get(name)));

		for(namespace in list$ToArray()) {
			namespace <- do.call(namespace, list());
			cat(namespace$namespace);
			cat("\t\t");
			cat(namespace$description);
			cat("\n");
		}
	})
}
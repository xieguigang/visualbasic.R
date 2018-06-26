# https://stackoverflow.com/questions/20223601/r-how-to-run-some-code-on-load-of-package

# There is usually a "processing function" (traditionally called zzz.R) with tasks 
# to be performed when the package is loaded, such as loading libraries and compiled 
# code. For example you can create a zzz.R file where you create this function:

.onLoad <- function(libname, pkgname){
	# 在这里执行一些初始化工作
	
	imports("Microsoft.VisualBasic.Language", frame = parent.frame(), silent = FALSE);
}
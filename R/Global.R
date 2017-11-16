include.list <- c();

DEBUG <- function() {

}

Imports <- function(Rscript, overrides = FALSE) {
    path <- tools::file_path_as_absolute(Rscript);

    if (sum(include.list == path) == 0) {

        if (file.exists(path)) {
            source(path);
            include.list <<- append(include.list, path);
        } else {
            if (DEBUG()) {
                warning(sprintf("'%s' is at a invalid location!", Rscript));
            }
        }

    } else {
        
        if (overrides) {
            if (file.exists(path)) { 
                source(path);
            } else {
                if (DEBUG()) {
                    warning(sprintf("Ask for a overrides Imports, but '%s' is at a invalid location!", Rscript));
                }
            }           
        }
    }   
}

# 判断对象是否为空
IsNothing <- function(x.) {
	is.null(x.) || is.na(x.) || length(x.) == 0;
}

# 如果对象为空则取默认值
`%||%` <- function(x, y) if(IsNothing(x)) y else x;

# 模拟C语言的打印函数
printf <- function(...) invisible(print(sprintf(...)));
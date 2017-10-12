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
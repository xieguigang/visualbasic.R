parse.package.description <- function(file = base::system.file("DESCRIPTION", package="VisualBasic.R")) {
    description <- list();
    last <- "";
    name <- "";

    for (line in file %=>% readLines) {
        if (InStr(line, " ") == 1) {
            # line continute
            last <- sprintf("%s\n%s", last, line);
        } else {
            if (name != "") {
                description[[name]] = last;
            }            
            c(name, last) := GetTagValue(line, ":");
        }
    }

    if (name != "") {
        description[[name]] = last;
    }

    description;
}

DESCRIPTION <- function(packageName) {
    parse.package.description(base::system.file("DESCRIPTION", package=packageName));
}
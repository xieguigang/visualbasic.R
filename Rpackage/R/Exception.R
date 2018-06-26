#' Get the name of current function
GetCurrentFunc <- function() {
    stacks <- sys.calls();
    name   <- stacks[[length(stacks) - 1]];    
    name   <- Strings.Split(toString(name), "\\(")[1];
    name;
}

#' Cast to logical type
#' 
CBool = function(x) {
    if (IsNothing(x)) {
        FALSE;
    } else {
        as.logical(x);
    }
}
#' parse size value
#' 
#' @param value value can be character, numeric and list
#' @param default the default size value is the parser test failure on the
#'      value parameter.
#' 
#' @return a list object that contains fields \code{w} and \code{h}.
#' 
parseSize = function(value, default = list(w = 1400, h = 1200)) {
    if (length(value) == 0 || is.null(value) || is.na(value)) {
        if (is.null(default)) {
            stop("parameter value is nothing!");
        } else {
            return(default);
        }
    } elseif (class(value) == "character") {
        value = strsplit(value[1], "\\s*,\\s*", perl = TRUE);
        value = as.numeric(value);
        value = list(
            w = value[1],
            h = value[2]
        );
    } elseif (class(value) == "numeric") {
        value = list(
            w = value[1],
            h = value[2]
        );
    } elseif (class(value) == "list") {
        # do nothing
    } elseif(is.null(default)) {
        stop(sprintf("invalid object type of the parameter value: %s", class(value)));
    } else {
        value = default;
    }

    value;
}
#Region "Microsoft.ROpen::0e3657d33644d2fb057d076b0043867c, binarySearch.R"

    # Summaries:

    # binarySearch <- function(source, find, key, compares = function(a, b) a - b) {...
    # binarySearch.list <- function(list, find, key, compares = function(a, b) a - b) {...
    # binarySearch.impl.generic <- function(ikey, .length, find, compares) {...
    # binarySearch.dataframe <- function(dataframe, find, key, compares = function(a, b) a - b) {...
    # sort.list <- function(list, key, key.numeric = function(v) as.numeric(v), desc = FALSE) {if (!is.function(key)) {...
    # sort.dataframe <- function(dataframe, key, key.numeric = function(v) as.numeric(v), desc = FALSE) {...

#End Region

#' Perform binary search on a sortted sequence.
#'
#' @param source The input sequence, this input source sequence must in sorted in
#'               asc or desc order.
#' @param find Target key for find the target element in the input source sequence.
#' @param compares The comparision method that using for find target element by compares
#'                 the target key and the key indexer values from the source sequence, by
#'                 default is comparision based on their numeric values.
#' @param key A lambda function that describ how to abstract the key indexer value
#'            from the elements of the input source sequence.
#'
#' @details If there are duplicated items in the \code{source} sequence, then
#'          you should group the items at first and then perfamence the binary search.
binarySearch <- function(source, find, key, compares = function(a, b) a - b) {
    type <- GetType(source);

    if (type == primitiveTypes()$data.frame) {
        binarySearch.dataframe(source, find, key, compares);
    } else if (type == primitiveTypes()$list) {
        binarySearch.list(source, find, key, compares);
    } else {
        stop("Not Supported!");
    }
}

#' Binary search on a \code{list}
#'
#' @seealso \code{\link{binarySearch.impl.generic}}
#'
binarySearch.list <- function(list, find, key, compares = function(a, b) a - b) {
    i <- binarySearch.impl.generic(
        function(i) key(list[[i]]),
        length(list),
        find,
        compares
    );

    if (i > -1) {
        list[[i]];
    } else {
        NULL;
    }
}

#' A generic function for sort sequence
#'
#' @description A internal private function which find the index of the element in the
#' input sequence which match a specific target key.
#'
#' @return This function returns the index i of the input sequence.
#'
binarySearch.impl.generic <- function(ikey, .length, find, compares) {
    L <- 1;
    R <- .length;
    i <- -1;

    while(L <= R) {
        m <- floor((L + R) / 2);
        c <- compares(ikey(m), find);

        if (c < 0) {
            L <- m + 1;
        } else if (c > 0) {
            R <- m - 1;
        } else {
            i <- m;
            break;
        }
    }

    i;
}

#' Binary Search on a \code{dataframe}
#'
#' @param dataframe A dataframe object
#' @param key The name of the key indexer column. And this key index column value should be ASC ordered.
#' @param compares A lambda function that generates the comparision numeric value between the
#'     \code{key} indexer element values.
#'
#' @details For the \code{compares} lambda function, it should accept two parameter and in format like:
#'
#'     \code{compares = function(a, b) a - b}
#'
#'     And this function should generates a numeric value for indicate the object comparision result:
#'
#'      0: means \code{a} equals to \code{b}\cr
#'      1: means \code{a} greater than \code{b} \cr
#'     -1: means \code{a} less than \code{b}
#'
#' @seealso \code{\link{binarySearch.impl.generic}}
binarySearch.dataframe <- function(dataframe, find, key, compares = function(a, b) a - b) {
    key <- as.vector(dataframe[, key]);
    i   <- binarySearch.impl.generic(function(i) key[i], length(key), find, compares);

    if (i > -1) {
        dataframe[i, ];
    } else {
        NULL;
    }
}

#' Sort the \code{list} object
#'
#' @description We assume that all of the elements in the input list is list object, and the key attribute
#' should exists in each list element.
#'
#' @param list The input list object
#' @param key A lambda function or property name string for get key value for the sort
#'            operation.
#' @param key.numeric A lambda function for evaluate the key value to numeric value
#' @param desc A logical flag to indicated that sort the input sequence in ASC or DESC mode?
#'
#' @return A data sequence which its elements has been reordered.
#'
sort.list <- function(list, key, key.numeric = function(v) as.numeric(v), desc = FALSE) {
    if (!is.function(key)) {
        getkey <- function(x) x[[key]];
    } else {
		getkey <- key;
	}

    listnames <- names(list);
    keys  <- sapply(listnames, function(name) {
        x <- list[[name]];
        key.numeric(getkey(x));
    });
    orders <- order(as.numeric(keys), decreasing = desc);
    listnames   <- listnames[orders];
    list        <- list[orders];
    names(list) <- listnames;

    list;
}

#' Sort the \code{dataframe}
#'
#' @description Sort a dataframe by a specific given column name or key indexer.
#'
#' @param key The key column name or a lambda function to summary the
#'            rows to a specific key
#' @param key.numeric This lambda function describ how to converts the specific key
#'                    column/indexer values to the numeric values which is use for
#'                    sort the rows based on these numeric value comparsion.
#' @param desc Sort in descending order? by default is not.
#'
sort.dataframe <- function(dataframe, key, key.numeric = function(v) as.numeric(v), desc = FALSE) {
    dataframe[order(key.numeric(dataframe[, key]), decreasing = desc), ];
}

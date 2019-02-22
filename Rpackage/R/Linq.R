#Region "Microsoft.ROpen::c45af1a9885cf6189f88d45f2ae04eff, Linq.R"

    # Summaries:

    # Microsoft.VisualBasic.Data.Linq <- function() {Take <- function(enumerable, m) {...
    # Skip <- function(enumerable, m) {...
    # Last <- function(enumerable) {...
    # WhichIsNotEmpty <- function(enumerable, assert = IsNothing) {...
    # GroupBy <- function(enumerable, key, type = c("data.frame", "list")) {if (type == "data.frame") {...
    # GroupBy.list <- function(list, key) {...
    # GroupBy.dataframe <- function(data.frame, key) {...
    # Group <- function(seq, case.Sensitive = FALSE) {...
    # Count <- function(groups) {...

#End Region

#' Linq helper in R language.
#'
#' @return \enumerate{
#'     \item \code{WhichIsNotEmpty} returns the index in the input
#'           \code{vector}/\code{list} sequence where is not nothing.
#' }
Microsoft.VisualBasic.Data.Linq <- function() {

	Take <- function(enumerable, m) {
		enumerable[1:m];
	}

	Skip <- function(enumerable, m) {
		enumerable[(m + 1):length(enumerable)];
	}

	Last <- function(enumerable) {
		type  <- GetType(enumerable);
		types <- primitiveTypes();

		if (IsNothing(enumerable)) {
			return(NULL);
		}

		if (type == types$vector) {
			enumerable[length(enumerable)];
		} else if (type == types$list) {
			enumerable[[length(enumerable)]];
		} else if (type == type$data.frame) {
			enumerable[nrow(enumerable), ];
		} else {
			stop("Object is not a enumerable type!");
		}
	}

	WhichIsNotEmpty <- function(enumerable, assert = IsNothing) {
		(!sapply(enumerable, assert)) %=>% which;
	}

	# (c(5,6,7,8,9) %=>% Take)(2)
	# [1] 5 6

	# (c(5,6,7,8,9,1,2,3) %=>% Skip)(5)
	# [1] 1 2 3

	#' group data.frame/list by keys
	#'
	#' @param enumerable The data source collection for the group operation, by default is ``data.frame`` type.
	#' @param key The column name or property name for using its corresponding value as the group key
	#' @param type Indicate that the data source ``enumerable`` is a data.frame or list?
	#'        default is a data.frame.
	GroupBy <- function(enumerable, key, type = c("data.frame", "list")) {

		if (type == "data.frame") {
			GroupBy.dataframe(data.frame = enumerable, key = key);
		} else if (type == "list") {
			GroupBy.list(list = enumerable, key = key);
		} else {
			stop("Not supported!");
		}

	}

	#' Groups the items in a list by s specific property of the list item.
	#'
	#' @param list The data source in list type
	#' @param key The item property name in this list source collection.
	#'
	GroupBy.list <- function(list, key) {

		groups <- list();
		tick   <- tick.helper(length(list));
		cat("\n");
		cat("  Progress%: ");

		for (i in 1:length(list)) {
			item      <- list[[i]];
			group.key <- item[[key]];
			key.list  <- groups[[group.key]];

			if (is.null(key.list)) {
				key.list <- list();
			}

			key.list[[length(key.list) + 1]] <- item;
			groups[[group.key]] <- key.list;
			tick();
		}

		cat("\n");
		cat("\n");

		groups;
	}

	#' Groups the rows in a dataframe by a specific column as key.
	#'
	#' @param data.frame The data source in data.frame type
	#' @param key The column name for read the column data in target source as the group key.
	GroupBy.dataframe <- function(data.frame, key) {
		
		keys    <- as.vector(unlist(data.frame[, key]));
		cols    <- colnames(data.frame);
		columns <- lapply(cols, function(col) {
			as.vector(unlist(data.frame[, col]));
		});
		names(columns) <- cols;
				
		clusters <- list();
		
		for (i in 1:length(keys)) {
			# group keys and get i index clusters
			clusters[[keys[i]]] <- append(clusters[[keys[i]]], i);
		}
		
		# and then get group data by i index cluster
		groups <- list();

		for (key in names(clusters)) {
			i <- clusters[[key]];
			sub <- lapply(cols, function(col) {
				v <- columns[[col]];
				v[i];
			});
			
			groups[[key]] <- cbind.dataframe(sub);
		}
		
		groups;
	}

	#' Group the string collection
	Group <- function(seq, case.Sensitive = FALSE) {

		`%||%` <- function(x, y) if(case.Sensitive) x else y;
		groups <- list();

		for (x in seq) {
			key   <- x %||% tolower(x);
			group <- groups[[key]];

			if (is.null(group)) {
				groups[[key]] <- x;
			} else {
				groups[[key]] <- append(group, x);
			}
		}

		groups;
	}

	#' Example:
	#'
	#' \code{
	#'   Group(c("A","B","C","c")) %=>% Count;\cr\cr
	#'
	#'   $A: 1\cr
	#'   $B: 1\cr
	#'   $C: 2\cr
	#' }
	#'
	#' This function is usually piped with the \code{Group} function
	Count <- function(groups) {
		names  <- names(groups);
		counts <- lapply(names, function(key) length(groups[[key]]));
		names(counts) <- names;
		counts;
	}

	from <- function(source) Enumerator(source);

	list(namespace = GetCurrentFunc(),
		 description = "Linq helper function in R language.",
		 methods = list(
			From    = get("from"),
			Take    = get("Take"),
			Skip    = get("Skip"),
			Count   = get("Count"),
			Group   = get("Group"),
			Last    = get("Last"),
			GroupBy = get("GroupBy"),
			WhichIsNotEmpty = get("WhichIsNotEmpty")
	));
}

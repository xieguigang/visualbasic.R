#Region "Microsoft.ROpen::37751c2b9110d87150c6804617ce2b6e, Linq.R"

    # Summaries:

    # Microsoft.VisualBasic.Data.Linq <- function() {Take <- function(enumerable, m) {...
    # Skip <- function(enumerable, m) {...
    # Last <- function(enumerable) {...
    # WhichIsNotEmpty <- function(enumerable, assert = IsNothing) {...
    # GroupBy <- function(enumerable,key,type = c("data.frame", "list"),verbose = FALSE) {if (type == "data.frame") {...
    # GroupBy.list <- function(list, key, verbose) {...
    # GroupBy.dataframe <- function(data.frame, key, verbose = FALSE) {if (is.character(key)) {...
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
	#' @param enumerable The data source collection for the group operation,
	#'     by default is ``data.frame`` type.
	#' @param key The column name string (or a column projector function in data.frame)
	#'     or property name (in list) for using its corresponding value as the group
	#'     key
	#' @param type Indicate that the data source ``enumerable`` is a data.frame or list?
	#'        default is a data.frame.
	#'
	GroupBy <- function(
	  enumerable,
    key,
    type = c("data.frame", "list"),
    verbose = FALSE, show.progress = FALSE) {

		if (type == "data.frame") {
			GroupBy.dataframe(data.frame = enumerable, key = key, verbose = verbose, show.progress = show.progress);
		} else if (type == "list") {
			GroupBy.list(list = enumerable, key = key, verbose = verbose);
		} else {
			stop("Not supported!");
		}

	}

	#' Groups the items in a list by s specific property of the list item.
	#'
	#' @param list The data source in list type
	#' @param key The item property name in this list source collection.
	#'
	GroupBy.list <- function(list, key, verbose) {

		# should removes all of the null element at first
		# or error happens:
		# Error in groups[[group.key]] :
		#    attempt to select less than one element in get1index

		notNulls <- lapply(list, function(x) !IsNothing(x[[key]])) %=>% unlist %=>% as.logical;

		if (sum(notNulls) < length(notNulls)) {
			# remove null elements
			list <- list[notNulls];

			msg <- "GroupBy(list, '%s', 'list'): Some element is NULL from your list source, these NULL element will be removed automatic";
			msg <- sprintf(msg, toString(key));

			if (verbose) {
				printf("\n%s\n", msg);
			}

			warning(msg);
		}

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
	#' @param key The column name in string mode or column projector function
	#'     for read the column data in target source as the group key.
	GroupBy.dataframe <- function(data.frame, key, verbose = FALSE, show.progress = FALSE) {

	  if (is.character(key)) {
	    keys <- as.vector(unlist(data.frame[, key]));
	    keys <- sapply(keys, toString) %=>% as.character;
	  } else {
	    keys <- key(data.frame);
	  }

		cols    <- colnames(data.frame);
		columns <- lapply(cols, function(col) {
			as.vector(unlist(data.frame[, col]));
		});
		names(columns) <- cols;
		cbind.dataframe <- Eval(Microsoft.VisualBasic.Data)$cbind.dataframe;

		if (verbose) {
		  print("group keys and get i index clusters");
		}

		# group keys and get i index clusters
		unique_keys <- keys %=>% unique
		clusters <- lapply(unique_keys, function(unique_key) which(keys == unique_key));
    names(clusters) <- unique_keys;

		if (verbose) {
		  printf("Unique key(size=%s)", length(unique_keys));
		}

		# and then get group data by i index cluster
		groups <- list();

		if (show.progress) {
		  tick <- tick.helper(length(unique_keys));
		  cat("\n");
		  cat("  Progress%: ");
		} else {
		  tick <- function() {
		    # do nothing
		  }
		}

		for (key in names(clusters)) {
			i <- clusters[[key]];
			subv <- lapply(cols, function(col) {
				v <- columns[[col]];
				v[i];
			});
			names(subv) <- cols;

			groups[[key]] <- cbind.dataframe(subv);
			tick();
		}

		if (show.progress) {
		  cat("\n\n");
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

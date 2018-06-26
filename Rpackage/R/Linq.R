#' Linq helper in R language.
microsoft.visualbasic.data.linq <- function() {

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
		is.true <- sapply(enumerable, function(x) !assert(x));
		which(is.true);
	}

	# (c(5,6,7,8,9) %=>% Take)(2)
	# [1] 5 6

	# (c(5,6,7,8,9,1,2,3) %=>% Skip)(5)
	# [1] 1 2 3

	# group data.frame/list by keys
	#
	# @param enumerable The data source collection for the group operation, by default is ``data.frame`` type.
	# @param key The column name or property name for using its corresponding value as the group key
	# @param type Indicate that the data source ``enumerable`` is a data.frame or list? default is a data.frame.
	GroupBy <- function(enumerable, key, type = c("data.frame", "list")) {

		if (type == "data.frame") {
			GroupBy.dataframe(data.frame = enumerable, key = key);
		} else if (type == "list") {
			GroupBy.list(list = enumerable, key = key);
		} else {
			stop("Not supported!");
		}

	}

	# Groups the items in a list by s specific property of the list item.
	#
	# @param list The data source in list type
	# @param key The item property name in this list source collection.
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

		return(groups);
	}

	# Groups the rows in a dataframe by a specific column as key.
	#
	# @param data.frame The data source in data.frame type
	# @param key The column name for read the column data in target source as the group key.
	GroupBy.dataframe <- function(data.frame, key) {

		groups <- list();
		keys   <- as.vector(unlist(data.frame[, key]));

		tick   <- tick.helper(nrow(data.frame));
		cat("\n");
		cat("  Progress%: ");

		for (i in 1:nrow(data.frame)) {
			row <- data.frame[i, ];
			key <- keys[i];
			groups[[key]] <- rbind(groups[[key]], row);
			tick();
		}

		cat("\n");
		cat("\n");

		return(groups);
	}

	# Group the string collection
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

	# Example:
	#
	# Group(c("A","B","C","c")) %=>% Count;
	#
	# $A: 1
	# $B: 1
	# $C: 2
	#
	# 这个函数通过是和Group函数构成一个流程来使用的
	Count <- function(groups) {
		names  <- names(groups);
		counts <- lapply(names, function(key) length(groups[[key]]));
		names(counts) <- names;
		counts;
	}

	from <- function(source) enumerator(source);

	list(
		from    = get("from"), 
		Take    = get("Take"),
		Skip    = get("Skip"),
		Count   = get("Count"),
		Group   = get("Group"),
		Last    = get("Last"),
		GroupBy = get("GroupBy")
	);
}




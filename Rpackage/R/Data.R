#' Common data operation helper function
#'
#' @details \enumerate{
#'    \item \code{ensure.dataframe} Fix for a bug in R language: for ensure that
#'          when get one row from a dataframe didn't output a list object, this
#'          function make sure the subset of a dataframe is always output a
#'          dataframe object.
#'    \item \code{.as.list} Converts the \code{data.frame} object to a set of
#'          \code{list} object from each rows data.
#' }
Microsoft.VisualBasic.Data <- function() {

	# data.frame rows to list collection
	.as.list <- function(d) {

		.list <- list();
		list  <- .to.list(d);
		names <- colnames(d);

		for (i in 1:nrow(d)) {
			l <- list();

			for (name in names) {
				l[[name]] <- list[[name]][i];
			}

			.list[[i]] <- l;
		}

		.list;
	}

	.to.list <- function(d) {
		list  <- list();
		names <- colnames(d);

		for (name in names) {
			list[[name]] <- as.vector(unlist(d[, name]));
		}

		list;
	}

	# 有些时候dataframe取列的结果得到的是一个list？
	# 这是一个什么bug？？？
	# 尝试使用这个函数来消除这个需要调用unlist的bug
	.as.matrix <- function(d, character = FALSE) {

	  names   <- colnames(d);
	  columns <- lapply(names, function(col) {
	    col    <- d[, col];
	    vector <- NULL;

	    if (is.list(col)) {
	      vector <- as.vector(unlist(col));
	    } else {
	      vector <- as.vector(col);
	    }

	    if (character) {
	      as.character(vector);
	    } else {
	      vector;
	    }
	  });

	  d  <- as.data.frame(columns);
	  colnames(d) <- names;
	  rownames(d) <- as.character(1:nrow(d));

	  d;
	}

	selectMany <- function(list, project) {
		v <- NULL;

		for(x in list) {
			v <- append(v, project(x));
		}

		v;
	}

	list.project <- function(list, fields) {
		l <- list();

		for (name in fields) {
			x <- list[[name]];

			if (is.null(x)) {
				l[[name]] <- NA;
			} else {
				l[[name]] <- x;
			}
		}

		l;
	}

	.as.dataframe <- function(list) {
		d <- NULL;
		list.names <- names(list);
		all.prop <- selectMany(list, function(x) names(x));
		all.prop <- unique(all.prop);

		for (name in list.names) {
			x <- list[[name]];
			x <- list.project(x, all.prop);
			d <- rbind(d, x);
		}

		rownames(d) <- list.names;
		colnames(d) <- all.prop;
		d;
	}

	## 确保结果数据是一个dataframe来的
	## 因为有时候对dataframe取子集的时候，假若最终的子集和只有一行数据，
	## 那么结果数据可能会被转换为一个vector，从而无法再被当做为dataframe
	## 使用，使用这个函数来确保取子集的结果全部都是dataframe
	.ensure.dataframe <- function(data, col.names) {
	  if (is.null(nrow(data))) {
	    data <- rbind(NULL, data);
	    colnames(data) <- col.names;
	  }

	  data;
	}

	# register function for namespace export
    list(namespace = GetCurrentFunc(),
		 description = "Namespace contains some common data operation helpers.",
		 methods = list(
			 .as.list     = .as.list,
		 	 .to.list     = .to.list,
		 	 .as.matrix   = .as.matrix,
		 	 .selectMany  = selectMany,
		 	 list.project = list.project,
		 	 as.dataframe = .as.dataframe,
			 ensure.dataframe = .ensure.dataframe
	));
}

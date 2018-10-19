#Region "Microsoft.ROpen::5c849b214bcf10748c602dff84d8fbb5, Data.R"

    # Summaries:

    # Microsoft.VisualBasic.Data <- function() {# data.frame rows to list collection.as.list <- function(d, rowname.as.listname = FALSE) {...
    # .to.list <- function(d) {...
    # .as.matrix <- function(d, character = FALSE) {...
    # selectMany <- function(list, project) {...
    # list.project <- function(list, fields) {...
    # .as.dataframe <- function(list) {...
    # .ensure.dataframe <- function(data, col.names) {if (is.null(nrow(data))) {...
    # .load.dataset <- function(file) {...

#End Region

#' Common data operation helper function
#'
#' @details \enumerate{
#'    \item \code{ensure.dataframe} Fix for a bug in R language: for ensure that
#'          when get one row from a dataframe didn't output a list object, this
#'          function make sure the subset of a dataframe is always output a
#'          dataframe object.
#'
#'    \item \code{.as.list} Converts the \code{data.frame} object to a set of
#'          \code{list} object from each rows data.
#'
#'    \item \code{read.dataset} Read the csv file as matrix, the first column from
#'          the csv file will be using as the rownames and the first row will be
#'          using as the colnames.
#' }
Microsoft.VisualBasic.Data <- function() {

	# data.frame rows to list collection
	.as.list <- function(d, rowname.as.listname = FALSE) {

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

		if (rowname.as.listname) {
		  names <- rownames(d);

		  if (!IsNothing(names)) {
		    names(.list) <- names;
		  }
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

	# There is a bug in R language dataframe subset:
	# The column value in a dataframe is a list sometimes?
	#
	# This function trying to eliminate this subset bug.
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
		list.names <- names(list);

		if (list.names %=>% IsNothing) {
			list.names <- 1:length(list);
		}

		# Get all slot name in list members
		all.prop <- selectMany(list, function(x) names(x));
		all.prop <- unique(all.prop);

		vectors <- lapply(all.prop, function(col) {
		  sapply(list.names, function(i) {
		    m <- list[[i]];
		    m[[col]];
		  }) %=>% as.vector;
		})

		d <- NULL;

		for (i in 1:length(vectors)) {
		  col <- vectors[[i]];
		  d   <- cbind(d, col);
		}

		colnames(d) <- all.prop;
		rownames(d) <- list.names;
		d;
	}

	#' ensure the result is a dataframe object
	#'
	#' There is a bug in R dataframe subset operation:
	#' If the subset result just one row,
	#' Then the subset result will be convert a vector automatically,
	#' not a dataframe any more. This will cause bugs and code inconsist.
	#' Using this function to ensure all of your dataframe subset result
	#' is a dataframe object, not a vector.
	.ensure.dataframe <- function(data, col.names) {
	  if (is.null(nrow(data))) {
	    data <- rbind(NULL, data);
	    colnames(data) <- col.names;
	  }

	  data;
	}

	.load.dataset <- function(file) {
	  d           <- read.csv(file);
	  rowNames    <- d[, 1] %=>% as.character;
	  d[, 1]      <- NULL;
	  rownames(d) <- rowNames;

	  d;
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
			 ensure.dataframe = .ensure.dataframe,
			 read.dataset     = .load.dataset
	));
}

#' Enumerator object constructor
#'
#' @description Enumerator object constructor function for create a VB.NET linq like data
#' sequence wrapper. This constructor function will create a list which contains a sequence
#' source and sevral linq data helper extension functions
#'
#' @param src A generic type data sequence, which can be a \code{dataframe}, \code{list}, or \code{vector}.
#'
#' @return A generic enumerator list object, which it contains:
#'         \enumerate{
#'         \item \code{src} The generic data sequence input
#'         \item \code{where} Extension function for select values by test on condition for each element if they are true
#'         \item \code{select} Project the input data sequence source to another form.
#'         \item \code{toarray} Get the input source data sequence.
#'         \item \code{orderBy}
#'         \item \code{orderByDescending}
#'         }
enumerator <- function(src) {

	imports("microsoft.visualbasic.language");

	type   <- GetType(src);
	types  <- primitiveTypes();

	#region "linq functions"

	.select <- function(project) {
		if (type == types$data.frame) {
			stop("Incompatible type!");
		} else if (type == types$list) {
			if (is.function(project)) {
				lapply(src, project);
			} else {
				microsoft.visualbasic.data()$list.project(src, project);
			}
		} else if (type == types$vector) {
			lapply(src, project);
		} else {
			stop("Incompatible type!");
		}
	}

	.where <- function(assert) {
		if (type == types$data.frame) {
			stop("Incompatible type!");
		} else if (type == types$list) {

			names <- names(src)
			test  <- sapply(names, function(name) assert(src[[name]])) %=>% which %=>% as.integer;
			list  <- src[names[test]];

			list;

		} else if (type == types$vector) {

			test <- sapply(src, function(x) assert(x)) %=>% which %=>% as.integer;
			list <- src[test];

		} else {
			stop("Incompatible type!");
		}
	}

	#' Reorder the elements in the source sequence by a given \code{key}
	#'
	#' @param key Can be a string name which can select the property of
	#'            the list object or a lambda function for evaluate the
	#'            element object to numeric value.
	.orderBy <- function(key, key.numeric = function(x) as.numeric(x)) {
    sort.list(src, key, key.numeric, FALSE);
	}

	.orderByDescending <- function(key, key.numeric = function(x) as.numeric(x)) {
	  sort.list(src, key, key.numeric, TRUE);
	}

	#endregion

	list(src   = src,
		 select  = function(project) enumerator(.select(project)),
		 where   = function(assert) enumerator(.where(assert)),
		 orderBy = function(key, key.numeric = as.numeric) enumerator(.orderBy(key, key.numeric)),
		 orderByDescending = function(key, key.numeric = as.numeric) enumerator(.orderBy(key, key.numeric, TRUE)),
		 toarray = function() src
	);
}

#' Enumerator object constructor function for create a VB.NET linq like data sequence wrapper.
#' This constructor function will create a list which contains a sequence source and sevral
#' linq data helper extension functions
#'
#' @param src A generic type data sequence, which can be a \code{dataframe}, \code{list}, or \code{vector}.
#'
#' @return A generic enumerator list object, which it contains:
#'         + \code{src} The generic data sequence input
#'         + \code{where} Extension function for select values by test on condition for each element if they are true
#'         + \code{select} Project the input data sequence source to another form.
#'         + \code{toarray} Get the input source data sequence.
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

	#endregion

	list(src     = src,
		 select  = function(project) enumerator(.select(project)),
		 where   = function(assert) enumerator(.where(assert)),
		 toarray = function() src
	);
}

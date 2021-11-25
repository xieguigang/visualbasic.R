#' Run R# session service
#' 
#' @return this function returns a session object 
#'    of the R# session service.
#'
service = function(debug = getOption("debug")) {
	renv = .searchREnv(debug);
	commandArgs = "dotnet %s";
}

.searchREnv = function(debug) {

}

.sessionClass = function() {
	setClass("Rsession", representation(
		port  = "numeric",
		key   = "character",
		debug = "logical"
	));
}

#' Invoke a \code{R#} function
#'
#' @param func the function name
#' @param argv the argument value list.
#' @param session the R# web service session handle.
#'
invoke = function(func, argv, session) {
	url = "http://localhost:%s/exec?func=%s&session=%s&debug=%s";
	url = sprintf(url, session@port, func, session@key, session@debug);
}
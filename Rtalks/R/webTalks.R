#' Run R# session service
#' 
#' @return this function returns a session object 
#'    of the R# session service.
#'
service = function(debug = getOption("debug")) {
	renv = .searchREnv(debug);
	session_port = .port();
	session_id = .ssid();
	commandArgs = "dotnet %s --session --port %s --ssid %s --workspace \"%s\"";
	commandArgs = sprintf(commandArgs, renv, session_port, session_id, getwd());
	
	system(commandArgs, 
		show.output.on.console = FALSE, 
		wait = FALSE
	);
	
	new("Rsession",
		port  = session_port,
		ssid  = session_id,
		debug = debug
	);
}

.ssid = function() {

}

.port = function(debug) {

}

.searchREnv = function(debug) {

}

.sessionClass = function() {
	setClass("Rsession", representation(
		port  = "numeric",
		ssid  = "character",
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
	url = sprintf(url, session@port, func, session@ssid, session@debug);
}
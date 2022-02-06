#Region "Microsoft.ROpen::4c05eb3546997fcb339ec5c26e886e0a, Rsharp_interop.R"

    # Summaries:

    # closureText = function(closure) {...

#End Region

#' Translate R code as R# code
#'
closureText = function(closure) {
	closure = gsub("%>%", ":>", capture.output(closure), fixed = TRUE);
	trim    = function (x) sub("\\s+$", "", x)

	if (closure[1] == "function() {") {
		# write by hand
		closure = closure[2:(length(closure) - 1)];
	} else if (gsub("\\s", "", closure[1], perl = TRUE) == "function()") {
		# formatted by R package compiler
		closure = closure[3:(length(closure) - 3)];
	}

	sapply(trim(closure), function(line) {
		if (endsWith(line, ":>") ||
			endsWith(line, "%do%") ||
			endsWith(line, "%dopar%") ||
			endsWith(line, "{") ||
			endsWith(line, ",")) {

			line;
		} else {
			sprintf("%s;", line);
		}
	}) %=>% as.vector;
}

decode_rjson = function(json) {
	require(jsonlite);

	if (is.character(json)) {
		json = jsonlite::fromJSON(json);
	}
}
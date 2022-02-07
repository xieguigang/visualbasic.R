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

	if (is.list(json)) {
		coerce_listNode(json); 
	} else {
		json;
	}
}

coerce_listNode = function(node) {
	if (is.list(node)) {
		node = lapply(node, function(child) {
			coerce_listNode(child); 
		});

		size_child = sapply(node, length);
		type_child = sapply(node, function(child) {
			if (is.matrix(child) || is.list(child) || is.data.frame(child)) {
				FALSE;
			} else {
				TRUE;
			}
		});
		all_equals = all((size_child == size_child[1]) | size_child == 1) && any(size_child > 1);
		all_primitive = all(type_child);

		if (length(node) > 1 && all_equals && all_primitive) {
			# convert to dataframe
			tab = data.frame(node);
			colnames(tab) = names(node);

			node = tab;
		} else {
			node;
		}
	} else {
		node;
	}
}
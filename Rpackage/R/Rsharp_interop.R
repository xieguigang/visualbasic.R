closureText = function(closure) {
	closure = gsub("%>%", ":>", capture.output(closure), fixed = TRUE);
	
	if (closure[1] == "function() {") {
		# write by hand
		closure = closure[2:(length(closure) - 1)];
	} else if (gsub("\\s", "", closure[1], perl = TRUE) == "function()") {
		# formatted by R package compiler
		closure = closure[3:(length(closure) - 3)];
	}
	
	closure;
}
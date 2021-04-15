closureText = function(closure) {
	closure = gsub("%>%", ":>", capture.output(closure), fixed = TRUE);
	
	if (closure[1] == "function() {") {
		closure = closure[2:(length(closure) - 1)];
	}
	
	closure;
}
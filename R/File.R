basename <- function(path) {

    file <- base::basename(path)
	tokens <- Strings.Split(file, "\.")
	tokens <- Take(tokens, length(tokens) - 1)
	file <- Strings.Join(tokens, ".")
	
	return(file)
}
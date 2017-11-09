basename <- function(path) {

    file <- base::basename(path)
	tokens <- Strings.Split(file, "\.")
	tokens <- Take(tokens, length(tokens) - 1)
	file <- Strings.Join(tokens, ".")
	
	return(file)
}

# 向目标指定的文件中追加指定的文本数据
Println <- function(file.txt, content) {

	cat(content, file = file.txt, append = TRUE);
	cat("\n",    file = file.txt, append = TRUE);
	
}

File.Open <- function(file.txt) {
	         function(content) Println(file.txt, content);
}
basename <- function(path) {

    file <- base::basename(path)
	tokens <- Strings.Split(file, "\\.")
	tokens <- Take(tokens, length(tokens) - 1)
	file <- Strings.Join(tokens, ".")
	
	return(file)
}

# 向目标指定的文件中追加指定的文本数据
Println <- function(file.txt, content) {

	cat(content, file = file.txt, append = TRUE);
	cat("\n",    file = file.txt, append = TRUE);
	
	return(invisible(NULL));
}

## 打开一个文件句柄，然后返回一个函数指针用来以``sprintf``格式化
## 的形式向文件以追加的形式写入数据
File.Open <- function(file.txt, append = FALSE) {
	
	try(dir.create(dirname(file.txt), recursive = TRUE));	
	
	if (!append) {
		cat(NULL, file = file.txt, append = FALSE);
	}
	
	# printf <- function(...) invisible(print(sprintf(...)));
	return(function(...) {		
		invisible(Println(file.txt, sprintf(...)));
	});
}

ReadAllText <- function(file.txt) {	
	conn  <- file(file.txt, open = "r");
	lines <- readLines(conn);	
	close(conn);		
	text <- paste0(lines, collapse = "\n");
	
	return(cat(text));
}
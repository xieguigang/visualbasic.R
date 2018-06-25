#' Get the file name of a given file path without extension name
basename <- function(path) {

    file <- base::basename(path)
	tokens <- Strings.Split(file, "\\.")
	tokens <- Take(tokens, length(tokens) - 1)
	file <- Strings.Join(tokens, ".")

	return(file)
}

#' Append text content to a specific text file.
#'
#' @param file.txt File path
#' @param content The text content that will be write to
#'                target text file.
#'
#' @return Nothing
Println <- function(file.txt, content) {

	cat(content, file = file.txt, append = TRUE);
	cat("\n",    file = file.txt, append = TRUE);

	return(invisible(NULL));
}

#' Open a text file for write in text mode
#'
#' @description 打开一个文件句柄，然后返回一个函数指针用来以\code{sprintf}格式化
#' 的形式向文件以追加的形式写入数据
#'
#' @param file.txt Target file path for write in text mode.
#' @param append A logical flag to indicate append the data to target file or not?
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

#' Read all text from a specific file
#'
#' @param file.txt The target file path for read
ReadAllText <- function(file.txt) {
	conn  <- file(file.txt, open = "r");
	lines <- readLines(conn);
	close(conn);
	text <- paste0(lines, collapse = "\n");

	return(cat(text));
}

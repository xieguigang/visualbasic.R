#Region "Microsoft.ROpen::0e986c08d999556e767d6db8093d9a55, File.R"

    # Summaries:

    # basename <- function(path) {...
    # File.ExtensionName <- function(path) {...
    # File.WithExtension <- function(path, ext) {...
    # Println <- function(file.txt, content) {...
    # File.Open <- function(file.txt, append = FALSE, format = TRUE) {...
    # ensure_dir_exists <- function(path) {if (!dir.exists(path)) {...
    # ReadAllText <- function(file.txt) {...
    # ReadAllLines <- function(file.txt) {...

#End Region

#' File name without extension name
#'
#' @description Get the file name of a given file path without extension name.
#'
#' @param path File path string.
#'
basename <- function(path) {
	Linq   <- Microsoft.VisualBasic.Data.Linq();

	sapply(path, function(file.path) {
	  file   <- base::basename(file.path);
	  tokens <- Strings.Split(file, "\\.");
	  tokens <- Linq$methods$Take(tokens, length(tokens) - 1);

	  Strings.Join(tokens, ".");
	}) %=>% as.character;
}

#' Get file extension name
File.ExtensionName <- function(path) {
	Linq   <- Microsoft.VisualBasic.Data.Linq();
    file   <- base::basename(path);
	tokens <- Strings.Split(file, "\\.");

	Linq$methods$Last(tokens);
}

#' Determine path end with a given extension name
#'
#' @description Case insensitive
File.WithExtension <- function(path, ext) {
	ext.parsed <- File.ExtensionName(path);
	tolower(ext.parsed) == tolower(ext);
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

#' Open a file for write in text mode
#'
#' @description Open a given file and returns the handle for write this file in text mode.
#'   The returned lambda function is a wrapper for function \code{link{sprintf}} and
#'   \code{cat} function.
#'
#' @param file.txt Target file path for write in text mode.
#' @param append A logical flag to indicate append the data to target file or not?
#'
#' @return A lambda function for write text data to file.
File.Open <- function(file.txt, append = FALSE, format = TRUE) {
  dir <- dirname(file.txt);

  if (dir != ".") {
    try(dir.create(dir, recursive = TRUE));
  }

	if (!append) {
		cat(NULL, file = file.txt, append = FALSE);
	}

	# printf <- function(...) invisible(print(sprintf(...)));
  if (format) {
    function(...) {
      invisible(Println(file.txt, sprintf(...)));
    };
  } else {
    function(str) {
      invisible(Println(file.txt, str));
    }
  }
}

#' Ensure the dir exists
ensure_dir_exists <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE);
  }
}

#' Read all text from a specific file
#'
#' @param file.txt The target file path for read
#'
#' @return Returns the text file content in one piece, not split in lines.
#'
ReadAllText <- function(file.txt) {
	paste0(file.txt %=>% ReadAllLines, collapse = "\n");
}

ReadAllLines <- function(file.txt) {
  conn  <- file(file.txt, open = "r");
  lines <- readLines(conn);

  conn %=>% close;

  lines;
}

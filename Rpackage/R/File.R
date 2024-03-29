#Region "Microsoft.ROpen::6639fde6420b6bbe528508c64158666c, File.R"

    # Summaries:

    # normalizeFileName = function(name) {...
    # basename <- function(path) {...
    # File.ExtensionName <- function(path) {...
    # File.WithExtension <- function(path, ext) {...
    # Println <- function(file.txt, content) {...
    # File.Open <- function(file.txt, append = FALSE, format = TRUE) {...
    # ensure_dir_exists <- function(path) {...
    # ReadAllText <- function(file.txt) {...
    # ReadAllLines <- function(file.txt) {...
    # FileReader <- function(path) {...
    # EmptyFile <- function(outfile) {...

#End Region

normalizeFileName = function(name) {
  name = gsub("*", "_", name, fixed = TRUE);
  name = gsub("|", "_", name, fixed = TRUE);
  name = gsub("/", "_", name, fixed = TRUE);
  name = gsub("\\", "_", name, fixed = TRUE);
  name = gsub(">", "_", name, fixed = TRUE);
  name = gsub("<", "_", name, fixed = TRUE);
  name = gsub(":", "_", name, fixed = TRUE);
  
  name;
}

#' File name without extension name
#'
#' @description Get the file name of a given file path without 
#'    extension name.
#'
#' @param path File path string vector.
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
#'
File.ExtensionName <- function(path) {
  Linq   <- Microsoft.VisualBasic.Data.Linq();
  file   <- base::basename(path);
  tokens <- Strings.Split(file, "\\.");

  Linq$methods$Last(tokens);
}

#' Determine path end with a given extension name
#'
#' @description Case insensitive
#'
#' @param ext File extension name without dot.
#'
File.WithExtension <- function(path, ext) {
  ext.parsed <- File.ExtensionName(path);
  tolower(ext.parsed) == tolower(ext);
}

#' Append text content to file
#'
#' @description Append text content to a specific text file.
#'
#' @param file.txt File path
#' @param content The text content that will be write to
#'                target text file.
#'                It can be a string lines vector.
#'
#' @return Nothing
Println <- function(file.txt, content) {

  # Add and extra newline after the content data
  content <- paste0(append(content, ""), collapse = "\n");
  cat(content,
      file = file.txt,
      append = TRUE
  );

  invisible(NULL);
}

#' Open a file for write in text mode
#'
#' @description Open a given file and returns the handle for 
#'   write this file in text mode. The returned lambda 
#'   function is a wrapper for function \code{link{sprintf}} 
#'   and \code{cat} function.
#'
#' @param file.txt Target file path for write in text mode.
#' @param append A logical flag to indicate append the data 
#'   to target file or not?
#'
#' @return A lambda function for write text data to file.
#' 
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
#'
#' @description If the directory path is not exists on
#' the file system, then this function will create it.
#'
ensure_dir_exists <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE);
  }
}

#' Read all text from a specific file
#'
#' @param file.txt The target file path for read
#'
#' @return Returns the text file content in one piece, 
#'    not split in lines.
#'
ReadAllText <- function(file.txt) {
  paste0(ReadAllLines(file.txt), collapse = "\n");
}

#' Read all text line
#'
#' @description Read all text lines from a specific text file.
#'
ReadAllLines <- function(file.txt) {
  using(FileReader(file.txt));
}

FileReader <- function(path) {
  p       <- file(path, open = "r");
  read    <- function() readLines(p);
  release <- function() close(p);

  list(invoke = read, dispose = release);
}

#' Create an empty file
#'
#' @description Create an empty file at specific location, if the target file
#'   is already exists, then all of the original contents of target file will
#'   be removed.
#'
EmptyFile <- function(outfile) {
  close( file( outfile, open="w" ) );
  invisible(NULL);
}

#Region "Microsoft.ROpen::cb3123c4c05dfdd59b19d4b88e5729ef, TextWriter.R"

    # Summaries:

    # textWriter <- function(path, buffer.Size = 4096) {...
    # addline <- function(line) {...
    # close <- function() {...
    # println = function(...) {...
    # tr <- function(file, saveAs = file) {...

#End Region

# Helper for write large text file in R language

#' Open a text writer
#'
#' @param path The file path string.
#' @param buffer.Size Buffer size in number of text lines. This
#'      parameter dramatically affect the writer performance.
#'
#' @return A text writer handler list object, with members:
#'
#' \enumerate{
#' \item \code{writeline} Write text line through this text writer helper.
#' \item \code{println} Write text line with \code{sprintf} format helper.
#' \item \code{close} Flush the remaining text data in buffer and
#'      then close the file connection and finally dispose the text writer buffer.
#' }
#'
textWriter <- function(path, buffer.Size = 4096) {

  # Using current environment as workspace
  workspace <- environment();
  flush     <- File.Open(path, FALSE, FALSE);
  assign("buffer", c(), envir = workspace);

  addline <- function(line) {
  	if (!base::exists("buffer", envir = workspace)) {
  		stop(sprintf("Text file '%s' is closed!", path));
  	} else {
  	  chunk <- append(get("buffer", envir = workspace), line);
  	}

    if (length(chunk) >= buffer.Size) {
      # write current data, and then clear the buffer
      chunk %=>% flush;
      chunk <- c();
    }

    assign("buffer", chunk, envir = workspace);
  }

  # Flush the remaining data in buffer and close file.
  close <- function() {
    chunk <- get("buffer", envir = workspace);
    chunk %=>% flush;

    rm(list = c("buffer"), envir = workspace);
    # rm(chunk);
    # rm(flush);
    gc();
	
	invisible(NULL);
  }

  list(
    writeline = addline,
    close = close,
    println = function(...) {
      addline(sprintf(...));
    });
}

#' Remove non-printable ASCII characters
#'
#' @description Remove non-printable ASCII characters from a file with this Unix command. 
#'  (https://alvinalexander.com/blog/post/linux-unix/how-remove-non-printable-ascii-characters-file-unix)
#'
#' @param file The file path of the target text file. 
#' @param saveAs The clean file save location, by default is the original location.
#'
#' @details This functin only works in unix system.
#'
tr <- function(file, saveAs = file) {
  out <- saveAs;
  
  if (file == saveAs) {
	# 20190701 if the saveas is the input file
	# this will required additional processing
	# or we just get empty blank output file....
	saveAs <- tempfile(pattern = "tr_temp");
  }
  
  cli <- 'tr -cd \'\\11\\12\\15\\40-\\176\' < "%s" > "%s"';
  cli <- sprintf(cli, file, saveAs);

  system(cli, intern = TRUE);
  
  if (file == out) {
	# copy back the result file
	cli <- sprintf('cat "%s" > "%s"', saveAs, file);
	system(cli, intern = TRUE);
  }
  
  invisible(NULL);
}

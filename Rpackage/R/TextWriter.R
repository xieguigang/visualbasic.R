#Region "Microsoft.ROpen::8269643c82c611044e96eebe0324b398, TextWriter.R"

    # Summaries:

    # textWriter <- function(path, buffer.Size = 4096) {...
    # addline <- function(line) {if (!base::exists("buffer", envir = workspace)) {...
    # close <- function() {...

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

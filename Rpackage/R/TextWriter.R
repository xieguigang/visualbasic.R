# Helper for write large text file in R language

#' Open a text writer
#'
#' @param path The file path string.
#' @param buffer.Size Buffer size in number of text lines.
#'
#' @return A text writer handler function.
#'
textWriter <- function(path, buffer.Size = 8192) {

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

    # rm(workspace);
    # rm(chunk);
    # rm(flush);
    # gc();
  }

  list(writeLine = addline, close = close);
}



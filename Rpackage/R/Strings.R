# Strings Helper function from Microsoft.VisualBasic.Strings namespace.

# Public Shared Function Split(Expression As String, Optional Delimiter As String =  , Optional Limit As Integer = -1, Optional Compare As Microsoft.VisualBasic.CompareMethod = 0) As String()
#     Member of Microsoft.VisualBasic.Strings

#' Split string using regexp
#'
#' @description Returns a zero-based, one-dimensional array containing a specified
#'              number of substrings.
#'
#' @param Expression Required. String expression containing substrings and delimiters.
#' @param Delimiter Optional. Any single character used to identify substring limits.
#'            If Delimiter is omitted, the space character (" ") is assumed to be
#'            the delimiter.
#' @param Limit Optional. Maximum number of substrings into which the input string
#'        should be split. The default, ?, indicates that the input string should
#'        be split at every occurrence of the Delimiter string.
#' @param Compare Optional. Numeric value indicating the comparison to use when
#'          evaluating substrings. See "Settings" for values.
#'
#' @return String array. If Expression is a zero-length string (""), Split returns a
#'        single-element array containing a zero-length string. If Delimiter is a
#'        zero-length string, or if it does not appear anywhere in Expression, Split
#'        returns a single-element array containing the entire Expression string.
Strings.Split <- function(Expression, Delimiter = " ", Compare = 0) {

	useBytes <- TRUE;

	if (Compare != 0) {
		useBytes <- FALSE;
	}

	out <- strsplit(x=Expression, split=Delimiter, fixed=FALSE, perl=TRUE, useBytes=useBytes);
	out <- as.vector(out[[1]]);

	out;
}

split <- function(expression, delimiter = " ", limit = -1, compare = 0) {
	Strings.Split(expression, delimiter, limit, compare);
}

# Public Shared Function Join(SourceArray As String(), Optional Delimiter As String =  ) As String
#     Member of Microsoft.VisualBasic.Strings

#' Contact string tokens
#'
#' @description Returns a string created by joining a number of substrings
#'              contained in an array.
#'
#' @param SourceArray Required. One-dimensional array containing substrings to be joined.
#' @param Delimiter Optional. Any string, used to separate the substrings in the
#'            returned string. If omitted, the space character (" ") is used. If
#'           Delimiter is a zero-length string ("") or Nothing, all items in the
#'            list are concatenated with no delimiters.
#'
#' @return Returns a string created by joining a number of substrings contained in an
#'      array.
Strings.Join <- function(SourceArray, Delimiter = " ") {
	paste0(SourceArray, collapse = Delimiter);
}

join <- function(sourceArray, delimiter) {
	Strings.Join(sourceArray, delimiter);
}

# Public Shared Function LCase(Value As String) As String
    # Member of Microsoft.VisualBasic.Strings

#' String to lowercase
#'
#' @description Returns a string or character converted to lowercase.
#'
#' @param Value Required. Any valid String or Char expression.
#'
#' @return Returns a string or character converted to lowercase.
Strings.LCase <- function(Value) {
	tolower(Value);
}

lcase <- function(value) {
	Strings.LCase(value);
}

# Public Shared Function Replace(Expression As String, Find As String, Replacement As String, Optional Start As Integer = 1, Optional Count As Integer = -1, Optional Compare As Microsoft.VisualBasic.CompareMethod = 0) As String
#     Member of Microsoft.VisualBasic.Strings

#' Text replacement
#'
#' @description Returns a string in which a specified substring has been
#'          replaced with another substring a specified number of times.
#'
#' @param Expression Required. String expression containing substring to replace.
#' @param Find Required. Substring being searched for.
#' @param Replacement Required. Replacement substring.
#' @param Start Optional. Position within Expression that starts a substring
#'     used for replacement. The return value of Replace is a string that begins
#'     at Start, with appropriate substitutions. If omitted, 1 is assumed.
#' @param Count Optional. Number of substring substitutions to perform.
#'     If omitted, the default value is –1, which means "make all possible
#'     substitutions."
#' @param Compare Optional. Numeric value indicating the kind of comparison
#'     to use when evaluating substrings. See Settings for values.
#'
#' @return Replace returns the following values.IfReplace returns Find
#'        is zero-length or NothingCopy of Expression Replace is zero-length
#'        Copy of Expression with no occurrences of Find Expression is zero-length
#'        or Nothing, or Start is greater than length of Expression Nothing Count is 0
#'        Copy of Expression
Strings.Replace <- function(Expression, Find, Replacement) {
	gsub(Find, Replacement, Expression, fixed = TRUE)
}

# Public Shared Function UCase(Value As String) As String
    # Member of Microsoft.VisualBasic.Strings

#' String in uppercase
#'
#' @description Returns a string or character containing the specified string
#'         converted to uppercase.
#'
#' @param Value Required. Any valid String or Char expression.
#'
#' @return Returns a string or character containing the specified string
#'         converted to uppercase.
Strings.UCase <- function(Value) {
	toupper(Value);
}

ucase <- function(value) {
	Strings.UCase(value);
}

Distinct <- function(words) {
	unique(tolower(words));
}

#' Locate substring position
#'
#' @return -1 means no match
InStr <- function(s, substring) {
	match <- regexpr(substring, s);
	match[1];
}

#' Substring
Mid <- function(s, start, length) {

}

#' A given string is empty?
#'
#' @param s A given string vector that using for measurement
#' @param NA.empty This logical flag indicate that should treat the text which is
#'       equals to \code{chr("NA")} as empty too? Default is not.
#'
#' @description Determine that target string is null or empty or not??
#' @aliases IsNothing
#'
#' @return Logical vector
Strings.Empty <- function(s, NA.empty = FALSE) {
	sapply(s, function(x) {
	  IsNothing(x, stringAsFactor = NA.empty);
	}) %=>% as.logical;
}

#' Text similarity score
name.similarity <- function(sa, sb) {
	l.max       <- max(nchar(c(sa, sb)));
	similarity  <- (l.max - levenshtein.distance(sa,sb)) / l.max;
	similarity;
}

#' Compute Levenshtein distance between two strings
#'
#' @param source      Source string.
#' @param target      Target string.
#' @param type        Specifies the return type. 'distance' for a single
#'                    distance value; 'matrix' for the matrix used during
#'                    dynamic programming.
#' @param insert.fun  delete.fun, substitute.fun: Penalty functions of insert,
#'                    delete and substitute operation, whose return value must
#'                    be a single scalar value.
#'
levenshtein.distance <- function(source, target,
	type           = c('distance','matrix'),
	insert.fun     = function(x) 1,
	delete.fun     = function(x) 1,
	substitute.fun = function(s,t) ifelse(s==t,0,1)) {

	type       <- match.arg(type);
	source.vec <- strsplit(source,'')[[1]];
	target.vec <- strsplit(target,'')[[1]];

	if(length(source.vec)==0 & length(target.vec)==0) return(0);
	if(length(source.vec)==0) return(sum(sapply(target.vec,insert.fun)));
	if(length(target.vec)==0) return(sum(sapply(source.vec,delete.fun)));

	ns <- length(source.vec) + 1
	nt <- length(target.vec) + 1

	d     <- matrix(0, nrow=ns, ncol=nt, dimnames=list(c('#',source.vec),c('#',target.vec)));
	d[,1] <- 0:(ns-1);
	d[1,] <- 0:(nt-1);

	for(j in 2:nt) {
		for(i in 2:ns) {
			d[i,j] <- min( d[i-1, j]   + delete.fun(source.vec[i-1]),
						   d[i,   j-1] + insert.fun(target.vec[j-1]),
						   d[i-1, j-1] + substitute.fun(source.vec[i-1], target.vec[j-1]) );
		}
	}

	switch(type,
		'distance' = d[ns,nt],
		'matrix'   = d);
}

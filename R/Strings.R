# Public Shared Function Split(Expression As String, Optional Delimiter As String =  , Optional Limit As Integer = -1, Optional Compare As Microsoft.VisualBasic.CompareMethod = 0) As String()
#     Member of Microsoft.VisualBasic.Strings

# Summary:
# Returns a zero-based, one-dimensional array containing a specified number of substrings.

# Parameters:
# Expression: Required. String expression containing substrings and delimiters.
# Delimiter: Optional. Any single character used to identify substring limits. If Delimiter is omitted, the space character (" ") is assumed to be the delimiter.
# Limit: Optional. Maximum number of substrings into which the input string should be split. The default, –1, indicates that the input string should be split at every occurrence of the Delimiter string.
# Compare: Optional. Numeric value indicating the comparison to use when evaluating substrings. See "Settings" for values.

# Returns:
# String array. If Expression is a zero-length string (""), Split returns a single-element array containing a zero-length string. If Delimiter is a zero-length string, or if it does not appear anywhere in Expression, Split returns a single-element array containing the entire Expression string.
Strings.Split <- function(Expression, Delimiter = " ", Compare = 0) {

	useBytes <- TRUE;
	
	if (Compare != 0) {
		useBytes <- FALSE;
	}
	
	return(strsplit(x=Expression, split=Delimiter, fixed = FALSE, perl=TRUE, useBytes = useBytes));
}

split <- function(expression, delimiter = " ", limit = -1, compare = 0) {
	return(Strings.Split(expression, delimiter, limit, compare));
}
#Region "Microsoft.ROpen::c148bcc6eec0752ed41b25b889a7531f, XML.R"

    # Summaries:

    # SaveXML <- function(x, file.xml, rootName = "Rlang.xml") {XML.Framework(  write = File.Open(file.txt = file.xml),  do.write = function(write) {...
    # XML.Framework <- function(write, do.write, rootName) {...
    # Xml.Write.Any <- function(x, indent, write, name = NULL) {if (is.data.frame(x) || is.matrix(x)) {...
    # Xml.Write.Vector <- function(vector, indent, write, name = NULL) {if (is.numeric(vector)) {...
    # Xml.Write.Matrix <- function(matrix, indent, write, node.name = NULL) {...
    # Xml.Write.List <- function(list, indent, write, node.name = NULL) {...

#End Region

#' Serialize any R object to a specific XML file
#'
#' @description Serialize any R object to a specific XML file, for save data in a more
#'   human readable format.
#'
#' @param x The R object in any type
#' @param file.xml The file path of the XML file dump data will be saved.
#' @param rootName The name of the generated xml root node.
#'
SaveXML <- function(x, file.xml, rootName = "Rlang.xml") {
  XML.Framework(
    write    = File.Open(file.txt = file.xml),
    do.write = function(write) {
      Xml.Write.Any(x, "", write);
    },
    rootName = rootName
  );
}

#' A framework for write R object to XML file.
#'
#' @param write The file write handle from the \code{\link{File.Open}} or \code{\link{textWriter}} function.
#'    It is recommended use \code{\link{textWriter}} function for better performance.
#' @param do.write A function pointer that used for describ how to build the output xml file
#' @param rootName The node name of the generated xml root node.
#'
XML.Framework <- function(write, do.write, rootName, xmlns = NULL) {
  xsd <- "http://www.w3.org/2001/XMLSchema";
  xsi <- "http://www.w3.org/2001/XMLSchema-instance";

  # write xml declare
  write("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
  # write xml root
  if (IsNothing(xmlns)) {
	  write("<%s xmlns:xsd=\"%s\" xmlns:xsi=\"%s\">", rootName, xsd, xsi);
  } else {
	  write("<%s xmlns=\"%s\" xmlns:xsd=\"%s\" xmlns:xsi=\"%s\">", rootName, xmlns, xsd, xsi);
  }
  
  # write xml body data
  do.write(write);
  # close xml root tag
  write("</%s>", rootName);

  invisible(NULL);
}

#' Write R object to Xml
#'
#' @description Choose different XML write function based on the input object
#'    type.
#'
#' @param x Type is limited to \code{vector}, \code{list} and
#'    \code{data.frame}/\code{matrix}.
#' @param indent The xml node indent, whitespace numbers.
#' @param write Text file write handle, which is created from \code{\link{File.Open}}
#' @param name Using this function parameter to overrides the node name.
#'
Xml.Write.Any <- function(x, indent, write, name = NULL) {

  if (is.data.frame(x) || is.matrix(x)) {

    Xml.Write.Matrix(x, indent, write, name);

  } else if (is.list(x)) {

    Xml.Write.List(x, indent, write, name);

  } else {
    if (length(x) == 1) {
      write('%s<%s value="%s" />', indent, name, x);
    } else {
      Xml.Write.Vector(x, indent, write, name);
    }
  }

  invisible(NULL);
}

#' Write the vector as the xml node
#'
#' @description Write the vector as the xml node:
#'
#'   If the vector type is characters, then this function will generates the
#'   resulted xml in a list format;\cr
#'
#'   If the vector type is numeric or logical, then the vector will be saved
#'   as xml attribute.
#'
#' @param vector All of the elements in this vector should be in the same mode.
#'    And elements type should also be the primitive types like: \code{numeric},
#'    \code{character} or \code{logical}, etc.
#'
#' @details
#'
#'   For numeric vector:
#'
#'   \code{<numeric vector="" />}
#'
#'   For logical vector:
#'
#'   \code{<logical vector="" />}
#'
#'   For character vector:
#'
#'   \code{
#'     <strings>\cr
#         <string></string>\cr
#      </strings>
#'   }
#'
Xml.Write.Vector <- function(vector, indent, write, name = NULL) {
  if (is.numeric(vector)) {
    if (is.null(name)) name = "numeric";

    line <- paste0(vector, collapse = " ");
    line <- sprintf("%s<%s vector=\"%s\" />", indent, name, line);
  } else if (is.logical(vector)) {
    if (is.null(name)) name = "logical";

    line <- paste0(vector, collapse = " ");
    line <- sprintf("%s<%s vector=\"%s\" />", indent, name, line);
  } else if (is.character(vector)) {
    if (is.null(name)) name = "strings";

    write(sprintf("%s<%s>", indent, name));
    for (line in vector) {
      write(sprintf("%s%s<string>%s</string>", indent, indent, line));
    }
    write(sprintf("%s</%s>", indent, name));

    return(0);
  } else {
    print(vector);
    stop(mode(vector));
  }

  write(line);
}

#' Write the matrix/dataframe as XML node
#'
#' @description Write the variable object of \code{matrix}/\code{data.frame}
#'   type as a node in XML document.
#'
#' @details Output the xml node in format like:
#'
#' \code{
#'
#'    <node.name nrows = ...>\cr
#'       <tr rowname = ...>\cr
#'          <td colname = >value</td>\cr
#'       </tr>\cr
#'       <tr>\cr
#'       </tr>\cr
#'    </node.name>
#'
#' }
#'
Xml.Write.Matrix <- function(matrix, indent, write, node.name = NULL) {

  colnames  <- colnames(matrix);
  rownames  <- rownames(matrix);
  .list     <- .as.list(matrix);
  node.name <- node.name %||% "table";

  write('%s<table name="%s" nrow="%s">', indent, node.name, nrow(matrix));

  for (i in 1:nrow(matrix)) {
    write('%s%s<tr rowname="%s">', indent, indent, rownames[i]);
    tr <- .list[[i]];

    for (name in colnames) {
      value <- tr[[name]];
      value <- sprintf('<td name="%s" value="%s" />', name, value);
      value <- sprintf("%s%s%s%s", indent, indent, indent, value);

      write(value);
    }
    write('%s%s</tr>', indent, indent);
  }

  write('%s</table>', indent);
}

#' Write the \code{list} as XML node
#'
#' @details A R list object is a kind of dictionary object in VB.NET
#'
Xml.Write.List <- function(list, indent, write, node.name = NULL) {

  name.list <- names(list);
  name.xml  <- names(list);

  if (is.null(name.list) || is.na(name.list)) {
    # Indexing by index numeric value, when without names
    name.list <- 1:length(list);
    name.xml  <- sprintf("node%s", name.list);
  }

  node.indent = indent;

  if (!is.null(node.name)) {
    write('%s<item name=\"%s\">', indent, node.name);
    node.indent = sprintf("%s%s", indent, indent);
  }

  for (i in 1:length(list)) {

    index <- name.list[i];
    name  <- name.xml[i];
    x     <- list[[index]];

    Xml.Write.Any(x, node.indent, write, name);
  }

  if (!is.null(node.name)) {
    write("%s</item>", indent);
  }
}

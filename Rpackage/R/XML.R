
#' Serialize any R object to a specific XML file, for save data in a more
#' human readable format.
#'
#' @param x The R object in any type
#' @param file.xml The file path of the XML file dump data will be saved.
#' @param rootName The name of the generated xml root node.
SaveXML <- function(x, file.xml, rootName = "Rlang.xml") {
	XML.Framework(
		write    = File.Open(file.txt = file.xml),
		do.write = function(write) {
			push.x(x, "", write);
		},
		rootName = rootName
	);
}

#' A framework for write R object to XML file.
#'
#' @param write The file write handle from the \code{\link{File.Open}} function
#' @param do.write A function pointer that used for describ how to build the output xml file
#' @param rootName The node name of the generated xml root node.
XML.Framework <- function(write, do.write, rootName) {
    write("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
    write("<%s xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">", rootName);
    do.write(write);
    write("</%s>", rootName);
}

#' Choose different XML write function based on the input object type.
push.x <- function(x, indent, write, name = NULL) {

	if (is.data.frame(x) || is.matrix(x)) {

		# 使用表格的形式写入数据
		Matrix.XML(x, indent, write, name);

	} else if (is.list(x)) {

        # 是一个类似于字典对象的东西
        List.XML(x, indent, write, name);

    } else {

        # 是相同元素的vector
		if (length(x) == 1) {
			write('%s<%s value="%s" />', indent, name, x);
		} else {
			Vector.XML(x, indent, write, name);
		}
    }
}

#' Write the vector as the xml node, If the vector type is characters, then
#' this function will generates the resulted xml in a list format; If the vector
#' type is numeric or logical, then the vector will be saved as xml attribute
Vector.XML <- function(vector, indent, write, name = NULL) {
    # 现在假设向量里面的元素都是基本的元素

    # <numeric vector="" />
    # <logical vector="" />
    # <strings>
    #     <string></string>
    # </strings>

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

#' Write the variable object of \code{matrix}/\code{data.frame} type
#' as a node in XML document.
Matrix.XML <- function(matrix, indent, write, node.name = NULL) {

	# <node.name nrows = ...>
	#     <tr rowname = ...>
	#         <td colname = >value</td>
	#     </tr>
	#     <tr>
	#     </tr>
	# </node.name>

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

List.XML <- function(list, indent, write, node.name = NULL) {

    name.list <- names(list);
    name.xml  <- names(list);

    if (is.null(name.list) || is.na(name.list)) {
        # 只有数字来进行索引，没有名称
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

        push.x(x, node.indent, write, name);
    }

    if (!is.null(node.name)) {
        write("%s</item>", indent);
    }
}
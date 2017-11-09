# 将任意的R对象序列化为XML文件保存

SaveXML <- function(x, file.xml, root = "R-xml") {
    
    write <- File.Open(file.txt = file.xml);
    write("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
    write(sprintf("<%s>", root));
    push.x(x, "", write);
    write(sprintf("</%s>", root));
}

push.x <- function(x, indent, write, name = "NULL") {

    if (is.list(x)) {

        # 是一个类似于字典对象的东西
        List.XML(x, sprintf("  %s", indent), write);

    } else {

        # 是相同元素的vector
        Vector.XML(x, sprintf("  %s", indent), write);
    }
}

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
        write(sprintf("%s</name>", indent, name));

        return(0);
    } else {
        print(vector);
        stop(mode(vector));
    }

    write(line);
}

List.XML <- function(list, indent, write) {
    name.list <- names(list);
    name.xml  <- names(list);

    if (is.null(name.list) || is.na(name.list)) {
        # 只有数字来进行索引，没有名称
        name.list <- 1:length(list);
        name.xml  <- sprintf("node%s", name.list);
    }

    for (i in 1:length(list)) {

        index <- name.list[i];
        name  <- name.xml[i];
        x     <- list[[index]];
        
        push.x(x, indent, write, name);
    }
}
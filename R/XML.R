# 将任意的R对象序列化为XML文件保存

SaveXML <- function(x, file.xml, root = "R-xml") {
    
    write <- File.Open(file.txt = file.xml);
    write("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
    write(sprintf("<%s>", root));

    if (is.list(x)) {

        # 是一个类似于字典对象的东西

    } else {

        # 是相同元素的vector
        Vector.XML(x, "  ", write);
    }

    write(sprintf("</%s>", root));
}

Vector.XML <- function(vector, indent, write) {
    # 现在假设向量里面的元素都是基本的元素

    # <numeric vector="" />
    # <logical vector="" />
    # <strings>
    #     <string></string>
    # </strings>

    if (is.numeric(vector)) {
        line <- paste0(vector, collapse = " ");
        line <- sprintf("%s<numeric vector=\"%s\" />", indent, line);
    } else if (is.logical(vector)) {
        line <- paste0(vector, collapse = " ");
        line <- sprintf("%s<logical vector=\"%s\" />", indent, line);
    } else if (is.character(vector)) {

        write(sprintf("%s<strings>", indent));
        for (line in vector) {
            write(sprintf("%s%s<string>%s</string>", indent, indent, line));
        }
        write(sprintf("%s</strings>", indent));

        return(0);
    } else {
        print(vector);
        stop(mode(vector));
    }

    write(line);
}

List.XML <- function(vector, indent, write) {
    
}
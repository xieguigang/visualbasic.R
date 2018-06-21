#' 对一个已经进行排序的序列进行二分法查找
#'
#' @param source 必须要对查找的键进行了升序排序操作
#' @param find 查找的目标key
#' @param compares find待查找目标键key和source之中的对象的key的比较方式，默认是按照数值大小进行比较
#' @param key 用于描述如何从source之中的一个元素得到进行比较的key的lambda表达式方法
binarySearch <- function(source, find, key, compares = function(a, b) a - b) {
    type <- GetType(source);

    if (type == primitiveTypes()$data.frame) {
        binarySearch.dataframe(source, find, key, compares);
    } else if (type == primitiveTypes()$list) {
        binarySearch.list(source, find, key, compares);
    } else {
        stop("Not Supported!");
    }
}

binarySearch.list <- function(list, find, key, compares = function(a, b) a - b) {
    i <- .binarySearch.impl.generic(
        function(i) key(list[[i]]),
        length(list),
        find,
        compares
    );

    if (i > -1) {
        list[[i]];
    } else {
        NULL;
    }
}

#' 这个查找函数返回序列的下标i
.binarySearch.impl.generic <- function(ikey, .length, find, compares) {
    L <- 1;
    R <- .length;
    i <- -1;

    while(L <= R) {
        m <- floor((L + R) / 2);
        c <- compares(ikey(m), find);

        if (c < 0) {
            L <- m + 1;
        } else if (c > 0) {
            R <- m - 1;
        } else {
            i <- m;
            break;
        }
    }

    i;
}

binarySearch.dataframe <- function(dataframe, find, key, compares = function(a, b) a - b) {
    # 获取得到索引列，这个索引列应该是进行了升序排序了的
    key <- as.vector(dataframe[, key]);
    i   <- .binarySearch.impl.generic(function(i) key[i], length(key), find, compares);

    if (i > -1) {
        dataframe[i, ];
    } else {
        NULL;
    }
}

#' 我们假设在这里的list是里面的所有的元素都是list对象，并且元素的名称都相同
#'
#' @param list The input list object
#' @param key A lambda function or property name string for get key value for the sort operation.
#' @param key.numeric A lambda function for evaluate the key value to numeric value
#' @param desc A logical flag to indicated that sort the input sequence in ASC or DESC mode?
#'
#' @return A data sequence which its elements has been reordered.
sort.list <- function(list, key, key.numeric = function(v) as.numeric(v), desc = FALSE) {
    if (!is.function(key)) {
        getkey <- function(x) x[[key]];
    } else {
		getkey <- key;
	}

    listnames <- names(list);
    keys  <- sapply(listnames, function(name) {
        x <- list[[name]];
        key.numeric(getkey(x));
    });
    orders <- order(as.numeric(keys), decreasing = desc);
    listnames   <- listnames[orders];
    list        <- list[orders];
    names(list) <- listnames;

    list;
}

#' 对数据框按照指定的列进行排序
#'
#' @param key 数据框之中的列名称或者列的索引编号
#' @param key.numeric 这个lambda函数描述了如何将所指定的列的值转换为用来进行排序所需要的数值依据的过程
#' @param desc 是否执行降序排序？默认是升序排序
sort.dataframe <- function(dataframe, key, key.numeric = function(v) as.numeric(v), desc = FALSE) {
    dataframe[order(key.numeric(dataframe[, key]), decreasing = desc), ];
}

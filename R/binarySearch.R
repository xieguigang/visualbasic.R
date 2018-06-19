## 对一个已经进行排序的序列进行二分法查找
##
## source 必须要对查找的键进行了升序排序操作
## find 查找的目标key
## compares find待查找目标键key和source之中的对象的key的比较方式，默认是按照数值大小进行比较
## key 用于描述如何从source之中的一个元素得到进行比较的key的lambda表达式方法
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
        compares);

    if (i > -1) {
        list[[i]];
    } else {
        NULL;
    }
} 

# 这个查找函数返回序列的下标i
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

sort.list <- function(list, key, compares = function(a, b) a - b) {

}

sort.dataframe <- function(dataframe, key, compares = function(a, b) a - b) {
    
}
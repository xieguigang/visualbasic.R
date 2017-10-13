`%=>%` <- function(x, y) { 
	function(...) y(x, ...);   
}


Take <- function(enumerable, m) {
	enumerable[1:m];
}

Skip <- function(enumerable, m) {
	enumerable[m:length(enumerable)];
}

# group data.frame/list by keys
#
# @param enumerable The data source collection for the group operation, by default is ``data.frame`` type.
# @param key The column name or property name for using its corresponding value as the group key
# @param type Indicate that the data source ``enumerable`` is a data.frame or list? default is a data.frame.
GroupBy <- function(enumerable, key, type = c("data.frame", "list")) {

    if (type == "data.frame") {
        GroupBy.dataframe(data.frame = enumerable, key = key);
    } else if (type == "list") {
        GroupBy.list(list = enumerable, key = key);
    } else {
        stop("Not supported!");
    }

}

# Groups the items in a list by s specific property of the list item.
#
# @param list The data source in list type
# @param key The item property name in this list source collection.
GroupBy.list <- function(list, key) {
    
    groups <- list();

    for (i in 1:length(list)) {
        item      <- list[[i]];
        group.key <- item[[key]];
        key.list  <- groups[[group.key]];
        
        if (is.null(key.list)) {
            key.list <- list();
        }

        key.list[[length(key.list) + 1]] <- item;
        groups[[group.key]] <- key.list;
    }

    return(groups);
}

# Groups the rows in a dataframe by a specific column as key.
#
# @param data.frame The data source in data.frame type
# @param key The column name for read the column data in target source as the group key.
GroupBy.dataframe <- function(data.frame, key) {
    
    groups      <- list();
    key.index   <- which(colnames(data.frame) == key);

    for (i in 1:nrow(data.frame)) {
        row <- data.frame[i, ];
        key <- row[key.index];
        groups[key] <- rbind(group[[key]], row);
    }

    return(groups);
}
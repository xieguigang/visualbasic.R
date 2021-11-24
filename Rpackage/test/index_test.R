#Region "Microsoft.ROpen::d3c4099c1988a4f856a2b8e05790ae58, test\index_test.R"

    # Summaries:


#End Region

i <- sprintf("T%s", 1: 30000);

index <- list();

# 查找效率比较高，但是前面这里构建list的时候会比较慢
for (x in i) {
	index[[x]] <- 1;
}


x <- "T3000";

test <- benchmark();

for(j in 1:300000) {
	dd <- x %in% i;
}

print(test());

test <- benchmark();

for (j in 1:300000) {
	dd <- !is.null(index[[x]])
}

print(test());


index <- as.index(i);

test <- benchmark();

for (j in 1:300000) {
	index(x);
}

print(test());

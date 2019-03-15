times <- 1:8000000;

x <- 9;

xx <- list(x = 9);


system.time({

sapply(times, function(i) x + 10);

})

system.time({

sapply(times, function(i) xx$x + 10);
})



alloca <- function(len, list = FALSE) {
	if (list) {
		lapply(1:len, function(i) NA);
	} else {
		rep(NA, len = len);
	}
}





# 用户 系统 流逝 
# 9.47 0.00 9.47

# 对向量数据，预先进行内存分配会更加有执行效率

system.time({
# 预分配
memory <- alloca(8000000);

	for(i in times) {
		memory[i] <- i + 1;
	}

})

# 下面没有内存预分配的时候花费了很长的时间

system.time({

memory <- c();

for ( i in times) {
memory[i] <- i + 1;
	
}

})
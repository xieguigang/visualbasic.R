d <- read.csv("./identify.csv");

tolerance <- function(a, b) {
	abs.d <- abs(a-b);	
	
	if (length(a) > 1) {
		sapply(1:length(abs.d), function(i) {
			d <- abs.d[i];
					
			if (d <= 0.3) {
				0;
			} else if (a[i] < b) {
				-1;
			} else {
				1;
			}
		});
	} else {
		if (abs.d <= 0.3) {
			0;
		} else if (a < b) {
			-1;
		} else {
			1;
		}
	}
}

binarySearch(d, 516, "mz", tolerance);

# 二叉树查找在小规模的数据集上面性能区分不大
# 但是在大型数据集上面非常有效

# 用户 系统 流逝 
# 0.02 0.00 0.02

benchmark.b <- function() {
	system.time({
		x <- binarySearch(d, 516, "mz", tolerance);	
		
		print("Search by binary search");
		print(x);
	});
}

# 用户 系统 流逝 
# 0.02 0.00 0.01

benchmark.n <- function() {
	system.time({
		x <- which(tolerance(as.vector(d[, "mz"]), 516) == 0);
		
		print("Search by which");
		print(d[x, ]);
	});
}
seq <- runif(100000, min=0, max=100);
assert <- function(x , y) abs(x - y) <= 0.3;

print( system.time({
	
print(	numeric.group(seq, assert)[[1]]);
	
}));


#' @param assert function(x,y) return logical
mz.grouping <- function(mz, assert) {

	# 首先进行这个unique碎片的mz的合并操作
	mz.unique   <- unique(mz);
	mz.groupKey <- list();

	# 按照0.5da获取得到二级碎片mz分组
	for (i in 1:length(mz.unique)) {
		mz      <- mz.unique[i];
		members <- NULL;
		
		# 被取出来的mz碎片都被设置为-1了
		if (mz != -1) {
			# 当前的这个mz也是这个分组的一个成员
			members <- append(members, mz);
		
			for (j in 1:length(mz.unique)) {
				if (assert(mz.unique[j], mz)) {
					members <- append(members, mz.unique[j]);
					mz.unique[j] = -1; #防止被重复吸收
				}
			}
			
			mz.groupKey[[as.character(mz)]] <- members;
		}
	}
			
	mz.groupKey;
}

print(system.time({
	
print(	mz.grouping(seq, assert)[[1]]);
	
}));


print( system.time({
	
print(	numeric.group(seq, assert)[[1]]);
	
}));
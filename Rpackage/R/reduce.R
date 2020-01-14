reduce <- function(x, aggregate = function(a, b) a + b, init.val = 0) {
	aggregate.val <- init.val;
	results <- x;
	
	for(i in 1:length(x)) {
		aggregate.val <- aggregate(aggregate.val, x[i]);
		results[i] <- aggregate.val;
	}
	
	results;
}
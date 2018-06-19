d <- read.csv("./identify.csv");

binarySearch(d, 516, "mz", function(a, b) {
	if (abs(a-b) <= 0.3) {
		0;
	} else if ( a < b) {
		-1;
	} else {
		1;
	}
});
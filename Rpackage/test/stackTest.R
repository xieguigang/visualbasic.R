test.function <- function() {
	GetCurrentFunc();
}

print(test.function());

# not working for closure

s <- sapply(1: 10, function(x) {
	sprintf("%s: %s", x, GetCurrentFunc());
});


s <- sapply(1: 10, function(x) {
	GetCurrentFunc();
});
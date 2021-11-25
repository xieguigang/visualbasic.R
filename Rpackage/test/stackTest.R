#Region "Microsoft.ROpen::27b19f8aa10323fce9b5a7a3d9a00d45, test\stackTest.R"

    # Summaries:

    # test.function <- function() {...

#End Region

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

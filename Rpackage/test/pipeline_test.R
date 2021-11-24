#Region "Microsoft.ROpen::c5aabb2d636a53bcac7c4af83044b0ce, test\pipeline_test.R"

    # Summaries:

    # `>` <- function(x, y) {...

#End Region

# "test.txt" %=>% ReadAllText %=>% File.Open("file_copy.txt"); 


`>` <- function(x, y) {
	if (is.function(y)) {
		y(x)
	} else {
		sign( x - y) == 1;		
	}
}

x = 999999;

x 
> is.na
> is.null
> print




system.time({

for(i in 1:1000000) {
 i > c(999,24,2,342,34,23,423,432,442,32342,3,4);
 
}


})

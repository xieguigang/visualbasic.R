#Region "Microsoft.ROpen::dc9ad44044bd07649baa4ed117d9c0f6, test\pipTest.R"

    # Summaries:

    # slave <- function(closure) {...
    # slave_closure <- function(closure) {...
    # dd <- function() {...

#End Region

#!/usr/bin/Rscript 

print(readLines(file("stdin")));

# echo 'hello world2' | R -q --no-restore --no-save --slave -e "print(readLines(file('stdin')));"


x <- cat(function() {
	
	n <- 2;
	gg <- n ^ 3;

	print(gg);
	
});


slave <- function(closure) {
	closure   <- capture.output(closure);
	slave_cli <- "R -q --no-restore --no-save --slave -e \"print(readLines(file('stdin')));\"";
}

slave_closure <- function(closure) {
	eval(parse(text = closure))()
}


dd <- function() { function(){ "<dsffsdfsfs>"} }

capture.output(dd()) %=>% stripREnvironmentInfo

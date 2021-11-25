#Region "Microsoft.ROpen::518385d8e727d1c9703c63dea5495764, test\tickTest.R"

    # Summaries:


#End Region

task <- 1:1000000;


system.time({

x <- 0;

for(i in task) {

    x = x + i;
}


})


system.time({

tick <- tick.helper(length(task), callback = function(p) warning(p));
cat("\n");
	cat("  Progress%: ");

x <- 0;

for(i in task) {

    x = x + i;
    tick();
}

	cat("\n");
	cat("\n");
})

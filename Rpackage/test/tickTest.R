task <- 1:100000;


system.time({

x <- 0;

for(i in task) {

    x = x + i;
}


})


system.time({

tick <- tick.helper(length(task));
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
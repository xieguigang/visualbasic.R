times <- 1:8000000;

x <- 9;

xx <- list(x = 9);


system.time({

sapply(times, function(i) x + 10);

})

system.time({

sapply(times, function(i) xx$x + 10);
})
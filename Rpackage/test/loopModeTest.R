#Region "Microsoft.ROpen::34cdaf824deb6743d6516d73115ce1fb, test\loopModeTest.R"

    # Summaries:

    # tick.each_lapply <- function(sequence, action) {...
    # tick.each_forloop <- function(sequence, action) {...

#End Region

require(VisualBasic.R)

tick.each_lapply <- function(sequence, action) {
  tick <- tick.helper(length(sequence));
  i <- 1;

  cat("\n");
  cat("  Progress%: ");

  out <- lapply(1:length(sequence), function(i) {
    tick();
    action(sequence[i]);
  });

  cat("\n");
  cat("\n");

  out;
}

tick.each_forloop <- function(sequence, action) {
  out  <- list();
  tick <- tick.helper(length(sequence));
  i <- 1;

  cat("\n");
  cat("  Progress%: ");

  for (x in sequence) {
    out[[i]] <- action(x);
    i = i + 1;
    tick();
  }

  cat("\n");
  cat("\n");

  out;
}

task <- 1:900000;

# 用户 系统 流逝 
# 5.91 0.03 5.95

system.time({
a <- tick.each_lapply(task, function(x) x+5);
})



system.time({
b <- tick.each_forloop(task, function(x) x+5);
})

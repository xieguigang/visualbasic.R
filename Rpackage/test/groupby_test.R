require(VisualBasic.R)

Imports("Microsoft.VisualBasic.Data.Linq");

data <- read.csv("./test_large.csv");

key <- "calibration.check"
gd <- GroupBy(data, key, type = "data.frame");

for(level in names(gd)) {

	write.csv(gd[[level]], file = sprintf("./%s.csv", level));
}
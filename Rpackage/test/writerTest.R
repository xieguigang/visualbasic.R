require(VisualBasic.R)

test <- "./test_large.csv" %=>% ReadAllLines

print(head(test))

# normal test

system.time({
	
	write <- "./test.tmp" %=>% File.Open;
	
	for(line in test) {
		write(line);
	}
	
});

# buffer test

system.time({
	
	write <- "./test2.tmp" %=>% textWriter;
	
	for(line in test) {
		write$writeLine(line);
	}
	
	write$close()
	
});
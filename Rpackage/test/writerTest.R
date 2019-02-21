require(VisualBasic.R)

test <- "./test_large.csv" %=>% ReadAllLines

print(head(test))

# normal test

 # 用户  系统  流逝 
# 19.56 10.25 32.76

system.time({
	
	write <- "./test.tmp" %=>% File.Open;
	
	for(line in test) {
		write(line);
	}
	
});

# buffer test

# 用户 系统 流逝 
# 3.02 0.10 3.39

system.time({
	
	write <- "./test2.tmp" %=>% textWriter;
	
	for(line in test) {
		write$writeLine(line);
	}
	
	write$close()
	
});
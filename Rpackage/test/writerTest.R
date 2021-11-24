#Region "Microsoft.ROpen::5555d9c2afa5ae1d49c81c729f1324d4, test\writerTest.R"

    # Summaries:


#End Region

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
	
	write <- textWriter("./test2.tmp", 8192);
	
	for(line in test) {
		write$writeline(line);
	}
	
	write$close();
	# will throw exception after file have been closed
	# write$writeline("");
});


system.time({
	
	write <- textWriter("./test2.tmp", 1024);
	
	for(line in test) {
		write$writeline(line);
	}
	
	write$close();
	# will throw exception after file have been closed
	# write$writeline("");
});


# buffer test 2
# 用户 系统 流逝 
# 3.83 0.04 4.17 

system.time({
	
	write <- "./test3.tmp" %=>% textWriter;
	
	for(line in test) {
		# sprintf 会稍微对性能产生一些影响
		write$println(line);
	}
	
	write$close();
});

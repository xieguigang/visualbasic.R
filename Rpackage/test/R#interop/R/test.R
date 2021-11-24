#Region "Microsoft.ROpen::d0f405fd3be31285f9bf8de3b2bbd2da, test\R#interop\R\test.R"

    # Summaries:

    # NPSearch = function() {...
    # RsharpText = function() {...
    # NPSearch = function() {...

#End Region

NPSearch = function() {
	require(test);
		
	`${dirname(@script)}/file.txt` %>% func_1(X) %>% func_2(Y);
	write.csv(table, file = `abc.csv`);
}

RsharpText = function() {
NPSearch = function() {
	require(test);
		
	`${dirname(@script)}/file.txt` %>% func_1(X) %>% func_2(Y);
	write.csv(table, file = `abc.csv`);
}

	print(VisualBasic.R::closureText(NPSearch));
}

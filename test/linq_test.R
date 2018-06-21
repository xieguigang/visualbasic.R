require(VisualBasic.R)

imports("microsoft.visualbasic.data.linq")

l <- list();
l[["A"]] <- list(a1 = "A", x = 90000099, d = 5678);
l[["B"]] <- list(a1 = "B", x = 19);
l[["C"]] <- list(a1 = "C", x = -98999, c = TRUE);
l[["D"]] <- list(a1 = "D", x = -999);
l[["E"]] <- list(x = 1999, a1 = "E" );
l[["F"]] <- list(a1 = "F", x = 934499, c = FALSE);
l[["G"]] <- list(a1 = "G", x = 4349);
l[["H"]] <- list(x = 99, a1 = "H");


from(l)$where(function(x) x$x > 0)
	   $select(function(x) x$a1)
	   $toarray();
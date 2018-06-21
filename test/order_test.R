l[["A"]] <- list(a1 = "A", x = 90000099);
l[["B"]] <- list(a1 = "B", x = 19);
l[["C"]] <- list(a1 = "C", x = -98999);
l[["D"]] <- list(a1 = "D", x = -999);
l[["E"]] <- list(a1 = "E", x = 1999);
l[["F"]] <- list(a1 = "F", x = 934499);
l[["G"]] <- list(a1 = "G", x = 4349);
l[["H"]] <- list(a1 = "H", x = 99);

print(l)
print(sort.list(l, "x"));


d <- as.data.frame(l);

print(d)
print(sort.dataframe(d, "x", desc = TRUE));

as.dataframe <-
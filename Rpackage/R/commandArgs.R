commandArgs = function(...) {
    argv = list(...);
    cmdl = base::commandArgs();

    if (length(argv) == 0) {
        return(cmdl);
    }

    str(argv);
}
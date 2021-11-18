commandArgs = function(..., debug = FALSE) {
    argv = list(...);
    cmdl = VisualBasic.R::argv();

    if (length(debug) > 0 && !is.logical(debug)) {
        # is a string array?
        cmdl  = .load_argv(cli = debug);
        debug = TRUE;
    }

    if (length(argv) == 0) {
        return(cmdl);
    } else if (debug) {
        print("View of the input commandline arguments:");
        str(cmdl);
        print("commandline schema:");
        str(argv);
    }

    if (cmdl$hasArg("--help")) {
        return(invisible(cmdl_helpDoc(argv)));
    }

    data = list();

    # validation
    # and get result string
    for(argName in names(argv)) {
        schema = argv[[argName]];
        required = as.logical(schema[["required"]]);

        if (!cmdl$hasArg(argName)) {
            if (required) {
                stop(schema[2]);
            } else {
                value = as.vector(unlist(schema[2]));
            }
        } else {
            value = cmdl$nextToken(argName);
        }

        # string interpolation
        value = cmdl_interpolate(value, cmdl);
        argName = gsub("(^[/-]+)|([/-]+$)", "", argName);
        argName = gsub("[-]+", ".", argName);
        data[[argName]] = value;
    }

    if (debug) {
        cat("\n");
        cat("-------------------end of debug echo--------------------");
        cat("\n\n\n");
    }

    data;
}

cmdl_helpDoc = function(argv) {
    stop("not implemented!");
}

cmdl_interpolate = function(value, cmdl) {
    value;
}
#Region "Microsoft.ROpen::6f96e30e0d0ca3c9e330e97dff72c7bc, commandArgs.R"

# Summaries:

# commandArgs = function(..., debug = FALSE) {...
# cmdl_helpDoc = function(argv) {...
# cmdl_interpolate = function(value, cmdl, allNames) {...

#End Region

#' The commandline helper for Rscript
#'
#' @param ... a list of commandline arguments for handle
#'    in this commandline helper. the element data in this
#'    list should be a list of key-value pair in formats
#'    like: \code{argument_name = description}. the
#'    \code{description} value shoud be a vector data in
#'    special data format.
#'
#' @param debug commandline helper debug options. value can
#'    be a logical switch or a list of debug data for assign
#'    to test of the commandline helper function.
#'
#' @return A list of commandline argument values in key-value
#'    pair data format. the commandline argument name has been
#'    trimmed of its prefix characters.
#'
#' @details the commandline argument value should be in format
#'    like \code{[required=TRUE/FALSE, default_value/error_message, description_text]}.
#'
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
    allNames = names(argv);

    # validation
    # and get result string
    for(argName in allNames) {
        schema   = argv[[argName]];
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
        value   = cmdl_interpolate(value, cmdl, allNames);
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

#' Build commandline help document
#'
#' @param argv the commandline description of the parameter
#'   from function \code{\link{commandArgs}}.
#'
cmdl_helpDoc = function(argv) {
    arguments = names(argv);
    optional  = sapply(argv, function(t) t[["required"]]);
    default   = sapply(argv, function(t) {
        if (t[["required"]]) {
            sprintf("error(%s)", t[2]);
        } else {
            t[2]
        }
    });
    description = sapply(argv, function(t) t[3]);

    print(data.frame(
        argument    = arguments,
        optional    = optional,
        default     = default,
        description = description
    ));
}

cmdl_interpolate = function(value, cmdl, allNames) {
    for(name in allNames) {
        str = cmdl$nextToken(name);

        if (IsNothing(str)) {
            str = "";
        }

        flag = sprintf("${%s}", name);
        value = gsub(flag, str, value, fixed = TRUE);
    }

    value;
}

#Region "Microsoft.ROpen::5a32a532b0db8ce78286f25ad708df75, test\commandlineTest.R"

    # Summaries:

    # argv2 <- function() {...
    # is.argName <- function(x) {...
    # getNextToken <- function(flag) {...

#End Region

require(VisualBasic.R);

argv2 <- function() {

  args <- commandArgs();

  # get all of the tokens after --args flag
  i <- which(args == "--args");

  if (length(i) == 0) {
    # No additional arguments
    cli <- c();
  } else {
    i <- i[1] + 2;
    cli <- args[i:length(args)];
  }

  name <- cli[1];
  args <- list();
  i    <- 2;
  is.argName <- function(x) {
    if (x %=>% IsNothing) {
      return (FALSE);
    }

    base::startsWith(x, "/")  ||
    base::startsWith(x, "--") ||
    base::startsWith(x, "-");
  }

  while(i < length(cli)) {
    argName = cli[i];

    if (cli[i + 1] %=>% is.argName) {
      # If the next element is the command argument name
      # then the current element is a logical flag
      args[argName] = TRUE;
      offset <- 1;
    } else {
      args[argName] = cli[i + 1];
      offset <- 2;
    }

    i = i + offset;
  }

  getNextToken <- function(flag) {
    args[which(args == flag) + 1];
  }

  list(argv = cli,
       commandName = name,
       args = args,
       nextToken = getNextToken
  );
}


print(argv2());

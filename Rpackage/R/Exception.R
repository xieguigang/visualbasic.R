#Region "Microsoft.ROpen::5e1c29cbeaa70a0673362f0a016c9134, D:\visualbasic.R\Rpackage\R"

    # Summaries:

    # GetCurrentFunc <- function() {

#End Region

#' Get the name of current function
GetCurrentFunc <- function() {
    stacks <- sys.calls();
    name   <- stacks[[length(stacks) - 1]];    
    name   <- Strings.Split(toString(name), "\\(")[1];
    name;
}

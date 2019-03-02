Imports Microsoft.VisualBasic.CommandLine
Imports Microsoft.VisualBasic.CommandLine.Reflection

Module Program

    Public Function Main() As Integer
        Return GetType(Program).RunCLI(App.CommandLine)
    End Function

    <ExportAPI("/html")>
    <Usage("/html /in <*.rd> [/out <*.html>]")>
    Public Function ParseHtml(args As CommandLine) As Integer
        Dim in$ = args <= "/in"
        Dim out$ = args("/out") Or [in].ChangeSuffix("html")
        Dim docs As RDoc = DocParser.ParseDoc([in])

        Return docs _
            .GetHtmlDoc _
            .SaveTo(out) _
            .CLICode
    End Function
End Module

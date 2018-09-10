Imports System.ComponentModel
Imports System.IO
Imports System.Runtime.CompilerServices
Imports Microsoft.VisualBasic.CommandLine
Imports Microsoft.VisualBasic.CommandLine.Reflection
Imports Microsoft.VisualBasic.Language
Imports Microsoft.VisualBasic.Language.UnixBash
Imports Microsoft.VisualBasic.Text

Module Program

    <Extension>
    Private Function versioning(ver%(), Optional i% = -1) As Integer()
        If i = -1 Then
            i = ver.Length - 1
        End If

        If i > 0 AndAlso ver(i) = 999 Then
            ver(i) = 0
            ver = versioning(ver, i - 1)
        Else
            ver(i) += 1
        End If

        Return ver
    End Function

    Private Function Versioning(args As CommandLine) As Integer
        Dim template$ = App.CommandLine.Name
        Dim version As Value(Of String) = template.ParentPath & "/version.txt"
        Dim ver%() = version.Value _
            .ReadAllText _
            .Split("."c) _
            .Select(Function(t) CInt(Val(t))) _
            .ToArray

        With CType(version, String)
            Call (version = ver.versioning().JoinBy(".")).SaveTo(.ByRef)
        End With

        Dim description = template _
            .IterateAllLines _
            .Where(Function(l) Not l.StringEmpty) _
            .Select(Function(l)
                        Return l.GetTagValue(":", trim:=True)
                    End Function)

        With New StreamWriter(Console.OpenStandardOutput)
            For Each tuple In description
                If tuple.Name.TextEquals("version") Then
                    Call .WriteLine($"Version: {version}")
                ElseIf tuple.Name.StringEmpty Then
                    Call .WriteLine($"  {tuple.Value}")
                Else
                    Call .WriteLine($"{tuple.Name}: {tuple.Value}")
                End If
            Next

            Call .Flush()
            Call .Dispose()
        End With

        Return 0
    End Function

    <ExportAPI("/summary")>
    <Description("Add Rscript file summary header")>
    <Usage("/summary /src <*.R directory> [/out <*.R directory, default=""/src"">]")>
    Public Function Summary(args As CommandLine) As Integer
        Dim src$ = args <= "/src"
        Dim out$ = args!out Or src

        For Each path As String In ls - l - r - "*.R" <= src
            Dim relativePath$ = ProgramPathSearchTool.RelativePath(
                pcFrom:=src,
                pcTo:=path,
                appendParent:=False
            )
            Dim output$ = $"{out}/{relativePath}"

            Call RFileHeader.Summary(path, src) _
                            .SaveTo(path:=output,
                                    encoding:=Encodings.UTF8WithoutBOM.CodePage
                             )
        Next

        Return 0
    End Function

    Public Function Main() As Integer
        Return GetType(Program).RunCLI(App.CommandLine, Nothing, executeNotFound:=AddressOf Versioning)
    End Function
End Module

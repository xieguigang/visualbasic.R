Imports System.IO
Imports System.Runtime.CompilerServices
Imports Microsoft.VisualBasic.Language

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

    Sub Main()
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

        With New StringWriter(Console.OpenStandardOutput)
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
    End Sub
End Module

Imports System.IO
Imports Microsoft.VisualBasic.Language

Module Program

    Sub Main()
        Dim template$ = App.CommandLine.Name
        Dim version As Value(Of String) = template.ParentPath & "/version.txt"
        Dim ver#() = version.Value _
            .ReadAllText _
            .Split("."c) _
            .Select(AddressOf Val) _
            .ToArray

        For i As Integer = ver.Length - 1 To 0 Step -1
            If i > 0 AndAlso ver(i) = 999 Then
                ver(i) = 0
                ver(i - 1) += 1
            End If
        Next

        With CType(version, String)
            Call (version = ver.JoinBy(".")).SaveTo(.ByRef)
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

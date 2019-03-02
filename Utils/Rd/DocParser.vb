Imports Microsoft.VisualBasic.Emit.Marshal
Imports Microsoft.VisualBasic.Language
Imports Microsoft.VisualBasic.Text

Public Class DocParser

    Dim text As Pointer(Of Char)
    Dim buffer As New List(Of Char)
    Dim parserEscape As New Escapes

    ''' <summary>
    ''' Rd文件之中的注释为单行注释
    ''' </summary>
    Dim docComments As String = ""


    Public Class Escapes

        Public docComment As Boolean = False

    End Class

    Private Sub New(text As Pointer(Of Char))
        Me.text = text
    End Sub

    Public Function CreateDoc() As RDoc
        Do While Not text.EndRead
            Call walkChar(++text)
        Loop

        Return New RDoc With {
            .comments = docComments
        }
    End Function

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="rd">The text content of ``*.Rd`` file.</param>
    ''' <returns></returns>
    Public Shared Function ParseDoc(rd As String) As RDoc
        Return New DocParser(text:=New Pointer(Of Char)(rd.SolveStream)).CreateDoc
    End Function

    Private Sub walkChar(c As Char)
        If c = "%"c AndAlso buffer = 0 Then
            ' 以%符号起始，并且缓存为空，则说明是一条注释文本
            parserEscape.docComment = True
        ElseIf c = ASCII.CR OrElse c = ASCII.LF Then

        ElseIf parserEscape.docComment Then
            buffer += c
        End If
    End Sub
End Class
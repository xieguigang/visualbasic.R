Imports Microsoft.VisualBasic.Emit.Marshal

Module DocParser

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="rd">The text content of ``*.Rd`` file.</param>
    ''' <returns></returns>
    Public Function ParseDoc(rd As String) As RDoc
        Dim text As New Pointer(Of Char)(rd.SolveStream)
    End Function
End Module

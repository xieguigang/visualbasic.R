
''' <summary>
''' The R document model
''' </summary>
Public Class RDoc

    Public Property name As String
    Public Property [alias] As String
    Public Property title As String
    Public Property usage As String
    Public Property arguments As Item()
    Public Property description As Doc

End Class

Public Class Item
    Public Property name As String
    Public Property description As Doc
End Class

Public Class Doc

End Class

Public Class Enumerate
    Public Property items As Doc()
End Class
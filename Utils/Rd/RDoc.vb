
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
    Public Property examples As String

End Class

Public Class Item
    Public Property name As String
    Public Property description As Doc
End Class

Public Class Enumerate
    Public Property items As Doc()
End Class

#Region "Doc content model"

Public Class Doc

    Public Property Fragments As DocFragment()

    Public ReadOnly Property PlainText As String
        Get
            Return ToString()
        End Get
    End Property

    Public Function GetMarkdown() As String

    End Function

    Public Function GetHtml() As String

    End Function

    Public Overrides Function ToString() As String
        Return Fragments.Select(Function(frag) frag.ToString).JoinBy(" ")
    End Function
End Class

Public MustInherit Class DocFragment

End Class

Public Class PlainText : Inherits DocFragment

    Public Property text As String

    Public Overrides Function ToString() As String
        Return text
    End Function
End Class

Public Class Code : Inherits DocFragment

    Public Property content As DocFragment

    Public Overrides Function ToString() As String
        Return content.ToString
    End Function
End Class

Public Class Link : Inherits DocFragment

    Public Property target As String

    Public Overrides Function ToString() As String
        Return target.ToString
    End Function
End Class

#End Region
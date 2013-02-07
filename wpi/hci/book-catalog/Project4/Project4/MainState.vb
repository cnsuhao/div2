Public Class MainState
    ' all data between forms is passed through here
    Public myCatalog As Catalog
    Public User As User
    Public filteredCatalog As Catalog
    Public selectedBook As Book
    Public browseType As String
    Public searchedBy As String
    Public searched As String

    Public rootForm As Object
    Public main As MainScreen

    Public Sub New(ByVal root)
        rootForm = root
        main = root
        myCatalog = LoadCatalog
        User = Nothing
        filteredCatalog = Nothing
        selectedBook = Nothing
    End Sub

    ' initial load of catalog - MODIFY CATALOG NAME HERE
    Public ReadOnly Property LoadCatalog() As Catalog
        Get
            Dim cat As Catalog
            cat = Serializable.Load(DataFolder & "\bookcatalog.xml", GetType(Catalog))

            Return cat
        End Get
    End Property

    ' used to load catalog - MODIFY RELATIVE DIRECTORY HERE
    Public ReadOnly Property DataFolder() As String
        Get
            Return Environment.CurrentDirectory & "\Books"
        End Get
    End Property
End Class

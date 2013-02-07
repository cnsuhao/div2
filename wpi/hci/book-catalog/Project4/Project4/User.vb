Imports System.Xml.Serialization

Public Class User
    Inherits Serializable

    Public UserName As String
    Public Password As String   'for actual product, this would have some sort of encryption

    Public FirstName As String
    Public LastName As String
    Public Email As String
    ' There could be a lot more fields than this, but for now...

    ' create wishlist NOTE: Not A CATALOG, was having wrapper issues so tagged catalog onto the end
    <XmlIgnore()> Public Books As New ArrayList()

    Public Sub AddBook(ByVal book As Book)
        Books.Add(book)
    End Sub

    Public Property Wishlist() As Book()
        Get
            Dim list(Books.Count - 1) As Book
            Books.CopyTo(list)
            Return list
        End Get
        Set(ByVal value As Book())
            ' loading(functions)
            Books.Clear()
            If Not value Is Nothing Then
                Dim book As Book
                For Each book In value
                    Books.Add(book)
                Next
            End If
        End Set
    End Property

    Public ReadOnly Property DataFolder() As String
        Get
            Return Environment.CurrentDirectory & "\Users\"
        End Get
    End Property

    Public ReadOnly Property WishlistDataTable() As DataTable
        Get
            Dim tbl As DataTable
            tbl = New DataTable("Books")

            Try
                Dim Title As DataColumn = New DataColumn("Title")
                Title.DataType = System.Type.GetType("System.String")
                tbl.Columns.Add(Title)

                Dim Author As DataColumn = New DataColumn("Author")
                Author.DataType = System.Type.GetType("System.String")
                tbl.Columns.Add(Author)

                Dim Genre As DataColumn = New DataColumn("Genre")
                Genre.DataType = System.Type.GetType("System.String")
                tbl.Columns.Add(Genre)

                Dim ID As DataColumn = New DataColumn("ID")
                ID.DataType = System.Type.GetType("System.Single")
                tbl.Columns.Add(ID)

                Dim book As Book
                Dim c As Integer = 0
                For Each book In Books
                    Dim r As DataRow = tbl.NewRow()
                    r.Item("id") = c
                    c = c + 1
                    r.Item("Title") = book.Title
                    r.Item("Author") = book.Author
                    r.Item("Genre") = book.Genre
                    tbl.Rows.Add(r)
                Next
            Catch
            End Try

            Return tbl
        End Get
    End Property
End Class

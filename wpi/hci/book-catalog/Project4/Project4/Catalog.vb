Imports System.Xml.Serialization

Public Class Catalog
    Inherits Serializable

    <XmlIgnore()> Public Items As New ArrayList()

    Public Sub AddBook(ByVal book As Book)
        Items.Add(book)
    End Sub

    ' search catalog for a specific book
    Public Function SearchCatalog(ByVal value As String, ByVal type As String) As Catalog
        Dim cat As New Catalog()
        Dim book As Book

        For Each book In Items
            If book.EqualsByType(value, type) Then
                cat.AddBook(book)
            End If
        Next

        Return cat
    End Function

    'union of letter and catalog
    Public Function BrowseByLetter(ByVal type As String, ByVal letter As String) As Catalog
        Dim cat As New Catalog()
        Dim book As Book

        If type = "Title" Then
            For Each book In Items
                If CheckTitleLetter(letter, book) Then cat.AddBook(book)
            Next
        Else
            For Each book In Items
                If CheckAuthorLetter(letter, book) Then cat.AddBook(book)
            Next
        End If

        Return cat
    End Function

    ' union of letters and catalog
    Public Function BrowseByLetters(ByVal type As String, ByVal letters As String()) As Catalog
        Dim cat As New Catalog()
        Dim book As Book

        If type = "Title" Then
            For Each book In Items
                Dim i As Integer
                For i = 0 To letters.Length - 1
                    If CheckTitleLetter(letters(i), book) Then cat.AddBook(book)
                Next
            Next
        Else
            For Each book In Items
                Dim i As Integer
                For i = 0 To letters.Length - 1
                    If CheckAuthorLetter(letters(i), book) Then cat.AddBook(book)
                Next
            Next
        End If

        Return cat
    End Function

    ' check title for letter (didn't do numbers)
    Public Function CheckTitleLetter(ByVal letter As String, ByVal book As Book) As Boolean
        If book.Title.StartsWith("The ") Then
            Return (book.Title.ToLower.Substring(4).StartsWith(letter.ToLower))
        Else
            Return (book.Title.ToLower.StartsWith(letter.ToLower))
        End If
    End Function

    ' see if author's last name starts with correct letter
    Public Function CheckAuthorLetter(ByVal letter As String, ByVal book As Book) As Boolean
        Dim author As String
        Dim lastSpace As Integer
        lastSpace = book.Author.LastIndexOf(" ")

        If lastSpace >= 0 Then
            author = book.Author.Substring(lastSpace + 1).ToLower
        Else
            author = book.Author.ToLower
        End If

        Return author.StartsWith(letter.ToLower)
    End Function

    ' union of genrelist to catalog
    Public Function BrowseByGenre(ByVal genre As ArrayList) As Catalog
        Dim cat As New Catalog()
        Dim book As Book

        For Each book In Items
            If genre.Contains(book.Genre) Then
                cat.AddBook(book)
            End If
        Next

        Return cat
    End Function

    ' get a list of every genre
    Public ReadOnly Property AllGenre() As ArrayList
        Get
            Dim list As New ArrayList()
            Dim book As Book

            For Each book In Items
                Dim genre As String
                Dim x As Boolean
                x = False
                ' see if contains genre
                For Each genre In list
                    x = x Or (genre = book.Genre)
                Next
                If Not x Then list.Add(book.Genre)
            Next

            Return list
        End Get
    End Property

    ' for serializing book tag
    Public Property Books() As Book()
        Get
            Dim list(Items.Count - 1) As Book
            Items.CopyTo(list)
            Return list
        End Get
        Set(ByVal value As Book())
            ' loading(functions)
            Items.Clear()
            If Not value Is Nothing Then
                Dim book As Book
                For Each book In value
                    Items.Add(book)
                Next
            End If
        End Set
    End Property

    ' Construct datatable for datagridview
    Public ReadOnly Property DataTable() As DataTable
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
                For Each book In Items
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

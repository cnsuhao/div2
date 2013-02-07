Public Class Book
    Inherits Serializable

    Public Genre As String
    Public Author As String
    Public Title As String

    Public Function EqualsByType(ByVal value As String, ByVal type As String) As Boolean
        If type.ToLower = "genre" Then Return Genre.ToLower.Contains(value)
        If type.ToLower = "author" Then Return Author.ToLower.Contains(value)
        If type.ToLower = "title" Then Return Title.ToLower.Contains(value)
    End Function

    Public Overrides Function ToString() As String
        Return Title & "       " & Author & "       " & Genre
    End Function

    Public Function EqualsBook(ByVal book As Book) As Boolean
        Return (book.Genre.Equals(Genre) And book.Author.Equals(Author) And book.Title.Equals(Title))
    End Function
End Class

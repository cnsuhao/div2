Imports System.IO
Imports System.Xml.Serialization

' code more or less taken straight from VB Course Book

Public Class Serializable
    Public Sub Save(ByVal filename As String)
        Dim tmpName As String
        tmpName = filename & ".tmp"

        Dim tmpInfo As New FileInfo(tmpName)
        If tmpInfo.Exists = True Then tmpInfo.Delete()

        Dim stream As New FileStream(tmpName, FileMode.Create)

        Save(stream)

        stream.Close()

        Dim orig As New FileInfo(filename)
        orig.Delete()

        tmpInfo.CopyTo(filename)
        tmpInfo.Delete()
    End Sub

    Public Sub Save(ByVal stream As Stream)
        Dim serializer As New XmlSerializer(Me.GetType)
        serializer.Serialize(stream, Me)
    End Sub

    Public Shared Function Load(ByVal filename As String, ByVal type As Type) As Object

        Dim fInfo As New FileInfo(filename)
        If fInfo.Exists = False Then
            Return System.Activator.CreateInstance(type)
        End If

        Dim stream As New FileStream(filename, FileMode.Open)

        Dim obj As Object = Load(stream, type)

        stream.Close()

        Return obj
    End Function

    Public Shared Function Load(ByVal stream As Stream, ByVal type As Type) As Object
        Dim serializer As New XmlSerializer(type)
        Dim obj As Object = serializer.Deserialize(stream)

        Return obj
    End Function
End Class

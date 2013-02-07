Public Class BrowseGenre
    Private CallingForm As Object
    Public myState As MainState

    Public Sub New(ByVal caller As Object)
        MyBase.New()
        InitializeComponent()
        '  Note which form has called this one
        CallingForm = caller
        myState = caller.myState
        ' This call is required by the Windows Form Designer.
        ' Add any initialization after the InitializeComponent() call.

        UpdateList()

        cListBox.CheckOnClick = True
    End Sub

    Private Sub UpdateList()
        cListBox.Items.Clear()

        Dim genre As String
        For Each genre In myState.myCatalog.AllGenre
            cListBox.Items.Add(genre)
        Next

        Results.Text = "Browse Genre: " & cListBox.Items.Count
    End Sub

    Private Sub Form2_Closing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        e.Cancel = True
        Me.Hide()
        '  If the CallingForm still exists then show it now
        myState.main.Close()
    End Sub

    Private Sub Main_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Main.Click
        Me.Hide()
        '  If the CallingForm still exists then show it now
        Me.myState.rootForm.Show()
    End Sub

    Private Sub Back_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Back.Click
        Me.Hide()
        '  If the CallingForm still exists then show it now
        If Not CallingForm Is Nothing Then CallingForm.Show()
        '  then dispose of it's reference here.
        CallingForm = Nothing
    End Sub

    Private Sub Browse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Browse.Click
        Dim frm As New SearchMain(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim items As New ArrayList()
        Dim Entry As Object
        For Each Entry In cListBox.CheckedItems
            items.Add(Entry)
        Next

        myState.filteredCatalog = myState.myCatalog.BrowseByGenre(items)
        Dim frm As New BrowseResults(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Wishlist_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Wishlist.Click
        If myState.User Is Nothing Then
            Dim i As Integer = MsgBox("You have not logged in" & vbCrLf & vbCrLf & "Would you like to do so now?", MsgBoxStyle.YesNo + MsgBoxStyle.Exclamation)

            If i = Windows.Forms.DialogResult.Yes Then
                Dim frm As New LoginScreen(Me, "main")
                frm.Show()
                Me.Hide()
            End If
        Else
            Dim frm As New WishlistDisplay(Me)
            frm.Show()
            Me.Hide()
        End If
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        If cListBox.CheckedItems.Count < cListBox.Items.Count Then
            Dim i As Integer
            For i = 0 To cListBox.Items.Count - 1
                cListBox.SetItemChecked(i, True)
            Next
        Else
            Dim i As Integer
            For i = 0 To cListBox.Items.Count - 1
                cListBox.SetItemChecked(i, False)
            Next
        End If
    End Sub
End Class
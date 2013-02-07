Public Class SearchMain

    Private CallingForm As Object
    Public myState As MainState

    Private Sub Label2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label2.Click

    End Sub
    Private Sub Login_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Back.Click
        Me.Hide()
        '  If the CallingForm still exists then show it now
        If Not CallingForm Is Nothing Then CallingForm.Show()
        '  then dispose of it's reference here.
        CallingForm = Nothing
    End Sub
    Private Sub Label3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub
    Private Sub GroupBox1_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GroupBox1.Enter

    End Sub
    Private Sub GroupBox2_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GroupBox2.Enter

    End Sub
    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click

    End Sub

    Public Sub New(ByVal caller As Object)
        MyBase.New()
        InitializeComponent()
        '  Note which form has called this one
        CallingForm = caller
        myState = caller.myState
        ' This call is required by the Windows Form Designer.
        ' Add any initialization after the InitializeComponent() call.
    End Sub

    Private Sub Form2_Closing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        e.Cancel = True
        Me.Hide()
        '  If the CallingForm still exists then show it now
        myState.main.Close()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        SearchType.Text = "Title"
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        SearchType.Text = "Author"
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        SearchType.Text = "Genre"
    End Sub

    Private Sub Main_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Main.Click
        Me.Hide()
        '  If the CallingForm still exists then show it now
        Me.myState.rootForm.Show()
    End Sub

    Private Sub Label10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label10.Click

    End Sub

    Private Sub Browse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Browse.Click
        Dim frm As New BrowseMain(Me)
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

    Private Sub TextBox1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox1.TextChanged
        
    End Sub

    Private Sub SearchType_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SearchType.Click

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Search.Click
        myState.searchedBy = SearchType.Text
        myState.searched = TextBox1.Text
        myState.filteredCatalog = myState.myCatalog.SearchCatalog(TextBox1.Text.ToLower, SearchType.Text)
        Dim frm As New SearchResults(Me)
        frm.Show()
        Me.Hide()
    End Sub
End Class
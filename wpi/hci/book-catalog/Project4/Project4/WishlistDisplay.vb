Public Class WishlistDisplay
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

        

        Dim col As Windows.Forms.DataGridViewColumn
        For Each col In DataGridView1.Columns
            If col.Index = 0 Then
                col.Width = 450
            ElseIf col.Index = 1 Or col.Index = 2 Then
                col.Width = 140
            ElseIf col.Index = 3 Then
                col.Visible = False
            End If
        Next
    End Sub

    Private Sub UpdateList()
        If myState.User.Books.Count = 0 Then
            Button1.Enabled = False
            Button2.Enabled = False
            Button3.Enabled = False
        End If

        bs.DataSource = myState.User.WishlistDataTable
        DataGridView1.DataSource = bs

        Results.Text = "Your Wishlist: " & myState.User.Books.Count
    End Sub
    Private Sub Form2_Closing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        e.Cancel = True
        Me.Hide()
        '  If the CallingForm still exists then show it now
        If Not CallingForm Is Nothing Then CallingForm.Show()
        '  then dispose of it's reference here.
        myState.main.Close()
    End Sub
    Private Sub Back_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Back.Click
        Me.Hide()
        '  If the CallingForm still exists then show it now
        If Not CallingForm Is Nothing Then CallingForm.Show()
        '  then dispose of it's reference here.
        CallingForm = Nothing
    End Sub

    Private Sub Main_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Main.Click
        Me.Hide()
        '  If the CallingForm still exists then show it now
        Me.myState.rootForm.Show()
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        myState.filteredCatalog = myState.myCatalog
        Dim frm As New BrowseResults(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        MsgBox("Your Purchase has been sent, please pick your book up and pay at the counter")
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        myState.selectedBook = GetSelected()
        Dim frm As New ViewBook(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub GroupBox1_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub GroupBox2_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GroupBox2.Enter

    End Sub

    Private Sub Label10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label10.Click

    End Sub

    Private Sub Browse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Browse.Click
        Dim frm As New SearchMain(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Wishlist_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Wishlist.Click
        Dim frm As New BrowseMain(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Button2_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim book As Book
        Dim toremove As Book = Nothing

        For Each book In myState.User.Books
            If book.EqualsBook(GetSelected()) Then
                toremove = book
            End If
        Next

        If Not toremove Is Nothing Then
            myState.User.Books.Remove(toremove)
        End If


        myState.User.Save(myState.User.DataFolder & myState.User.UserName & ".xml")

        MsgBox("Book removed from your wishlist")

        UpdateList()
    End Sub

    Private Sub DataGridView1_CellContentClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs)

    End Sub

    Public Function GetSelected() As Book
        Dim row As Windows.Forms.DataGridViewRow = DataGridView1.SelectedRows(0)
        Dim i As Integer = row.Cells(3).Value

        Return myState.User.Books(i)
    End Function
End Class
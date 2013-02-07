Public Class BrowseResults
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

        Dim dt As DataTable = myState.filteredCatalog.DataTable
        bs.DataSource = dt
        DataGridView1.DataSource = bs

        Results.Text = "Browse Results: " & myState.filteredCatalog.Items.Count

        If myState.filteredCatalog.Items.Count = 0 Then
            Button1.Enabled = False
            Button2.Enabled = False
            Button3.Enabled = False
        End If

        Dim index As Integer = -1
        If myState.browseType = "Title" Then
            index = 0
        ElseIf myState.browseType = "Author" Then
            index = 1
        ElseIf myState.browseType = "Genre" Then
            index = 2
        End If

        If Not index = -1 Then
            DataGridView1.Sort(DataGridView1.Columns(index), ComponentModel.ListSortDirection.Ascending)
        End If

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

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        myState.filteredCatalog = myState.myCatalog
        Dim frm As New BrowseResults(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        MsgBox("Your Purchase has been sent, please pick your book up and pay at the counter")
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        If myState.User Is Nothing Then
            Dim i As Integer = MsgBox("You have not logged in" & vbCrLf & vbCrLf & "Would you like to do so now?", MsgBoxStyle.YesNo + MsgBoxStyle.Exclamation)

            If i = Windows.Forms.DialogResult.Yes Then
                Dim frm As New LoginScreen(Me, "")
                frm.Show()
                Me.Hide()
            End If
        Else
            Dim books As ArrayList = myState.User.Books
            Dim newbook As Book = GetSelected()
            Dim isNew As Boolean = True
            Dim book As Book

            For Each book In books
                isNew = isNew And (Not newbook.EqualsBook(book))
            Next

            If Not isNew Then
                MsgBox("Book already in wishlist!")
            Else
                myState.User.AddBook(GetSelected())
                myState.User.Save(myState.User.DataFolder & myState.User.UserName & ".xml")

                MsgBox("Book added to your wishlist")
            End If
        End If
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        myState.selectedBook = GetSelected()
        Dim frm As New ViewBook(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub GroupBox1_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GroupBox1.Enter

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

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click

    End Sub

    Private Sub Results_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Results.Click

    End Sub

    Private Sub BrowseResults_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Dim frm As New BrowseMain(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Public Function GetSelected() As Book
        Dim row As Windows.Forms.DataGridViewRow = DataGridView1.SelectedRows(0)
        Dim i As Integer = row.Cells(3).Value

        Return myState.filteredCatalog.Items(i)
    End Function
End Class
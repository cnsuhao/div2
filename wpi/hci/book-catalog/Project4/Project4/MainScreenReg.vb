Public Class MainScreenReg

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
        Dim name As String
        name = myState.User.FirstName

        If name = "" Then
            name = myState.User.UserName
        End If

        Header.Text = "Welcome, " & name & ","
    End Sub

    Private Sub Form2_Closing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        e.Cancel = True
        Me.Hide()
        '  If the CallingForm still exists then show it now
        myState.main.Close()
    End Sub

    Private Sub Search_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Search.Click
        Dim frm As New SearchMain(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim frm As New BrowseMain(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim frm As New WishlistDisplay(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        MsgBox("Feature is not yet supported")
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        MsgBox("Feature is not yet supported")
    End Sub

    Private Sub Login_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Login.Click
        Dim frm As New EditUser(Me)
        frm.Show()
        Me.Hide()
    End Sub
End Class
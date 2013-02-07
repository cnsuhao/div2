Public Class MainScreen

    Public myState As MainState

    Public Sub New()
        InitializeComponent()
        '  Note which form has called this one
        myState = New MainState(Me)
        ' This call is required by the Windows Form Designer.
        ' Add any initialization after the InitializeComponent() call.

    End Sub

    Private Sub Search_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Search.Click
        Dim frm As New SearchMain(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Browse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Browse.Click
        Dim frm As New BrowseMain(Me)
        frm.Show()
        Me.Hide()
    End Sub

    Private Sub Login_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Login.Click
        Dim frm As New LoginScreen(Me, "main")
        frm.Show()
        Me.Hide()
    End Sub
End Class
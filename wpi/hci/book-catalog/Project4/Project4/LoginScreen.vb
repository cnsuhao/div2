Public Class LoginScreen

    Private CallingForm As Object
    Public myState As MainState
    Private myTarget As String

    Private Sub Login_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Back.Click
        Me.Hide()
        '  If the CallingForm still exists then show it now
        If Not CallingForm Is Nothing Then CallingForm.Show()
        '  then dispose of it's reference here.
        CallingForm = Nothing
    End Sub

    Public Sub New(ByVal caller As Object, ByVal target As String)
        MyBase.New()
        InitializeComponent()
        '  Note which form has called this one
        CallingForm = caller
        myState = caller.myState
        myTarget = target
        ' This call is required by the Windows Form Designer.
        ' Add any initialization after the InitializeComponent() call.
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

    Private Sub Browse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Browse.Click
        Dim frm As New SearchMain(Me)
        frm.Show()
        Me.Hide()
    End Sub
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Search.Click
        Dim usr As User = Serializable.Load(DataFolder & login.Text & ".xml", GetType(User))

        If usr.UserName = "" Then
            MsgBox("Invalid username or password")
        ElseIf usr.Password = pass.Text Then
            myState.User = usr

            MsgBox("Login successful")
            myState.rootForm = New MainScreenReg(CallingForm)
            Me.Hide()
            If myTarget = "main" Then
                Me.myState.rootForm.Show()
            Else
                CallingForm.Show()
            End If
        Else
            MsgBox("Invalid username or password")
        End If
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If myTarget = "main" Then
            Dim frm As New CreateUser(Me, "main")
            frm.Show()
            Me.Hide()
        Else
            Dim frm As New CreateUser(CallingForm, "")
            frm.Show()
            Me.Hide()
        End If
        
    End Sub

    Private Sub Wishlist_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Wishlist.Click
        Dim frm As New BrowseMain(Me)
        frm.Show()
        Me.Hide()
    End Sub
    Public ReadOnly Property DataFolder() As String
        Get
            Return Environment.CurrentDirectory & "\Users\"
        End Get
    End Property
End Class
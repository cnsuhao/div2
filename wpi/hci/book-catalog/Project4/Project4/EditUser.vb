Imports System.IO

Public Class EditUser

    Private CallingForm As Object
    Public myState As MainState

    Public Sub New(ByVal caller As Object)
        MyBase.New()
        InitializeComponent()
        '  Note which form has called this one
        CallingForm = caller
        myState = caller.myState

        user.Text = myState.User.UserName
        pass.Text = myState.User.Password
        conf.Text = myState.User.Password

        first.Text = myState.User.FirstName
        last.Text = myState.User.LastName
        mail.Text = myState.User.Email
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
        Me.Hide()
        '  If the CallingForm still exists then show it now
        If Not CallingForm Is Nothing Then CallingForm.Show()
        '  then dispose of it's reference here.
        CallingForm = Nothing
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim errors As ArrayList
        errors = CheckErrors()

        If errors.Count = 0 Then
            myState.User.Password = pass.Text
            myState.User.FirstName = first.Text
            myState.User.LastName = last.Text
            myState.User.Email = mail.Text

            Dim file As String = DataFolder & myState.User.UserName & ".xml"

            myState.User.Save(file)

            MsgBox("Changes saved")

            myState.rootForm = New MainScreenReg(CallingForm)

            Me.Hide()
            myState.rootForm.Show()
        Else
            Dim printout As String
            printout = "The following errors have occured:   "
            Dim err As String
            For Each err In errors
                printout = printout & err & "; "
            Next
            MsgBox(printout)
        End If
    End Sub

    Private Function CheckErrors() As ArrayList
        Dim err As New ArrayList()

        'check passwords
        If pass.Text = "" Then
            err.Add("Password field is blank")
        ElseIf Not pass.Text = conf.Text Then
            err.Add("Passwords do not match")
        End If

        If conf.Text = "" Then
            err.Add("Confirmation password is blank")
        End If

        Return err
    End Function

    Public ReadOnly Property DataFolder() As String
        Get
            Return Environment.CurrentDirectory & "\Users\"
        End Get
    End Property
End Class

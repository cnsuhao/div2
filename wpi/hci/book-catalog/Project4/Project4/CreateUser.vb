Imports System.IO

Public Class CreateUser

    Private CallingForm As Object
    Public myState As MainState
    Private myTarget As String

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

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Me.Hide()
        If myTarget = "main" Then
            Me.myState.rootForm.Show()
        Else
            CallingForm.Show()
        End If
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim errors As ArrayList
        errors = CheckErrors()

        If errors.Count = 0 Then
            Dim usr As New User()
            usr.UserName = user.Text
            usr.Password = pass.Text
            usr.FirstName = first.Text
            usr.LastName = last.Text
            usr.Email = mail.Text

            Dim file As String = DataFolder & user.Text & ".xml"
            usr.Save(file)
            myState.User = usr

            myState.rootForm = New MainScreenReg(CallingForm)

            Me.Hide()
            If myTarget = "main" Then
                Me.myState.rootForm.Show()
            Else
                CallingForm.Show()
            End If
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

        'check username
        
        Dim tmpName As String
        tmpName = DataFolder & user.Text & ".xml"
        Dim tmpInfo As New FileInfo(tmpName)

        If user.Text = "" Then
            err.Add("Username is blank")
        ElseIf tmpInfo.Exists Then
            err.Add("Username already exists, ")
        End If

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
<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class LoginScreen
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(LoginScreen))
        Me.Back = New System.Windows.Forms.Button
        Me.Search = New System.Windows.Forms.Button
        Me.GroupBox1 = New System.Windows.Forms.GroupBox
        Me.Label4 = New System.Windows.Forms.Label
        Me.Label6 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.SearchType = New System.Windows.Forms.Label
        Me.pass = New System.Windows.Forms.TextBox
        Me.login = New System.Windows.Forms.TextBox
        Me.Label1 = New System.Windows.Forms.Label
        Me.Wishlist = New System.Windows.Forms.Button
        Me.Browse = New System.Windows.Forms.Button
        Me.Main = New System.Windows.Forms.Button
        Me.Label10 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label7 = New System.Windows.Forms.Label
        Me.Label9 = New System.Windows.Forms.Label
        Me.GroupBox2 = New System.Windows.Forms.GroupBox
        Me.Button1 = New System.Windows.Forms.Button
        Me.Label8 = New System.Windows.Forms.Label
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'Back
        '
        Me.Back.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Back.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Back.Location = New System.Drawing.Point(125, 525)
        Me.Back.Name = "Back"
        Me.Back.Size = New System.Drawing.Size(105, 42)
        Me.Back.TabIndex = 14
        Me.Back.Text = "Back"
        Me.Back.UseVisualStyleBackColor = True
        '
        'Search
        '
        Me.Search.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Search.Font = New System.Drawing.Font("Impact", 26.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Search.Location = New System.Drawing.Point(578, 56)
        Me.Search.Name = "Search"
        Me.Search.Size = New System.Drawing.Size(164, 76)
        Me.Search.TabIndex = 20
        Me.Search.Text = "Login"
        Me.Search.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        Me.GroupBox1.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.GroupBox1.Controls.Add(Me.Label4)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.SearchType)
        Me.GroupBox1.Controls.Add(Me.Search)
        Me.GroupBox1.Controls.Add(Me.pass)
        Me.GroupBox1.Controls.Add(Me.login)
        Me.GroupBox1.Location = New System.Drawing.Point(12, 186)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(766, 148)
        Me.GroupBox1.TabIndex = 12
        Me.GroupBox1.TabStop = False
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Font = New System.Drawing.Font("Comic Sans MS", 18.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.ForeColor = System.Drawing.SystemColors.HotTrack
        Me.Label4.Location = New System.Drawing.Point(456, 16)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(74, 35)
        Me.Label4.TabIndex = 19
        Me.Label4.Text = "Login"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Font = New System.Drawing.Font("Comic Sans MS", 18.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label6.Location = New System.Drawing.Point(6, 16)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(465, 35)
        Me.Label6.TabIndex = 24
        Me.Label6.Text = "Enter your username and password to "
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label3.Location = New System.Drawing.Point(7, 103)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(107, 29)
        Me.Label3.TabIndex = 23
        Me.Label3.Text = "Password"
        Me.Label3.TextAlign = System.Drawing.ContentAlignment.TopRight
        '
        'SearchType
        '
        Me.SearchType.AutoSize = True
        Me.SearchType.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.SearchType.Location = New System.Drawing.Point(7, 58)
        Me.SearchType.Name = "SearchType"
        Me.SearchType.Size = New System.Drawing.Size(114, 29)
        Me.SearchType.TabIndex = 22
        Me.SearchType.Text = "Username"
        Me.SearchType.TextAlign = System.Drawing.ContentAlignment.TopRight
        '
        'pass
        '
        Me.pass.Font = New System.Drawing.Font("Microsoft Sans Serif", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.pass.Location = New System.Drawing.Point(127, 97)
        Me.pass.Name = "pass"
        Me.pass.PasswordChar = Global.Microsoft.VisualBasic.ChrW(42)
        Me.pass.Size = New System.Drawing.Size(435, 35)
        Me.pass.TabIndex = 2
        '
        'login
        '
        Me.login.Font = New System.Drawing.Font("Microsoft Sans Serif", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.login.Location = New System.Drawing.Point(127, 56)
        Me.login.Name = "login"
        Me.login.Size = New System.Drawing.Size(435, 35)
        Me.login.TabIndex = 1
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Impact", 64.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.ForeColor = System.Drawing.SystemColors.HotTrack
        Me.Label1.Location = New System.Drawing.Point(330, -10)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(465, 105)
        Me.Label1.TabIndex = 9
        Me.Label1.Text = "*Bookstore*"
        '
        'Wishlist
        '
        Me.Wishlist.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Wishlist.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Wishlist.Location = New System.Drawing.Point(560, 525)
        Me.Wishlist.Name = "Wishlist"
        Me.Wishlist.Size = New System.Drawing.Size(105, 42)
        Me.Wishlist.TabIndex = 15
        Me.Wishlist.Text = "Browse"
        Me.Wishlist.UseVisualStyleBackColor = True
        '
        'Browse
        '
        Me.Browse.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Browse.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Browse.Location = New System.Drawing.Point(670, 525)
        Me.Browse.Name = "Browse"
        Me.Browse.Size = New System.Drawing.Size(105, 42)
        Me.Browse.TabIndex = 16
        Me.Browse.Text = "Search"
        Me.Browse.UseVisualStyleBackColor = True
        '
        'Main
        '
        Me.Main.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Main.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Main.Location = New System.Drawing.Point(15, 525)
        Me.Main.Name = "Main"
        Me.Main.Size = New System.Drawing.Size(105, 42)
        Me.Main.TabIndex = 17
        Me.Main.Text = "To Main"
        Me.Main.UseVisualStyleBackColor = True
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(-3, 510)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(799, 13)
        Me.Label10.TabIndex = 18
        Me.Label10.Text = resources.GetString("Label10.Text")
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Impact", 56.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(12, 90)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(469, 93)
        Me.Label2.TabIndex = 10
        Me.Label2.Text = "Account Login"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Font = New System.Drawing.Font("Comic Sans MS", 21.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label7.ForeColor = System.Drawing.SystemColors.HotTrack
        Me.Label7.Location = New System.Drawing.Point(321, 64)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(108, 40)
        Me.Label7.TabIndex = 10
        Me.Label7.Text = "Today!"
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Font = New System.Drawing.Font("Comic Sans MS", 21.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label9.Location = New System.Drawing.Point(154, 64)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(170, 40)
        Me.Label9.TabIndex = 8
        Me.Label9.Text = "Create one"
        '
        'GroupBox2
        '
        Me.GroupBox2.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.GroupBox2.Controls.Add(Me.Button1)
        Me.GroupBox2.Controls.Add(Me.Label7)
        Me.GroupBox2.Controls.Add(Me.Label8)
        Me.GroupBox2.Controls.Add(Me.Label9)
        Me.GroupBox2.Location = New System.Drawing.Point(12, 352)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(766, 128)
        Me.GroupBox2.TabIndex = 19
        Me.GroupBox2.TabStop = False
        '
        'Button1
        '
        Me.Button1.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button1.Font = New System.Drawing.Font("Impact", 26.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button1.Location = New System.Drawing.Point(473, 28)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(269, 76)
        Me.Button1.TabIndex = 25
        Me.Button1.Text = "Create Account"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Font = New System.Drawing.Font("Comic Sans MS", 20.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label8.Location = New System.Drawing.Point(24, 28)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(343, 38)
        Me.Label8.TabIndex = 9
        Me.Label8.Text = "Don't have an Account? "
        '
        'LoginScreen
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(792, 566)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.Main)
        Me.Controls.Add(Me.Browse)
        Me.Controls.Add(Me.Wishlist)
        Me.Controls.Add(Me.Back)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.MaximumSize = New System.Drawing.Size(800, 600)
        Me.Name = "LoginScreen"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = " "
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Back As System.Windows.Forms.Button
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Wishlist As System.Windows.Forms.Button
    Friend WithEvents Browse As System.Windows.Forms.Button
    Friend WithEvents Main As System.Windows.Forms.Button
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents Search As System.Windows.Forms.Button
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents pass As System.Windows.Forms.TextBox
    Friend WithEvents login As System.Windows.Forms.TextBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents SearchType As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Label8 As System.Windows.Forms.Label
End Class

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class BrowseGenre
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(BrowseGenre))
        Me.Label10 = New System.Windows.Forms.Label
        Me.Main = New System.Windows.Forms.Button
        Me.Browse = New System.Windows.Forms.Button
        Me.Wishlist = New System.Windows.Forms.Button
        Me.Label1 = New System.Windows.Forms.Label
        Me.Back = New System.Windows.Forms.Button
        Me.GroupBox2 = New System.Windows.Forms.GroupBox
        Me.Button2 = New System.Windows.Forms.Button
        Me.Button1 = New System.Windows.Forms.Button
        Me.cListBox = New System.Windows.Forms.CheckedListBox
        Me.Results = New System.Windows.Forms.Label
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(-3, 510)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(799, 13)
        Me.Label10.TabIndex = 36
        Me.Label10.Text = resources.GetString("Label10.Text")
        '
        'Main
        '
        Me.Main.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Main.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Main.Location = New System.Drawing.Point(15, 525)
        Me.Main.Name = "Main"
        Me.Main.Size = New System.Drawing.Size(105, 42)
        Me.Main.TabIndex = 35
        Me.Main.Text = "To Main"
        Me.Main.UseVisualStyleBackColor = True
        '
        'Browse
        '
        Me.Browse.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Browse.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Browse.Location = New System.Drawing.Point(670, 525)
        Me.Browse.Name = "Browse"
        Me.Browse.Size = New System.Drawing.Size(105, 42)
        Me.Browse.TabIndex = 34
        Me.Browse.Text = "Search"
        Me.Browse.UseVisualStyleBackColor = True
        '
        'Wishlist
        '
        Me.Wishlist.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Wishlist.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Wishlist.Location = New System.Drawing.Point(560, 525)
        Me.Wishlist.Name = "Wishlist"
        Me.Wishlist.Size = New System.Drawing.Size(105, 42)
        Me.Wishlist.TabIndex = 33
        Me.Wishlist.Text = "Wishlist"
        Me.Wishlist.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Impact", 64.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.ForeColor = System.Drawing.SystemColors.HotTrack
        Me.Label1.Location = New System.Drawing.Point(330, -10)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(465, 105)
        Me.Label1.TabIndex = 28
        Me.Label1.Text = "*Bookstore*"
        '
        'Back
        '
        Me.Back.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Back.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Back.Location = New System.Drawing.Point(125, 525)
        Me.Back.Name = "Back"
        Me.Back.Size = New System.Drawing.Size(105, 42)
        Me.Back.TabIndex = 32
        Me.Back.Text = "Back"
        Me.Back.UseVisualStyleBackColor = True
        '
        'GroupBox2
        '
        Me.GroupBox2.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.GroupBox2.Controls.Add(Me.Button2)
        Me.GroupBox2.Controls.Add(Me.Button1)
        Me.GroupBox2.Controls.Add(Me.cListBox)
        Me.GroupBox2.Location = New System.Drawing.Point(15, 138)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(766, 368)
        Me.GroupBox2.TabIndex = 31
        Me.GroupBox2.TabStop = False
        '
        'Button2
        '
        Me.Button2.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button2.Font = New System.Drawing.Font("Impact", 14.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button2.Location = New System.Drawing.Point(19, 285)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(171, 42)
        Me.Button2.TabIndex = 33
        Me.Button2.Text = "Check/Uncheck All"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Button1
        '
        Me.Button1.Font = New System.Drawing.Font("Impact", 36.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button1.Location = New System.Drawing.Point(545, 285)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(206, 67)
        Me.Button1.TabIndex = 1
        Me.Button1.Text = "Browse!"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'cListBox
        '
        Me.cListBox.FormattingEnabled = True
        Me.cListBox.Location = New System.Drawing.Point(7, 20)
        Me.cListBox.Name = "cListBox"
        Me.cListBox.Size = New System.Drawing.Size(752, 259)
        Me.cListBox.TabIndex = 0
        '
        'Results
        '
        Me.Results.AutoSize = True
        Me.Results.Font = New System.Drawing.Font("Impact", 36.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Results.Location = New System.Drawing.Point(12, 75)
        Me.Results.Name = "Results"
        Me.Results.Size = New System.Drawing.Size(322, 60)
        Me.Results.TabIndex = 29
        Me.Results.Text = "Browse Genre:"
        '
        'BrowseGenre
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(792, 566)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.Main)
        Me.Controls.Add(Me.Browse)
        Me.Controls.Add(Me.Wishlist)
        Me.Controls.Add(Me.Back)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.Results)
        Me.Controls.Add(Me.Label1)
        Me.Name = "BrowseGenre"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "BrowseGenre"
        Me.GroupBox2.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents Main As System.Windows.Forms.Button
    Friend WithEvents Browse As System.Windows.Forms.Button
    Friend WithEvents Wishlist As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Back As System.Windows.Forms.Button
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents Results As System.Windows.Forms.Label
    Friend WithEvents cListBox As System.Windows.Forms.CheckedListBox
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Button2 As System.Windows.Forms.Button
End Class

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class ViewBook
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(ViewBook))
        Me.Browse = New System.Windows.Forms.Button
        Me.Wishlist = New System.Windows.Forms.Button
        Me.Label10 = New System.Windows.Forms.Label
        Me.Results = New System.Windows.Forms.Label
        Me.Main = New System.Windows.Forms.Button
        Me.Button2 = New System.Windows.Forms.Button
        Me.Button1 = New System.Windows.Forms.Button
        Me.Label1 = New System.Windows.Forms.Label
        Me.Back = New System.Windows.Forms.Button
        Me.GroupBox2 = New System.Windows.Forms.GroupBox
        Me.Genre = New System.Windows.Forms.Label
        Me.Author = New System.Windows.Forms.Label
        Me.Title = New System.Windows.Forms.Label
        Me.RichTextBox1 = New System.Windows.Forms.RichTextBox
        Me.GroupBox1 = New System.Windows.Forms.GroupBox
        Me.Button3 = New System.Windows.Forms.Button
        Me.Label2 = New System.Windows.Forms.Label
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Browse
        '
        Me.Browse.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Browse.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Browse.Location = New System.Drawing.Point(670, 525)
        Me.Browse.Name = "Browse"
        Me.Browse.Size = New System.Drawing.Size(105, 42)
        Me.Browse.TabIndex = 34
        Me.Browse.Text = "Browse"
        Me.Browse.UseVisualStyleBackColor = True
        '
        'Wishlist
        '
        Me.Wishlist.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Wishlist.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Wishlist.Location = New System.Drawing.Point(448, 525)
        Me.Wishlist.Name = "Wishlist"
        Me.Wishlist.Size = New System.Drawing.Size(105, 42)
        Me.Wishlist.TabIndex = 33
        Me.Wishlist.Text = "Wishlist"
        Me.Wishlist.UseVisualStyleBackColor = True
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
        'Results
        '
        Me.Results.AutoSize = True
        Me.Results.Font = New System.Drawing.Font("Impact", 36.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Results.Location = New System.Drawing.Point(21, 79)
        Me.Results.Name = "Results"
        Me.Results.Size = New System.Drawing.Size(246, 60)
        Me.Results.TabIndex = 29
        Me.Results.Text = "View Book:"
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
        'Button2
        '
        Me.Button2.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button2.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button2.Location = New System.Drawing.Point(391, 19)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(185, 36)
        Me.Button2.TabIndex = 30
        Me.Button2.Text = "Add to Wishlist"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Button1
        '
        Me.Button1.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button1.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button1.Location = New System.Drawing.Point(223, 19)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(162, 36)
        Me.Button1.TabIndex = 29
        Me.Button1.Text = "Purchase"
        Me.Button1.UseVisualStyleBackColor = True
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
        Me.GroupBox2.Controls.Add(Me.Genre)
        Me.GroupBox2.Controls.Add(Me.Author)
        Me.GroupBox2.Controls.Add(Me.Title)
        Me.GroupBox2.Controls.Add(Me.RichTextBox1)
        Me.GroupBox2.Location = New System.Drawing.Point(15, 142)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(766, 297)
        Me.GroupBox2.TabIndex = 31
        Me.GroupBox2.TabStop = False
        '
        'Genre
        '
        Me.Genre.AutoSize = True
        Me.Genre.Font = New System.Drawing.Font("Impact", 24.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Genre.Location = New System.Drawing.Point(308, 163)
        Me.Genre.Name = "Genre"
        Me.Genre.Size = New System.Drawing.Size(92, 39)
        Me.Genre.TabIndex = 3
        Me.Genre.Text = "Title:  "
        '
        'Author
        '
        Me.Author.AutoSize = True
        Me.Author.Font = New System.Drawing.Font("Impact", 24.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Author.Location = New System.Drawing.Point(308, 109)
        Me.Author.Name = "Author"
        Me.Author.Size = New System.Drawing.Size(92, 39)
        Me.Author.TabIndex = 2
        Me.Author.Text = "Title:  "
        '
        'Title
        '
        Me.Title.AutoSize = True
        Me.Title.Font = New System.Drawing.Font("Impact", 24.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Title.Location = New System.Drawing.Point(308, 50)
        Me.Title.Name = "Title"
        Me.Title.Size = New System.Drawing.Size(92, 39)
        Me.Title.TabIndex = 1
        Me.Title.Text = "Title:  "
        '
        'RichTextBox1
        '
        Me.RichTextBox1.BackColor = System.Drawing.SystemColors.ButtonShadow
        Me.RichTextBox1.Location = New System.Drawing.Point(87, 40)
        Me.RichTextBox1.Name = "RichTextBox1"
        Me.RichTextBox1.ReadOnly = True
        Me.RichTextBox1.Size = New System.Drawing.Size(165, 201)
        Me.RichTextBox1.TabIndex = 0
        Me.RichTextBox1.Text = "Image Not Found"
        '
        'GroupBox1
        '
        Me.GroupBox1.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.Button1)
        Me.GroupBox1.Controls.Add(Me.Button2)
        Me.GroupBox1.Location = New System.Drawing.Point(15, 445)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(766, 65)
        Me.GroupBox1.TabIndex = 30
        Me.GroupBox1.TabStop = False
        '
        'Button3
        '
        Me.Button3.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button3.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button3.Location = New System.Drawing.Point(559, 525)
        Me.Button3.Name = "Button3"
        Me.Button3.Size = New System.Drawing.Size(105, 42)
        Me.Button3.TabIndex = 37
        Me.Button3.Text = "Search"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.ForeColor = System.Drawing.SystemColors.AppWorkspace
        Me.Label2.Location = New System.Drawing.Point(176, 36)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(41, 13)
        Me.Label2.TabIndex = 38
        Me.Label2.Text = "Broken"
        '
        'ViewBook
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(792, 566)
        Me.Controls.Add(Me.Button3)
        Me.Controls.Add(Me.Results)
        Me.Controls.Add(Me.Browse)
        Me.Controls.Add(Me.Wishlist)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.Main)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Back)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.Name = "ViewBook"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "ViewBook"
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Browse As System.Windows.Forms.Button
    Friend WithEvents Wishlist As System.Windows.Forms.Button
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents Results As System.Windows.Forms.Label
    Friend WithEvents Main As System.Windows.Forms.Button
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Back As System.Windows.Forms.Button
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents RichTextBox1 As System.Windows.Forms.RichTextBox
    Friend WithEvents Genre As System.Windows.Forms.Label
    Friend WithEvents Author As System.Windows.Forms.Label
    Friend WithEvents Title As System.Windows.Forms.Label
    Friend WithEvents Button3 As System.Windows.Forms.Button
    Friend WithEvents Label2 As System.Windows.Forms.Label
End Class

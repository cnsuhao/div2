<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class BrowseResults
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
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(BrowseResults))
        Me.Label10 = New System.Windows.Forms.Label
        Me.Main = New System.Windows.Forms.Button
        Me.Browse = New System.Windows.Forms.Button
        Me.Wishlist = New System.Windows.Forms.Button
        Me.Label1 = New System.Windows.Forms.Label
        Me.Back = New System.Windows.Forms.Button
        Me.GroupBox2 = New System.Windows.Forms.GroupBox
        Me.DataGridView1 = New System.Windows.Forms.DataGridView
        Me.Button3 = New System.Windows.Forms.Button
        Me.Button2 = New System.Windows.Forms.Button
        Me.Button1 = New System.Windows.Forms.Button
        Me.Button4 = New System.Windows.Forms.Button
        Me.GroupBox1 = New System.Windows.Forms.GroupBox
        Me.Button5 = New System.Windows.Forms.Button
        Me.Results = New System.Windows.Forms.Label
        Me.bs = New System.Windows.Forms.BindingSource(Me.components)
        Me.Label2 = New System.Windows.Forms.Label
        Me.GroupBox2.SuspendLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox1.SuspendLayout()
        CType(Me.bs, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(-3, 510)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(799, 13)
        Me.Label10.TabIndex = 27
        Me.Label10.Text = resources.GetString("Label10.Text")
        '
        'Main
        '
        Me.Main.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Main.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Main.Location = New System.Drawing.Point(15, 525)
        Me.Main.Name = "Main"
        Me.Main.Size = New System.Drawing.Size(105, 42)
        Me.Main.TabIndex = 26
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
        Me.Browse.TabIndex = 25
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
        Me.Wishlist.TabIndex = 24
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
        Me.Label1.TabIndex = 19
        Me.Label1.Text = "*Bookstore*"
        '
        'Back
        '
        Me.Back.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Back.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Back.Location = New System.Drawing.Point(125, 525)
        Me.Back.Name = "Back"
        Me.Back.Size = New System.Drawing.Size(105, 42)
        Me.Back.TabIndex = 23
        Me.Back.Text = "Back"
        Me.Back.UseVisualStyleBackColor = True
        '
        'GroupBox2
        '
        Me.GroupBox2.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.GroupBox2.Controls.Add(Me.Label2)
        Me.GroupBox2.Controls.Add(Me.DataGridView1)
        Me.GroupBox2.Controls.Add(Me.Button3)
        Me.GroupBox2.Controls.Add(Me.Button2)
        Me.GroupBox2.Controls.Add(Me.Button1)
        Me.GroupBox2.Location = New System.Drawing.Point(15, 137)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(766, 297)
        Me.GroupBox2.TabIndex = 22
        Me.GroupBox2.TabStop = False
        '
        'DataGridView1
        '
        Me.DataGridView1.AllowUserToAddRows = False
        Me.DataGridView1.AllowUserToDeleteRows = False
        Me.DataGridView1.AllowUserToResizeRows = False
        Me.DataGridView1.BackgroundColor = System.Drawing.SystemColors.ControlLightLight
        Me.DataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridView1.Location = New System.Drawing.Point(7, 19)
        Me.DataGridView1.MultiSelect = False
        Me.DataGridView1.Name = "DataGridView1"
        Me.DataGridView1.ReadOnly = True
        Me.DataGridView1.RowHeadersVisible = False
        Me.DataGridView1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        Me.DataGridView1.Size = New System.Drawing.Size(750, 225)
        Me.DataGridView1.TabIndex = 28
        '
        'Button3
        '
        Me.Button3.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button3.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button3.Location = New System.Drawing.Point(474, 250)
        Me.Button3.Name = "Button3"
        Me.Button3.Size = New System.Drawing.Size(162, 36)
        Me.Button3.TabIndex = 31
        Me.Button3.Text = "View Book"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Me.Button2.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button2.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button2.Location = New System.Drawing.Point(283, 250)
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
        Me.Button1.Location = New System.Drawing.Point(115, 250)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(162, 36)
        Me.Button1.TabIndex = 29
        Me.Button1.Text = "Purchase"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Button4
        '
        Me.Button4.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button4.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button4.Location = New System.Drawing.Point(393, 19)
        Me.Button4.Name = "Button4"
        Me.Button4.Size = New System.Drawing.Size(162, 36)
        Me.Button4.TabIndex = 20
        Me.Button4.Text = "Browse All"
        Me.Button4.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        Me.GroupBox1.BackColor = System.Drawing.SystemColors.ButtonHighlight
        Me.GroupBox1.Controls.Add(Me.Button5)
        Me.GroupBox1.Controls.Add(Me.Button4)
        Me.GroupBox1.Location = New System.Drawing.Point(15, 440)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(766, 65)
        Me.GroupBox1.TabIndex = 21
        Me.GroupBox1.TabStop = False
        '
        'Button5
        '
        Me.Button5.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button5.Font = New System.Drawing.Font("Impact", 18.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button5.Location = New System.Drawing.Point(225, 19)
        Me.Button5.Name = "Button5"
        Me.Button5.Size = New System.Drawing.Size(162, 36)
        Me.Button5.TabIndex = 21
        Me.Button5.Text = "Browse By. . ."
        Me.Button5.UseVisualStyleBackColor = True
        '
        'Results
        '
        Me.Results.AutoSize = True
        Me.Results.Font = New System.Drawing.Font("Impact", 36.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Results.Location = New System.Drawing.Point(12, 74)
        Me.Results.Name = "Results"
        Me.Results.Size = New System.Drawing.Size(365, 60)
        Me.Results.TabIndex = 20
        Me.Results.Text = "Browse Results: "
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.ForeColor = System.Drawing.SystemColors.AppWorkspace
        Me.Label2.Location = New System.Drawing.Point(68, 267)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(41, 13)
        Me.Label2.TabIndex = 38
        Me.Label2.Text = "Broken"
        '
        'BrowseResults
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(792, 566)
        Me.Controls.Add(Me.Results)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.Main)
        Me.Controls.Add(Me.Browse)
        Me.Controls.Add(Me.Wishlist)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Back)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.Name = "BrowseResults"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "BrowseResluts"
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox1.ResumeLayout(False)
        CType(Me.bs, System.ComponentModel.ISupportInitialize).EndInit()
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
    Friend WithEvents Button4 As System.Windows.Forms.Button
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents Results As System.Windows.Forms.Label
    Friend WithEvents Button3 As System.Windows.Forms.Button
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Button5 As System.Windows.Forms.Button
    Friend WithEvents DataGridView1 As System.Windows.Forms.DataGridView
    Friend WithEvents bs As System.Windows.Forms.BindingSource
    Friend WithEvents Label2 As System.Windows.Forms.Label
End Class

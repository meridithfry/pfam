<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class HelpInfo
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
        Me.HelpTextBox = New System.Windows.Forms.RichTextBox
        Me.SuspendLayout()
        '
        'HelpTextBox
        '
        Me.HelpTextBox.BackColor = System.Drawing.SystemColors.Control
        Me.HelpTextBox.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.HelpTextBox.Location = New System.Drawing.Point(13, 12)
        Me.HelpTextBox.Margin = New System.Windows.Forms.Padding(4)
        Me.HelpTextBox.Name = "HelpTextBox"
        Me.HelpTextBox.Size = New System.Drawing.Size(633, 536)
        Me.HelpTextBox.TabIndex = 2
        Me.HelpTextBox.Text = ""
        '
        'HelpInfo
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(659, 576)
        Me.Controls.Add(Me.HelpTextBox)
        Me.Name = "HelpInfo"
        Me.Text = "HelpInfo"
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents HelpTextBox As System.Windows.Forms.RichTextBox
End Class

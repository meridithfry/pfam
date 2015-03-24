Public Class HelpInfo

    Private Sub HelpInfo_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim AppPath As String
        AppPath = My.Application.Info.DirectoryPath()

        Try
            HelpTextBox.LoadFile(AppPath & "\PFAMmanual.rtf")
        Catch ex As Exception
            HelpTextBox.Text = "help unavailable"
        End Try
    End Sub
End Class
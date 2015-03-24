Module Module_ErrorTests

    'Function testIntegers(ByVal name As TextBox) As Integer
    '    Dim TestNumber As Integer
    '    testIntegers = 1

    '    Try
    '        TestNumber = name.Text
    '    Catch ex As Exception
    '        MsgBox("Check the value for " & name.Tag)
    '        Return 0
    '    End Try

    'End Function

    'Function testRealNumbers(ByVal name As TextBox) As Integer
    '    Dim TestNumber As Double
    '    testRealNumbers = 1

    '    Try
    '        TestNumber = name.Text
    '    Catch ex As Exception
    '        MsgBox("Check the value for " & name.Tag)
    '        Return 0
    '    End Try

    'End Function

    'Sub zeroCheck(ByVal name As TextBox)
    '    Dim MaxHalfLifeMessage As String
    '    MaxHalfLifeMessage = "A zero Half Life in PFAM is a flag for a stable compound. Default half Life is 1e8 for "

    '    If name.Text = 0 Then
    '        name.Text = "1e8"
    '        MsgBox(MaxHalfLifeMessage & name.Tag)
    '    End If
    'End Sub

    'Function calendarCheck(ByVal dayBox As TextBox, ByVal monthBox As TextBox) As Integer

    '    Dim monthtest As Integer
    '    Dim daytest As Integer

    '    calendarCheck = 0

    '    Try
    '        monthtest = Convert.ToInt16(monthBox.Text)
    '    Catch ex As Exception
    '        MsgBox(ex.Message & "  Check " & monthBox.Tag)
    '        Return 0
    '    End Try

    '    Try
    '        daytest = Convert.ToInt16(dayBox.Text)
    '    Catch ex As Exception
    '        MsgBox(ex.Message & " Check " & dayBox.Tag)
    '        Return 0
    '    End Try

    '    If monthtest > 12 Or monthtest < 1 Then
    '        MsgBox("The following month is not posssible: " & monthBox.Tag)
    '        Return 0
    '    End If

    '    Select Case monthtest

    '        Case 1, 3, 5, 7, 8, 10, 12
    '            If daytest > 31 Or daytest < 1 Then
    '                MsgBox("Bad day for " & dayBox.Tag)
    '                Return 0
    '            End If
    '        Case 2
    '            If daytest > 28 Or daytest < 1 Then
    '                MsgBox("Bad day for " & dayBox.Tag)
    '                Return 0
    '            End If
    '        Case 4, 6, 9, 11
    '            If daytest > 30 Or daytest < 1 Then
    '                MsgBox("Bad day for " & dayBox.Tag)
    '                Return 0
    '            End If
    '        Case Else
    '            MsgBox("Month does not exist on Earth calendar for " & monthBox.Tag)
    '            Return 0
    '    End Select

    '    calendarCheck = 1
    'End Function
















End Module

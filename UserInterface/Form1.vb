
Public Class Form1

    Private Sub SaveInputToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveInputToolStripMenuItem.Click
        'Save Input menu item.  Gets inputs from user interface and records them into a text file line by line.  One item per line.
        Dim result As System.Windows.Forms.DialogResult
        SaveFileDialog1.Filter = "PFAM files (*.PFA)|*.PFA"

        SaveFileDialog1.InitialDirectory = workingdirectory.Text
        SaveFileDialog1.FileName = ""


        result = SaveFileDialog1.ShowDialog(Me)

        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If
        workingdirectory.Text = System.IO.Path.GetDirectoryName(SaveFileDialog1.FileName) & "\"


        If SaveFileDialog1.FileName <> "" Then
            Dim msg As String
            Try 'open file and trap any errors using handler
                msg = constructInputString()
                Try
                    'System.IO.Directory.SetCurrentDirectory(workingDirectory.Text)
                    My.Computer.FileSystem.WriteAllText(SaveFileDialog1.FileName, msg, False, System.Text.Encoding.ASCII)
                Catch ex As Exception
                    MsgBox(ex.Message)
                End Try

            Catch ex As Exception
                MsgBox(ex.Message & " There is a problem saving this info to file.")
            Finally
                FileClose(1) 'close file
            End Try


        End If




    End Sub

    Private Sub RetrieveInputToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RetrieveInputToolStripMenuItem.Click
        Dim result As System.Windows.Forms.DialogResult
        Dim currentrow As String()

        OpenFileDialog1.Filter = "PFAM Text files (*.PFA)|*.PFA|All files (*.*)|*.*"


        OpenFileDialog1.InitialDirectory = workingdirectory.Text
        OpenFileDialog1.FileName = ""


        result = OpenFileDialog1.ShowDialog() 'display Open dialog box
        'Cancel button will cause return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        workingdirectory.Text = System.IO.Path.GetDirectoryName(OpenFileDialog1.FileName) & "\"


        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(OpenFileDialog1.FileName)
            MyReader.TextFieldType = FileIO.FieldType.Delimited
            MyReader.SetDelimiters(",")

            Try
                '************ Chemical Properties **********
                TB_aerobic.Text = MyReader.ReadLine
                TB_benthic.Text = MyReader.ReadLine
                TB_unflooded.Text = MyReader.ReadLine

                TB_photolysis.Text = MyReader.ReadLine
                TB_hydrolysis.Text = MyReader.ReadLine
                TB_MWT.Text = MyReader.ReadLine
                TB_VP.Text = MyReader.ReadLine
                TB_SOL.Text = MyReader.ReadLine
                TB_Koc.Text = MyReader.ReadLine

                TB_AerTemp.Text = MyReader.ReadLine
                TB_BenthicTemp.Text = MyReader.ReadLine
                TB_UnfloodTemp.Text = MyReader.ReadLine
                TB_PhotoLat.Text = MyReader.ReadLine

                TB_enthalpy.Text = MyReader.ReadLine
                TB_henrytemp.Text = MyReader.ReadLine

                '********* Pesticide Applications ****************
                TB_AppNumber.Text = MyReader.ReadLine
                updateApplications()

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To ApplicationInfo.maxApplications - 1
                    ApplicationInfo.day(i).Text = currentrow(i)
                Next

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To ApplicationInfo.maxApplications - 1
                    ApplicationInfo.month(i).Text = currentrow(i)
                Next

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To ApplicationInfo.maxApplications - 1
                    ApplicationInfo.rate(i).Text = currentrow(i)
                Next

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To ApplicationInfo.maxApplications - 1
                    ApplicationInfo.slowRelease(i).Text = currentrow(i)
                Next

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To ApplicationInfo.maxApplications - 1
                    ApplicationInfo.drift(i).Text = currentrow(i)
                Next

                MyReader.ReadLine() 'Spoare Lines
                MyReader.ReadLine()

                '*********** Location ******************
                TB_metfile.Text = MyReader.ReadLine
                TB_Latitude.Text = MyReader.ReadLine

                '******** Flood Control ***************
                Dim floodnumber As Integer
                floodnumber = MyReader.ReadLine
                TB_FloodNumber.Text = floodnumber
                UpdateFlood()

                FloodDayZero.Text = MyReader.ReadLine
                FloodMonthZero.Text = MyReader.ReadLine

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To floodnumber - 1
                    FloodInfo.day(i).Text = currentrow(i)
                Next

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To floodnumber - 1
                    FloodInfo.fill(i).Text = currentrow(i)
                Next

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To floodnumber - 1
                    FloodInfo.wier(i).Text = currentrow(i)
                Next

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To floodnumber - 1
                    FloodInfo.min(i).Text = currentrow(i)
                Next

                currentrow = MyReader.ReadFields
                For i As Integer = 0 To floodnumber - 1
                    FloodInfo.turnOver(i).Text = currentrow(i)
                Next


                '**** CROP CHARACTERISTICS ********************
                CropDayZero.Text = MyReader.ReadLine
                CropMonthZero.Text = MyReader.ReadLine
                TB_DaysCropMax.Text = MyReader.ReadLine
                TB_DaysCropRemove.Text = MyReader.ReadLine
                TB_CropMaxArea.Text = MyReader.ReadLine

                '**** Physical Inputs********************
                TB_MassTransfer.Text = MyReader.ReadLine
                TB_RefDepth.Text = MyReader.ReadLine
                TB_benthicdepth.Text = MyReader.ReadLine
                TB_pctwa.Text = MyReader.ReadLine
                TB_bulkd.Text = MyReader.ReadLine
                TB_froc1.Text = MyReader.ReadLine
                TB_froc2.Text = MyReader.ReadLine
                TB_sused.Text = MyReader.ReadLine
                TB_chl.Text = MyReader.ReadLine
                TB_doc1.Text = MyReader.ReadLine

                Dim dummy As String
                TB_WierLeakage.Text = MyReader.ReadLine
                dummy = MyReader.ReadLine
                dummy = MyReader.ReadLine
                TB_qt.Text = MyReader.ReadLine
                TB_dfac.Text = MyReader.ReadLine
                TB_area.Text = MyReader.ReadLine
                TB_Leakage.Text = MyReader.ReadLine

                '****** Output File ***************
                TB_OutputDirectory.Text = MyReader.ReadLine
                TB_outputfile.Text = MyReader.ReadLine

                '*********Number of Degradates********************
                Dim NumberOfChemicals As Integer
                NumberOfChemicals = MyReader.ReadLine
                Select Case NumberOfChemicals
                    Case 1
                        CalcDegradate1.Checked = False
                        CalcDegradate2.Checked = False
                    Case 2
                        CalcDegradate1.Checked = True
                        CalcDegradate2.Checked = False
                    Case 3
                        CalcDegradate1.Checked = True
                        CalcDegradate2.Checked = True
                End Select

                TB_aerobic1.Text = MyReader.ReadLine
                TB_benthic1.Text = MyReader.ReadLine
                TB_unflooded1.Text = MyReader.ReadLine
                TB_photolysis1.Text = MyReader.ReadLine
                TB_hydrolysis1.Text = MyReader.ReadLine
                TB_MWT1.Text = MyReader.ReadLine
                TB_VP1.Text = MyReader.ReadLine
                TB_SOL1.Text = MyReader.ReadLine
                TB_Koc1.Text = MyReader.ReadLine
                TB_AerTemp1.Text = MyReader.ReadLine
                TB_benthicTemp1.Text = MyReader.ReadLine
                TB_UnfloodTemp1.Text = MyReader.ReadLine
                TB_PhotoLat1.Text = MyReader.ReadLine
                TB_enthalpy1.Text = MyReader.ReadLine
                TB_henryTemp1.Text = MyReader.ReadLine
                TB_aerobicXform1.Text = MyReader.ReadLine
                TB_benthicXform1.Text = MyReader.ReadLine
                TB_unfloodXform1.Text = MyReader.ReadLine
                TB_photXform1.Text = MyReader.ReadLine
                TB_hydroXform1.Text = MyReader.ReadLine

                TB_aerobic2.Text = MyReader.ReadLine
                TB_benthic2.Text = MyReader.ReadLine
                TB_unflooded2.Text = MyReader.ReadLine
                TB_photolysis2.Text = MyReader.ReadLine
                TB_hydrolysis2.Text = MyReader.ReadLine
                TB_MWT2.Text = MyReader.ReadLine
                TB_VP2.Text = MyReader.ReadLine
                TB_SOL2.Text = MyReader.ReadLine
                TB_Koc2.Text = MyReader.ReadLine
                TB_AerTemp2.Text = MyReader.ReadLine
                TB_benthicTemp2.Text = MyReader.ReadLine
                TB_UnfloodTemp2.Text = MyReader.ReadLine
                TB_PhotoLat2.Text = MyReader.ReadLine
                TB_enthalpy2.Text = MyReader.ReadLine
                TB_henryTemp2.Text = MyReader.ReadLine
                TB_aerobicXform2.Text = MyReader.ReadLine
                TB_benthicXform2.Text = MyReader.ReadLine
                TB_unfloodXform2.Text = MyReader.ReadLine
                TB_photXform2.Text = MyReader.ReadLine
                TB_hydroXform2.Text = MyReader.ReadLine
                VVWMCheckBox.Checked = MyReader.ReadLine
            Catch ex As Exception
                StatusLabel.ForeColor = Color.Red
                MsgBox(ex.Message & ";  It is possible that some data area missing.  Check your inputs.")
            End Try

            Try
                TB_AreaWatershed.Text = MyReader.ReadLine
                TB_WatershedCN.Text = MyReader.ReadLine

                TB_widthMixingCell.Text = MyReader.ReadLine
                TB_depthMixingCell.Text = MyReader.ReadLine
                TB_lengthMixingCell.Text = MyReader.ReadLine
                TB_baseFlow.Text = MyReader.ReadLine
            Catch ex As Exception
                If VVWMCheckBox.Checked Then
                    MsgBox(ex.Message & ";  It is possible that recieving water body info is missing.  Check your inputs.")
                End If
            End Try
        End Using


        StatusLabel.ForeColor = Color.Blue
        StatusLabel.Text = ("Ready...")

    End Sub

    Private Sub Button_weather_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button_weather.Click
        Dim result As System.Windows.Forms.DialogResult

        OpenFileDialog2.Filter = "DVF files (*.DVF)|*.DVF"
        result = OpenFileDialog2.ShowDialog() 'display Open dialog box

        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        TB_metfile.Text = OpenFileDialog2.FileName

    End Sub

    Private Sub UpdateApp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UpdateApp.Click
        updateApplications()
    End Sub

    Sub updateApplications()
        Dim ApplicationNumber As Integer

        Try
            ApplicationNumber = TB_AppNumber.Text
        Catch ex As Exception
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("Application Number must be an integer")
            Beep()
            ApplicationNumber = 1
        End Try

        'convert real numbers to integers:
        TB_AppNumber.Text = ApplicationNumber



        If ApplicationNumber > ApplicationInfo.maxApplications Then
            ApplicationNumber = ApplicationInfo.maxApplications
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = String.Format("Application Number maximum is {0}", ApplicationInfo.maxApplications)
            Beep()
            TB_AppNumber.Text = ApplicationInfo.maxApplications
        End If

        If ApplicationNumber < 1 Then
            ApplicationNumber = 1
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("Application Number doesnt make sense.  Need at least 1 application")
            Beep()
            TB_AppNumber.Text = 1
        End If


        For i As Integer = 0 To ApplicationNumber - 1
            ApplicationInfo.day(i).Visible = True
            ApplicationInfo.month(i).Visible = True
            ApplicationInfo.rate(i).Visible = True
            ApplicationInfo.appLabel(i).Visible = True
            ApplicationInfo.slowRelease(i).Visible = True
            ApplicationInfo.drift(i).Visible = True
        Next



        For i As Integer = ApplicationNumber To ApplicationInfo.maxApplications - 1
            ApplicationInfo.day(i).Visible = False
            ApplicationInfo.month(i).Visible = False
            ApplicationInfo.rate(i).Visible = False
            ApplicationInfo.appLabel(i).Visible = False
            ApplicationInfo.slowRelease(i).Visible = False
            ApplicationInfo.drift(i).Visible = False
        Next



    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        UpdateFlood()
    End Sub

    Sub UpdateFlood()

        Dim FloodNumber As Integer

        Try
            FloodNumber = TB_FloodNumber.Text
        Catch ex As Exception
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("Number of Flood Events must be an integer")
            Beep()
            FloodNumber = 1
        End Try

        'convert real numbers to integers:
        TB_FloodNumber.Text = FloodNumber

        If FloodNumber > FloodInfo.maxFloodEvents Then
            FloodNumber = FloodInfo.maxFloodEvents
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = String.Format("Number of Flood Events maximum is {0}", FloodInfo.maxFloodEvents)
            Beep()
            TB_FloodNumber.Text = FloodInfo.maxFloodEvents
        End If

        If FloodNumber < 1 Then
            FloodNumber = 1
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("Flood Number doesnt make sence.  Need at least 1 Flood")
            Beep()
            TB_FloodNumber.Text = 1
        End If


        For i As Integer = 0 To FloodNumber - 1

            FloodInfo.eventNumber(i).Visible = True
            FloodInfo.day(i).Visible = True
            FloodInfo.fill(i).Visible = True
            FloodInfo.wier(i).Visible = True
            FloodInfo.min(i).Visible = True
            FloodInfo.turnOver(i).Visible = True
        Next

        For i As Integer = FloodNumber To FloodInfo.maxFloodEvents - 1
            FloodInfo.eventNumber(i).Visible = False
            FloodInfo.day(i).Visible = False
            FloodInfo.fill(i).Visible = False
            FloodInfo.wier(i).Visible = False
            FloodInfo.min(i).Visible = False
            FloodInfo.turnOver(i).Visible = False
        Next

    End Sub

    Private Sub Button_OutputFileBrowse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button_OutputFileBrowse.Click

        Dim result As System.Windows.Forms.DialogResult

        SaveFileDialog2.Filter = "Text files (*.TXT)|*.TXT"
        result = SaveFileDialog2.ShowDialog() 'display Open dialog box

        'Cancel button will cuase return without further execution
        If result = Windows.Forms.DialogResult.Cancel Then
            Return
        End If

        TB_OutputDirectory.Text = System.IO.Path.GetDirectoryName(SaveFileDialog2.FileName) & "\"
        TB_outputfile.Text = System.IO.Path.GetFileName(SaveFileDialog2.FileName)

    End Sub

    Private Sub Button_Run_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button_Run.Click

        Dim AppPath As String
        Dim XFERfile As String
        Dim outputFile As String

        UpdateFlood()
        updateApplications()

        If CheckValues() = 0 Then
            'StatusLabel.Text = "Problem, fix it and try again..."
            'StatusLabel.ForeColor = Color.Chocolate
            Return 'Signifies problem with input. CheckValues functiom does just that.
        End If

        'Clear any old transfer files in the directory
        IO.File.Delete(TB_OutputDirectory.Text & "PFAMtransferfile.xxx")

        AppPath = My.Application.Info.DirectoryPath()
        'XFERfile = AppPath & "\PFAMtransferfile.xxx"

        XFERfile = TB_OutputDirectory.Text & "PFAMtransferfile.xxx"

        writeXFERfile(XFERfile)  'create the transfer file and write the data

        Try
            Dim startInfo As New System.Diagnostics.ProcessStartInfo(AppPath & "\PFAMfortran.exe", """" & XFERfile & """")
            startInfo.WindowStyle = ProcessWindowStyle.Hidden
            System.Diagnostics.Process.Start(startInfo).WaitForExit()

            'System.Diagnostics.Process.Start(AppPath & "\PFAMfortran.exe", """" & XFERfile & """").WaitForExit()
        Catch ex As Exception
            MsgBox(ex.Message & "Possible problem finding PFAMfortran.exe or the transfer file")
        End Try

        outputFile = TB_OutputDirectory.Text & TB_outputfile.Text

        Try
            Dim startInfo2 As New System.Diagnostics.ProcessStartInfo(AppPath & "\PFAMpp1.exe", """" & outputFile & """")
            startInfo2.WindowStyle = ProcessWindowStyle.Hidden
            System.Diagnostics.Process.Start(startInfo2).WaitForExit()
        Catch ex As Exception
            MsgBox(ex.Message & "Possible problem finding PFAMpp1.exe.exe or its argument")
        End Try

        'Here is another Post Processor that will function if it exists in the Application Directory
        If VVWMCheckBox.Checked = True Then
            If System.IO.File.Exists(AppPath & "\VVWM_PFAM.exe") Then
                Try
                    Dim startInfo3 As New System.Diagnostics.ProcessStartInfo(AppPath & "\VVWM_PFAM.exe", """" & outputFile & """")
                    startInfo3.WindowStyle = ProcessWindowStyle.Hidden
                    System.Diagnostics.Process.Start(startInfo3).WaitForExit()
                Catch ex As Exception
                    MsgBox(ex.Message)
                End Try
            End If
        End If

        'Here is another Post Processor that will function if it exists in the Application Directory
        If System.IO.File.Exists(AppPath & "\PFAMpp2.exe") Then
            Try
                Dim startInfo3 As New System.Diagnostics.ProcessStartInfo(AppPath & "\PFAMpp2.exe", """" & outputFile & """")
                startInfo3.WindowStyle = ProcessWindowStyle.Hidden
                System.Diagnostics.Process.Start(startInfo3).WaitForExit()
            Catch ex As Exception
                MsgBox(ex.Message)
            End Try
        End If

        'Get rid of the new transfer file
        '  IO.File.Delete(TB_OutputDirectory.Text & "PFAMtransferfile.xxx")

        StatusLabel.Text = "Run completed at " & Now
        StatusLabel.ForeColor = Color.Purple

    End Sub

    Sub writeXFERfile(ByVal writefilename As String)
        Dim msg As String
        msg = constructInputString()
        Try
            'System.IO.Directory.SetCurrentDirectory(workingDirectory.Text)
            My.Computer.FileSystem.WriteAllText(writefilename, msg, False, System.Text.Encoding.ASCII)
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub

    Function constructInputString() As String
        Dim msg As String
        msg = ChemInfo()
        msg = msg & vbNewLine & ScenarioInfo()
        msg = msg & vbNewLine & TB_OutputDirectory.Text
        msg = msg & vbNewLine & TB_outputfile.Text
        '****************Degradate Option*************************
        'All inforation in the degradate text boxes is kept regardless of whether it is used or not
        Dim NumberOfChemicals As Integer
        If CalcDegradate1.Checked Then
            NumberOfChemicals = 2
            If CalcDegradate2.Checked Then
                NumberOfChemicals = 3
            End If
        Else
            NumberOfChemicals = 1
        End If

        msg = msg & vbNewLine & NumberOfChemicals
        msg = msg & vbNewLine & DegradateInfo1()
        msg = msg & vbNewLine & DegradateInfo2()

        msg = msg & vbNewLine & PostProcessingInfo()
        '****************************************************
        Return msg

    End Function

    Private Function ChemInfo() As String
        Dim msg As String

        msg = String.Format("{0}", TB_aerobic.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_benthic.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_unflooded.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_photolysis.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_hydrolysis.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_MWT.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_VP.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_SOL.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_Koc.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_AerTemp.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_BenthicTemp.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_UnfloodTemp.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_PhotoLat.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, TB_enthalpy.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_henrytemp.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, TB_AppNumber.Text)


        msg = msg & vbNewLine
        For i As Integer = 0 To ApplicationInfo.maxApplications - 1
            msg = msg & String.Format("{0},", ApplicationInfo.day(i).Text)
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To ApplicationInfo.maxApplications - 1
            msg = msg & String.Format("{0},", ApplicationInfo.month(i).Text)
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To ApplicationInfo.maxApplications - 1
            msg = msg & String.Format("{0},", ApplicationInfo.rate(i).Text)
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To ApplicationInfo.maxApplications - 1
            msg = msg & String.Format("{0},", ApplicationInfo.slowRelease(i).Text)
        Next


        msg = msg & vbNewLine
        For i As Integer = 0 To ApplicationInfo.maxApplications - 1
            msg = msg & String.Format("{0},", ApplicationInfo.drift(i).Text)
        Next

        msg = msg & vbNewLine & "Line For Expansion"
        msg = msg & vbNewLine & "Line For Expansion"

        Return msg

    End Function

    Private Function PostProcessingInfo() As String
        Dim msg As String

            msg = String.Format("{0}", VVWMCheckBox.Checked)
            msg = msg & String.Format("{0}{1}", vbNewLine, TB_AreaWatershed.Text)
            msg = msg & String.Format("{0}{1}", vbNewLine, TB_WatershedCN.Text)
            msg = msg & String.Format("{0}{1}", vbNewLine, TB_widthMixingCell.Text)
            msg = msg & String.Format("{0}{1}", vbNewLine, TB_depthMixingCell.Text)

            msg = msg & String.Format("{0}{1}", vbNewLine, TB_lengthMixingCell.Text)
            msg = msg & String.Format("{0}{1}", vbNewLine, TB_baseFlow.Text)

        Return msg

    End Function

    Private Function DegradateInfo1() As String
        Dim msg As String

        msg = String.Format("{0}", TB_aerobic1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_benthic1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_unflooded1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_photolysis1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_hydrolysis1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_MWT1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_VP1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_SOL1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_Koc1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_AerTemp1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_benthicTemp1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_UnfloodTemp1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_PhotoLat1.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, TB_enthalpy1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_henryTemp1.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, TB_aerobicXform1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_benthicXform1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_unfloodXform1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_photXform1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_hydroXform1.Text)

        Return msg

    End Function

    Private Function DegradateInfo2() As String
        Dim msg As String

        msg = String.Format("{0}", TB_aerobic2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_benthic2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_unflooded2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_photolysis2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_hydrolysis2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_MWT2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_VP2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_SOL2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_Koc2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_AerTemp2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_benthicTemp2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_UnfloodTemp2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_PhotoLat2.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, TB_enthalpy2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_henryTemp2.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, TB_aerobicXform2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_benthicXform2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_unfloodXform2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_photXform2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_hydroXform2.Text)

        Return msg

    End Function

    Private Function ScenarioInfo() As String
        Dim msg As String
        msg = String.Format("{0}", TB_metfile.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_Latitude.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_FloodNumber.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, FloodDayZero.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, FloodMonthZero.Text)

        msg = msg & vbNewLine
        For i As Integer = 0 To FloodInfo.maxFloodEvents - 1
            msg = msg & FloodInfo.day(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To FloodInfo.maxFloodEvents - 1
            msg = msg & FloodInfo.fill(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To FloodInfo.maxFloodEvents - 1
            msg = msg & FloodInfo.wier(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To FloodInfo.maxFloodEvents - 1
            msg = msg & FloodInfo.min(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To FloodInfo.maxFloodEvents - 1
            msg = msg & FloodInfo.turnOver(i).Text & ","
        Next

        msg = msg & String.Format("{0}{1}", vbNewLine, CropDayZero.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, CropMonthZero.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_DaysCropMax.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_DaysCropRemove.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_CropMaxArea.Text)

        '**** Physical ********************8
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_MassTransfer.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_RefDepth.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_benthicdepth.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_pctwa.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_bulkd.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_froc1.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_froc2.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_sused.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_chl.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_doc1.Text)

        msg = msg & String.Format("{0}{1}", vbNewLine, TB_WierLeakage.Text) 'new starting v1.05
        msg = msg & String.Format("{0}{1}", vbNewLine, 0) 'dummy place holder for old biomass1 value
        msg = msg & String.Format("{0}{1}", vbNewLine, 0) 'dummy place holder for old biomass2 value
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_qt.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_dfac.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_area.Text)
        msg = msg & String.Format("{0}{1}", vbNewLine, TB_Leakage.Text)

        Return msg
    End Function

    Function CheckValues() As Integer

        CheckValues = 1

        '********************************************
        If testRealNumbers(TB_aerobic) = 0 Then Return 0
        zeroCheck(TB_aerobic)

        If testRealNumbers(TB_benthic) = 0 Then Return 0
        zeroCheck(TB_benthic)
        If testRealNumbers(TB_unflooded) = 0 Then Return 0
        zeroCheck(TB_unflooded)
        If testRealNumbers(TB_photolysis) = 0 Then Return 0
        zeroCheck(TB_photolysis)

        If testRealNumbers(TB_hydrolysis) = 0 Then Return 0
        zeroCheck(TB_hydrolysis)

        If testRealNumbers(TB_MWT) = 0 Then Return 0
        If testRealNumbers(TB_VP) = 0 Then Return 0

        If testRealNumbers(TB_SOL) = 0 Then Return 0
        If testRealNumbers(TB_Koc) = 0 Then Return 0
        If testRealNumbers(TB_AerTemp) = 0 Then Return 0
        If testRealNumbers(TB_BenthicTemp) = 0 Then Return 0
        If testRealNumbers(TB_UnfloodTemp) = 0 Then Return 0

        If testRealNumbers(TB_PhotoLat) = 0 Then Return 0
        If TB_PhotoLat.Text < 0 Or TB_PhotoLat.Text > 90 Then
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("Photolysis Latitude should be between 0 and 90 degrees.")
            Beep()
            Return 0
        End If

        If testRealNumbers(TB_henrytemp) = 0 Then Return 0
        If testRealNumbers(TB_enthalpy) = 0 Then Return 0
        If testRealNumbers(TB_BenthicTemp) = 0 Then Return 0
        If testRealNumbers(TB_BenthicTemp) = 0 Then Return 0

        '*****  Mass Application and Date Checks ************************* 
        Dim ApplicationNumber As Integer

        If testIntegers(TB_AppNumber) = 0 Then Return 0
        ApplicationNumber = TB_AppNumber.Text

        For i As Integer = 0 To TB_AppNumber.Text - 1
            If testRealNumbers(ApplicationInfo.rate(i)) = 0 Then Return 0
            If testRealNumbers(ApplicationInfo.slowRelease(i)) = 0 Then Return 0
            If calendarCheck(ApplicationInfo.day(i), ApplicationInfo.month(i)) = 0 Then Return 0
        Next
        '**********************************************************************

        Dim returnValue As Boolean
        returnValue = System.IO.File.Exists(TB_metfile.Text)
        If Not returnValue Then
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("Meteorolical file does not exist. Under Location Tab")
            Beep()
            Return 0
        End If

        If testRealNumbers(TB_Latitude) = 0 Then Return 0

        If TB_Latitude.Text < 0 Or TB_PhotoLat.Text > 90 Then
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("Location latitude should be between 0 and 90 degrees.")
            Beep()
            Return 0
        End If

        '**********************************************************************
        Dim floodnumber As Integer


        If testIntegers(TB_FloodNumber) = 0 Then Return 0
        floodnumber = TB_FloodNumber.Text


        If calendarCheck(FloodDayZero, FloodMonthZero) = 0 Then Return 0

        Dim dummyFill As Double
        Dim dummyWeir As Double
        Dim dummyMin As Double

        For i As Integer = 0 To floodnumber - 1

            If testIntegers(FloodInfo.day(i)) = 0 Then Return 0
            If testRealNumbers(FloodInfo.wier(i)) = 0 Then Return 0
            If testRealNumbers(FloodInfo.fill(i)) = 0 Then Return 0
            If testRealNumbers(FloodInfo.min(i)) = 0 Then Return 0
            If testRealNumbers(FloodInfo.turnOver(i)) = 0 Then Return 0
            If FloodInfo.day(i).Text > 365 Then
                StatusLabel.ForeColor = Color.Red
                StatusLabel.Text = ("All flood events must occur within 365 days of reference day.")
                Beep()
                Return 0
            End If

            dummyFill = FloodInfo.fill(i).Text
            dummyWeir = FloodInfo.wier(i).Text
            dummyMin = FloodInfo.min(i).Text

            If dummyFill > dummyWeir Then
                StatusLabel.ForeColor = Color.Red
                StatusLabel.Text = ("Fill Height must be equal or less than weir height" & vbCrLf & "for flood event " & i + 1 & ".")
                Beep()
            End If

            If dummyMin > dummyFill Then
                StatusLabel.ForeColor = Color.Red
                StatusLabel.Text = ("Minimum height must be less than fill height" & vbCrLf & "for flood event " & i + 1 & ".")
                Beep()
            End If
        Next

        If calendarCheck(CropDayZero, CropMonthZero) = 0 Then Return 0

        If testIntegers(TB_DaysCropMax) = 0 Then Return 0

        If testIntegers(TB_DaysCropRemove) = 0 Then Return 0
        If testRealNumbers(TB_CropMaxArea) = 0 Then Return 0

        If TB_CropMaxArea.Text < 0 Or TB_CropMaxArea.Text > 1 Then
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("crop max should be a between 0 and 1.")
            Beep()
            Return 0
        End If


        '**************************************************************
        If testRealNumbers(TB_MassTransfer) = 0 Then Return 0
        If testRealNumbers(TB_Leakage) = 0 Then Return 0
        If testRealNumbers(TB_RefDepth) = 0 Then Return 0
        If testRealNumbers(TB_benthicdepth) = 0 Then Return 0
        If testRealNumbers(TB_pctwa) = 0 Then Return 0
        If testRealNumbers(TB_bulkd) = 0 Then Return 0
        If testRealNumbers(TB_froc1) = 0 Then Return 0
        If testRealNumbers(TB_froc2) = 0 Then Return 0
        If testRealNumbers(TB_sused) = 0 Then Return 0
        If testRealNumbers(TB_chl) = 0 Then Return 0
        If testRealNumbers(TB_doc1) = 0 Then Return 0
        If testRealNumbers(TB_qt) = 0 Then Return 0
        If testRealNumbers(TB_dfac) = 0 Then Return 0
        If testRealNumbers(TB_area) = 0 Then Return 0
        If testRealNumbers(TB_bulkd) = 0 Then Return 0

        If Not IO.Directory.Exists(TB_OutputDirectory.Text) Then
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("The output directory does not exist." & vbCrLf & "Select an output directory and file name.  See Output Tab")
            Beep()
            Return 0
        End If

        If TB_outputfile.Text = "" Then
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("Specify an output file.")
            Beep()
            Return 0
        End If

        '***************** Degradates ***************************
        If CalcDegradate1.Checked Then

            If testRealNumbers(TB_aerobicXform1) = 0 Then Return 0
            If testRealNumbers(TB_benthicXform1) = 0 Then Return 0
            If testRealNumbers(TB_unfloodXform1) = 0 Then Return 0
            If testRealNumbers(TB_photXform1) = 0 Then Return 0
            If testRealNumbers(TB_hydroXform1) = 0 Then Return 0

            If testRealNumbers(TB_aerobic1) = 0 Then Return 0
            zeroCheck(TB_aerobic1)
            If testRealNumbers(TB_benthic1) = 0 Then Return 0
            zeroCheck(TB_benthic1)
            If testRealNumbers(TB_unflooded1) = 0 Then Return 0
            zeroCheck(TB_unflooded1)
            If testRealNumbers(TB_photolysis1) = 0 Then Return 0
            zeroCheck(TB_photolysis1)

            If testRealNumbers(TB_hydrolysis1) = 0 Then Return 0
            zeroCheck(TB_hydrolysis1)

            If testRealNumbers(TB_MWT1) = 0 Then Return 0
            If testRealNumbers(TB_VP1) = 0 Then Return 0

            If testRealNumbers(TB_SOL1) = 0 Then Return 0
            If testRealNumbers(TB_Koc1) = 0 Then Return 0
            If testRealNumbers(TB_AerTemp1) = 0 Then Return 0
            If testRealNumbers(TB_benthicTemp1) = 0 Then Return 0
            If testRealNumbers(TB_UnfloodTemp1) = 0 Then Return 0

            If testRealNumbers(TB_PhotoLat1) = 0 Then Return 0
            If TB_PhotoLat1.Text < 0 Or TB_PhotoLat1.Text > 90 Then
                StatusLabel.ForeColor = Color.Red
                StatusLabel.Text = ("Degradate 1 Photolysis Latitude should be between 0 and 90 degrees.")
                Beep()
                Return 0
            End If

            If testRealNumbers(TB_henryTemp1) = 0 Then Return 0
            If testRealNumbers(TB_enthalpy1) = 0 Then Return 0
            If testRealNumbers(TB_benthicTemp1) = 0 Then Return 0
            If testRealNumbers(TB_benthicTemp1) = 0 Then Return 0

        End If

        If CalcDegradate2.Checked Then

            If testRealNumbers(TB_aerobicXform2) = 0 Then Return 0
            If testRealNumbers(TB_benthicXform2) = 0 Then Return 0
            If testRealNumbers(TB_unfloodXform2) = 0 Then Return 0
            If testRealNumbers(TB_photXform2) = 0 Then Return 0
            If testRealNumbers(TB_hydroXform2) = 0 Then Return 0

            If testRealNumbers(TB_aerobic2) = 0 Then Return 0
            zeroCheck(TB_aerobic2)
            If testRealNumbers(TB_benthic2) = 0 Then Return 0
            zeroCheck(TB_benthic2)
            If testRealNumbers(TB_unflooded2) = 0 Then Return 0
            zeroCheck(TB_unflooded2)
            If testRealNumbers(TB_photolysis2) = 0 Then Return 0
            zeroCheck(TB_photolysis2)

            If testRealNumbers(TB_hydrolysis2) = 0 Then Return 0
            zeroCheck(TB_hydrolysis2)

            If testRealNumbers(TB_MWT2) = 0 Then Return 0
            If testRealNumbers(TB_VP2) = 0 Then Return 0

            If testRealNumbers(TB_SOL2) = 0 Then Return 0
            If testRealNumbers(TB_Koc2) = 0 Then Return 0
            If testRealNumbers(TB_AerTemp2) = 0 Then Return 0
            If testRealNumbers(TB_benthicTemp2) = 0 Then Return 0
            If testRealNumbers(TB_UnfloodTemp2) = 0 Then Return 0

            If testRealNumbers(TB_PhotoLat2) = 0 Then Return 0
            If TB_PhotoLat1.Text < 0 Or TB_PhotoLat1.Text > 90 Then
                StatusLabel.ForeColor = Color.Red
                StatusLabel.Text = ("Degradate 1 Photolysis Latitude should be between 0 and 90 degrees.")
                Beep()
                Return 0
            End If

            If testRealNumbers(TB_henryTemp2) = 0 Then Return 0
            If testRealNumbers(TB_enthalpy2) = 0 Then Return 0
            If testRealNumbers(TB_benthicTemp2) = 0 Then Return 0
            If testRealNumbers(TB_benthicTemp2) = 0 Then Return 0

        End If

        If VVWMCheckBox.Checked = True Then
            If testRealNumbers(TB_AreaWatershed) = 0 Then Return 0
            If testRealNumbers(TB_WatershedCN) = 0 Then Return 0
            If testRealNumbers(TB_widthMixingCell) = 0 Then Return 0
            If testRealNumbers(TB_depthMixingCell) = 0 Then Return 0
            If testRealNumbers(TB_lengthMixingCell) = 0 Then Return 0
            If testRealNumbers(TB_baseFlow) = 0 Then Return 0

        End If

    End Function

    Private Sub UserGuidanceToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UserGuidanceToolStripMenuItem.Click
        My.Forms.HelpInfo.Show()
    End Sub

    Private Sub CalcDegradate1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CalcDegradate1.CheckedChanged
        If CalcDegradate1.Checked = True Then
            Degradate1Panel.Enabled = True
            CalcDegradate2.Enabled = True
        Else
            Degradate1Panel.Enabled = False
            CalcDegradate2.Enabled = False
            degradate2Panel.Enabled = False
            CalcDegradate2.Checked = False
        End If
    End Sub

    Private Sub CalcDegradate2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CalcDegradate2.CheckedChanged
        If CalcDegradate2.Checked = True Then
            degradate2Panel.Enabled = True
        Else
            degradate2Panel.Enabled = False
        End If
    End Sub

    Private Sub VVWMCheckBox_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VVWMCheckBox.CheckedChanged
        If VVWMCheckBox.Checked = True Then
            TB_WatershedCN.Enabled = True
            TB_AreaWatershed.Enabled = True
            TB_WatershedCN.Enabled = True
            TB_widthMixingCell.Enabled = True
            TB_depthMixingCell.Enabled = True
            TB_lengthMixingCell.Enabled = True
            TB_baseFlow.Enabled = True
        Else
            TB_WatershedCN.Enabled = False
            TB_AreaWatershed.Enabled = False
            TB_WatershedCN.Enabled = False
            TB_widthMixingCell.Enabled = False
            TB_depthMixingCell.Enabled = False
            TB_lengthMixingCell.Enabled = False
            TB_baseFlow.Enabled = False
        End If
    End Sub

    Function testRealNumbers(ByVal name As TextBox) As Integer
        Dim TestNumber As Double
        testRealNumbers = 1

        Try
            TestNumber = name.Text
        Catch ex As Exception
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = "Check the value for " & name.Tag
            Beep()
            Return 0
        End Try

    End Function

    Function testIntegers(ByVal name As TextBox) As Integer
        Dim TestNumber As Integer
        testIntegers = 1

        Try
            TestNumber = name.Text
        Catch ex As Exception
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = "Check the value for " & name.Tag
            Beep()
            Return 0
        End Try

    End Function

    Function calendarCheck(ByVal dayBox As TextBox, ByVal monthBox As TextBox) As Integer

        Dim monthtest As Integer
        Dim daytest As Integer

        calendarCheck = 0

        Try
            monthtest = Convert.ToInt16(monthBox.Text)
        Catch ex As Exception
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = (ex.Message & "  Check " & monthBox.Tag)
            Beep()
            Return 0
        End Try

        Try
            daytest = Convert.ToInt16(dayBox.Text)
        Catch ex As Exception
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = (ex.Message & " Check " & dayBox.Tag)
            Beep()
            Return 0
        End Try

        If monthtest > 12 Or monthtest < 1 Then
            StatusLabel.ForeColor = Color.Red
            StatusLabel.Text = ("The following month is not posssible: " & monthBox.Tag)
            Beep()
            Return 0
        End If

        Select Case monthtest

            Case 1, 3, 5, 7, 8, 10, 12
                If daytest > 31 Or daytest < 1 Then
                    StatusLabel.ForeColor = Color.Red
                    StatusLabel.Text = ("Bad day for " & dayBox.Tag)
                    Beep()
                    Return 0
                End If
            Case 2
                If daytest > 28 Or daytest < 1 Then
                    StatusLabel.ForeColor = Color.Red
                    StatusLabel.Text = ("Bad day for " & dayBox.Tag)
                    Beep()
                    Return 0
                End If
            Case 4, 6, 9, 11
                If daytest > 30 Or daytest < 1 Then
                    StatusLabel.ForeColor = Color.Red
                    StatusLabel.Text = ("Bad day for " & dayBox.Tag)
                    Beep()
                    Return 0
                End If
            Case Else
                StatusLabel.ForeColor = Color.Red
                StatusLabel.Text = ("Month does not exist on Earth calendar for " & monthBox.Tag)
                Beep()
                Return 0
        End Select

        calendarCheck = 1
    End Function

    Sub zeroCheck(ByVal name As TextBox)
        Dim MaxHalfLifeMessage As String
        MaxHalfLifeMessage = "A zero Half Life in PFAM is a flag for a stable compound. Default half Life is 1e8 for "

        If name.Text = 0 Then
            name.Text = "1e8"
            MsgBox(MaxHalfLifeMessage & name.Tag)

        End If
    End Sub

  
    Private Sub Label34_Click(sender As Object, e As EventArgs) Handles Label34.Click

    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Panel2_Paint(sender As Object, e As PaintEventArgs) Handles Panel2.Paint

    End Sub
End Class

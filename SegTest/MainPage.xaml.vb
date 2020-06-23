Imports RS = Intel.RealSense
Imports Accord.Math
Imports Emgu.CV
Imports SegTest.Common
Imports System.Collections.Concurrent
Imports System.ComponentModel
Imports System.Reflection
Imports System.Runtime.InteropServices

Class MainPage
#Region "ProgramConstantsAndVariables"
    ' PluginName defines the friendly name of the plugin
    Private Const PluginName As String = "Main"
    Public UIDispatcher As Threading.Dispatcher
    Public ClipDataStore As TrueObservableCollection(Of ClipData)
    Private WithEvents m_RealSenseCapture As RealSenseCapture
    Private m_SaveVideo As Boolean
    Private m_CaptureStart As Date
    Private m_CaptureStop As Date
    Private m_FramesReceived As Integer
    Private m_Processing As Boolean
    Private m_Converting As Boolean
    Private m_CaptureClipData As ClipData
    Private m_LImage As Matrix(Of Byte) = Nothing
    Private m_RImage As Matrix(Of Byte) = Nothing
    Private m_FrameDisplay As FrameDisplay
    Private m_Compressor As TurboJpegWrapper.TJCompressor
    Private m_ByteFileWriter As ByteFileWriter
    Private m_Processor As Processor
    Private m_Special As Dictionary(Of SpecialEnum, List(Of String))
    Private m_ViewSelections As Boolean
    Private m_SelectForeground As Boolean
    Private m_SelectBackground As Boolean
    Private m_CopySelections As Boolean
    Private m_MarkSelection As Boolean
    Private Const SmallWidth As Integer = 320
    Private Const CaptureFolder As String = "CaptureVideo"
    Private Const VideoFolder As String = "Video"
    Private Const ClipFileName As String = "Clip.dat"
    Private Const ClipBakName As String = "Clip.bak"
    Private Const SelectionFileName As String = "Selections.dat"
    Private Const SelectionBakName As String = "Selections.bak"
    Private Const ExcelResultsName As String = "SegmentationResults.xlsx"
    Private Const SaveVideoWidth As Integer = 640
    Private Const SaveVideoHeight As Integer = 480
    Private Const MaxMissingFaces As Integer = 5 ' maximum number of missing face detections to interpolate
    Private Const DepthFillTolerance As Single = 0.02 ' tolerance in cm of flood fill to get depth mask
    Private Const FPS As Integer = 30 ' fps of video clips
    Private Const WatershedMarginInner As Integer = 15 ' inner watershed distance to refine segment mask
    Private Const WatershedMarginOuter As Integer = 30 ' outer watershed distance to refine segment mask
    Private Const ProcessBegin As Integer = 10 ' begin accuracy analysis X seconds after ClipSeated frame
    Private Const ProcessLength As Integer = 20 ' length of accuracy analysis
    Private Const ProcessClip As Integer = 40 ' cut the clips to this length
    Private Const ProcessCapture As Integer = 60 ' total duration of capture
    Private Const ProcessGhost As Integer = 1 ' processing time after seated for ghost image clip
    Private Const ScaleDown As Integer = 4 ' downscaling of optical flow processing
    Private Const LowerRim As Integer = 20 ' search for black components within LowerRim pixels of the image bottom
    Private Const TargetColourBrightness As Byte = 128 ' target brightness value for the colour images
    Private Const GammaLow As Single = 0.1 ' gamma search range
    Private Const GammaHigh As Single = 5.0 ' gamma search range
    Private Const GammaStep As Single = 0.05 ' gamma search range
    Private Const NoisePrimary As String = "STD" ' default for generating noise clips
    Private Const SegTasks As Integer = 4 ' number of parallel segmentation tasks
    Private Const PlayViewInterval As Integer = 400 ' delay in play view
    Private Const LowerResult As Single = 0.1 ' lower centile cutoff for result output
    Private Const MidResult As Single = 0.5 ' lower centile cutoff for result output
    Private Const UpperResult As Single = 0.9 ' lower centile cutoff for result output
#End Region
#Region "Properties"
    Private Property LImage As Matrix(Of Byte)
        Get
            Return m_LImage
        End Get
        Set(value As Matrix(Of Byte))
            If CommonFunctions.MatrixNotNothing(m_LImage) Then
                m_LImage.Dispose()
            End If
            m_LImage = value
            If IsNothing(value) Then
                ImageL.Source = Nothing
                If ImageL.Visibility <> Visibility.Hidden Then
                    ImageL.Visibility = Visibility.Hidden
                End If
            Else
                ImageL.Source = Converter.MatrixToBitmapSource(value)
                If ImageL.Visibility <> Visibility.Visible Then
                    ImageL.Visibility = Visibility.Visible
                End If
            End If
            SetIcons()
        End Set
    End Property
    Private Property RImage As Matrix(Of Byte)
        Get
            Return m_RImage
        End Get
        Set(value As Matrix(Of Byte))
            If CommonFunctions.MatrixNotNothing(m_RImage) Then
                m_RImage.Dispose()
            End If
            m_RImage = value
            If IsNothing(value) Then
                ImageR.Source = Nothing
                If ImageR.Visibility <> Visibility.Hidden Then
                    ImageR.Visibility = Visibility.Hidden
                End If
            Else
                ImageR.Source = Converter.MatrixToBitmapSource(value)
                If ImageR.Visibility <> Visibility.Visible Then
                    ImageR.Visibility = Visibility.Visible
                End If
            End If
        End Set
    End Property
    Public ReadOnly Property IsRunning As Boolean
        Get
            Return m_CaptureStop = Date.MaxValue
        End Get
    End Property
#End Region
#Region "Main"
    Sub New()
        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        UIDispatcher = Threading.Dispatcher.CurrentDispatcher
        ClipDataStore = New TrueObservableCollection(Of ClipData)
        m_RealSenseCapture = New RealSenseCapture(ComboBoxDevice, ComboBoxProfile, ComboBoxResolution)
        m_SaveVideo = False
        m_CaptureStart = Date.MaxValue
        m_CaptureStop = Date.MinValue
        m_FramesReceived = 0
        m_Processing = False
        m_Converting = False
        m_CaptureClipData = Nothing

        m_FrameDisplay = New FrameDisplay(ComboBoxFrames)
        m_Compressor = New TurboJpegWrapper.TJCompressor
        m_ByteFileWriter = New ByteFileWriter
        m_Processor = New Processor
        m_Special = New Dictionary(Of SpecialEnum, List(Of String))
        m_Special.Add(SpecialEnum.GAU, {"GAU10", "GAU20", "GAU30", "GAU40"}.ToList)
        m_Special.Add(SpecialEnum.UNI, {"UNI05", "UNI10", "UNI15", "UNI20"}.ToList)
        m_Special.Add(SpecialEnum.INI, {"INI00", "INI02"}.ToList)
        m_ViewSelections = False
        m_SelectForeground = False
        m_SelectBackground = False
        m_CopySelections = False
        m_MarkSelection = False

        SetBindings()
    End Sub
    Private Sub Page_Loaded(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles Me.Loaded
        PageMainWindow.Title = MainWindow.ModuleName + " - " + PluginName
        SetIcons()
    End Sub
    Private Sub Page_Unloaded(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles Me.Unloaded
        ' checks to perform when navigating away from this page
        If IsRunning Then
            StopPlaying()
        End If

        If CommonFunctions.MatrixNotNothing(m_LImage) Then
            m_LImage.Dispose()
        End If
        If CommonFunctions.MatrixNotNothing(m_RImage) Then
            m_RImage.Dispose()
        End If

        m_FrameDisplay.Dispose()

        ' wait until the writer is idle
        Do While m_ByteFileWriter.Active <> ByteFileWriter.ActiveEnum.Idle
            Threading.Thread.Sleep(100)
        Loop
    End Sub
    Private Sub SetIcons()
        ' sets the icons for the page
        If m_ViewSelections Then
            ButtonAdjust.HBSource = CommonFunctions.GetIcon("XAMLView")
        Else
            ButtonAdjust.HBSource = CommonFunctions.GetIcon("XAMLViewGray")
        End If
        If m_SelectForeground Then
            ButtonSelectForeground.HBSource = CommonFunctions.GetIcon("XAMLGroundOrange")
        Else
            ButtonSelectForeground.HBSource = CommonFunctions.GetIcon("XAMLMarkForeground")
        End If
        If m_SelectBackground Then
            ButtonSelectBackground.HBSource = CommonFunctions.GetIcon("XAMLGroundOrange")
        Else
            ButtonSelectBackground.HBSource = CommonFunctions.GetIcon("XAMLMarkBackground")
        End If
        If m_MarkSelection Then
            ButtonSelection.HBSource = CommonFunctions.GetIcon("XAMLPencilOrange")
        Else
            ButtonSelection.HBSource = CommonFunctions.GetIcon("XAMLPencil")
        End If
        If m_CopySelections Then
            ButtonCopySelections.HBSource = CommonFunctions.GetIcon("XAMLGrabCut")
        Else
            ButtonCopySelections.HBSource = CommonFunctions.GetIcon("XAMLGrabCutGray")
        End If
        ButtonLoadSelections.HBSource = CommonFunctions.GetIcon("XAMLLoadFile")
        ButtonSaveSelections.HBSource = CommonFunctions.GetIcon("XAMLSaveFile")

        If m_FrameDisplay.LoadVideoFrame Then
            ButtonRewind.HBSource = CommonFunctions.GetIcon("XAMLBackBack")
            ButtonBack.HBSource = CommonFunctions.GetIcon("XAMLBack")
            ButtonPause.HBSource = CommonFunctions.GetIcon("XAMLHold")
            ButtonForward.HBSource = CommonFunctions.GetIcon("XAMLForward")
            ButtonPlay.HBSource = CommonFunctions.GetIcon("XAMLForwardForward")
            ButtonPlayView.HBSource = CommonFunctions.GetIcon("XAMLReplay")
            ButtonEntrance.HBSource = CommonFunctions.GetIcon("XAMLStanding")
            ButtonSeated.HBSource = CommonFunctions.GetIcon("XAMLSitting")

            StackGrabCut.Visibility = Visibility.Visible

            ButtonStartStop.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            ButtonStartStop.HBInnerToolTip = "Start Capture"
            ButtonSaveVideo.IsEnabled = False
            ButtonSaveVideo.HBSource = CommonFunctions.GetIcon("XAMLSaveVideoInactive")
            ButtonSaveVideo.HBInnerToolTip = "Save Video"

            ButtonLoadFrame.HBSource = CommonFunctions.GetIcon("XAMLLoadStepActive")
            ButtonLoadFrame.IsEnabled = True

            ClipDataGrid.IsEnabled = False
        Else
            ButtonRewind.HBSource = CommonFunctions.GetIcon("XAMLBackBackGray")
            ButtonBack.HBSource = CommonFunctions.GetIcon("XAMLBackGray")
            ButtonPause.HBSource = CommonFunctions.GetIcon("XAMLHoldGray")
            ButtonForward.HBSource = CommonFunctions.GetIcon("XAMLForwardGray")
            ButtonPlay.HBSource = CommonFunctions.GetIcon("XAMLForwardForwardGray")
            ButtonPlayView.HBSource = CommonFunctions.GetIcon("XAMLReplayGray")
            ButtonEntrance.HBSource = CommonFunctions.GetIcon("XAMLStandingGray")
            ButtonSeated.HBSource = CommonFunctions.GetIcon("XAMLSittingGray")

            StackGrabCut.Visibility = Visibility.Collapsed

            If Not m_RealSenseCapture.CaptureReady Then
                ButtonStartStop.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
                ButtonStartStop.HBInnerToolTip = "Start Capture"
                ButtonSaveVideo.IsEnabled = False

                ButtonLoadFrame.HBSource = CommonFunctions.GetIcon("XAMLLoadStepInactive")
                ButtonLoadFrame.IsEnabled = True
            ElseIf IsRunning Then
                ButtonStartStop.HBSource = CommonFunctions.GetIcon("XAMLNo")
                ButtonStartStop.HBInnerToolTip = "Stop Capture"
                ButtonSaveVideo.IsEnabled = False

                ButtonLoadFrame.HBSource = CommonFunctions.GetIcon("XAMLLoadStepGray")
                ButtonLoadFrame.IsEnabled = False
            Else
                ButtonStartStop.HBSource = CommonFunctions.GetIcon("XAMLYes")
                ButtonStartStop.HBInnerToolTip = "Start Capture"
                ButtonSaveVideo.IsEnabled = Not m_Converting

                ButtonLoadFrame.HBSource = CommonFunctions.GetIcon("XAMLLoadStepInactive")
                ButtonLoadFrame.IsEnabled = True
            End If
            If m_SaveVideo Then
                ButtonSaveVideo.HBSource = CommonFunctions.GetIcon("XAMLSaveVideoActive")
                ButtonSaveVideo.HBInnerToolTip = "Saving Video"
            Else
                ButtonSaveVideo.HBSource = CommonFunctions.GetIcon("XAMLSaveVideoInactive")
                ButtonSaveVideo.HBInnerToolTip = "Save Video"
            End If

            ClipDataGrid.IsEnabled = True
        End If

        ButtonExit.HBSource = CommonFunctions.GetIcon("XAMLExit")
        ButtonConfig.HBSource = CommonFunctions.GetIcon("XAMLConfiguration")
        ButtonConfig.IsEnabled = Not m_Converting

        ButtonAddClip.HBSource = CommonFunctions.GetIcon("XAMLPlus")
        ButtonRemoveClip.HBSource = CommonFunctions.GetIcon("XAMLMinus")
        ButtonLoadClips.HBSource = CommonFunctions.GetIcon("XAMLLoadFile")
        ButtonSaveClips.HBSource = CommonFunctions.GetIcon("XAMLSaveFile")
        ButtonProcessVideo.HBSource = CommonFunctions.GetIcon("XAMLMarkerVideo")
        ButtonGroundTruth.HBSource = CommonFunctions.GetIcon("XAMLMarkerGround")
        ButtonAccuracy.HBSource = CommonFunctions.GetIcon("XAMLMarkerAccuracy")
        ButtonNoise.HBSource = CommonFunctions.GetIcon("XAMLMarkerNoise")
        ButtonCut.HBSource = CommonFunctions.GetIcon("XAMLMarkerScissors")
        ButtonAll.HBSource = CommonFunctions.GetIcon("XAMLMarkerStar")
        ButtonExport.HBSource = CommonFunctions.GetIcon("XAMLMarkerSave")
    End Sub
    Private Sub SetBindings()
        ComboBoxDevice.DataContext = m_RealSenseCapture
        ComboBoxProfile.DataContext = m_RealSenseCapture
        ComboBoxResolution.DataContext = m_RealSenseCapture
        ComboBoxFrames.DataContext = m_FrameDisplay
        ClipDataGrid.DataContext = ClipDataStore

        Dim oBindingDevice1 As New Binding
        oBindingDevice1.Path = New PropertyPath("DevicesDisplay")
        oBindingDevice1.Mode = BindingMode.OneWay
        oBindingDevice1.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxDevice.SetBinding(HighlightComboBox.HCBContentProperty, oBindingDevice1)

        Dim oBindingDevice2 As New Binding
        oBindingDevice2.Path = New PropertyPath("SelectedDevice")
        oBindingDevice2.Mode = BindingMode.TwoWay
        oBindingDevice2.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxDevice.SetBinding(HighlightComboBox.HCBTextProperty, oBindingDevice2)

        Dim oBindingProfile1 As New Binding
        oBindingProfile1.Path = New PropertyPath("ProfilesDisplay")
        oBindingProfile1.Mode = BindingMode.OneWay
        oBindingProfile1.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxProfile.SetBinding(HighlightComboBox.HCBContentProperty, oBindingProfile1)

        Dim oBindingProfile2 As New Binding
        oBindingProfile2.Path = New PropertyPath("SelectedProfile")
        oBindingProfile2.Mode = BindingMode.TwoWay
        oBindingProfile2.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxProfile.SetBinding(HighlightComboBox.HCBTextProperty, oBindingProfile2)

        Dim oBindingResolution1 As New Binding
        oBindingResolution1.Path = New PropertyPath("ResolutionsDisplay")
        oBindingResolution1.Mode = BindingMode.OneWay
        oBindingResolution1.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxResolution.SetBinding(HighlightComboBox.HCBContentProperty, oBindingResolution1)

        Dim oBindingResolution2 As New Binding
        oBindingResolution2.Path = New PropertyPath("SelectedResolution")
        oBindingResolution2.Mode = BindingMode.TwoWay
        oBindingResolution2.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxResolution.SetBinding(HighlightComboBox.HCBTextProperty, oBindingResolution2)

        Dim oBindingFrame1 As New Binding
        oBindingFrame1.Path = New PropertyPath("FramesDisplay")
        oBindingFrame1.Mode = BindingMode.OneWay
        oBindingFrame1.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxFrames.SetBinding(HighlightComboBox.HCBContentProperty, oBindingFrame1)

        Dim oBindingFrame2 As New Binding
        oBindingFrame2.Path = New PropertyPath("SelectedFrame")
        oBindingFrame2.Mode = BindingMode.TwoWay
        oBindingFrame2.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxFrames.SetBinding(HighlightComboBox.HCBTextProperty, oBindingFrame2)
    End Sub
    Private Sub SoftwareFrameAvailableHandler(ByVal oFrame As Frame) Handles m_RealSenseCapture.SoftwareFrameAvailable
        ' handler for software frame available event
        Dim oCreateDate As Date = If(oFrame.ColourCreateDate >= oFrame.DepthCreateDate, oFrame.ColourCreateDate, oFrame.DepthCreateDate)
        Select Case oFrame.FrameAction
            Case Frame.FrameActionEnum.StartTransmission
                m_CaptureStart = oCreateDate
                m_CaptureStop = Date.MaxValue
                PlayingStarted()

                Dim TaskDelegate As Action = Sub()
                                                 TextImageL.Text = "Depth"
                                             End Sub

                CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
            Case Frame.FrameActionEnum.StopTransmission
                m_CaptureStart = Date.MaxValue
                m_CaptureStop = oCreateDate
                PlayingFinished()
            Case Frame.FrameActionEnum.Frame
                If CommonFunctions.MatrixNotNothing(oFrame.DepthFrameSingle) Then
                    Dim sFileName As String = oFrame.DepthFileName
                    Dim oFileInfo As New IO.FileInfo(sFileName)
                    Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
                    Dim sNewName As String = oFileInfo.DirectoryName + "\" + Left(sName, 52) + "DepthSingle" + Right(sName, 1) + oFileInfo.Extension

                    m_ByteFileWriter.EnqueueData(sNewName, (New MatrixData(Of Single)(oFrame.DepthFrameSingle)).GetBytes, m_CaptureClipData)

                    If Not m_Processing Then
                        m_Processing = True

                        Dim oDepthFrameSingle As Matrix(Of Single) = oFrame.DepthFrameSingle.Clone

                        Dim TaskDelegate As Action = Sub()
                                                         Dim oColourImage As Matrix(Of Byte) = Nothing
                                                         Converter.CreateDepthMap(oColourImage, oDepthFrameSingle, CvEnum.ColorMapType.Jet)
                                                         LImage = oColourImage

                                                         oColourImage.Dispose()
                                                         oDepthFrameSingle.Dispose()

                                                         m_Processing = False
                                                     End Sub

                        CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
                    End If
                End If
        End Select

        ' clean up
        oFrame.Dispose()
    End Sub
    Private Sub RingAlarm() Handles m_RealSenseCapture.RingAlarm
        ' rings the alarm sound
        System.Media.SystemSounds.Asterisk.Play()
    End Sub
    Private Sub FrameAvailableHandler(ByVal oFrame As Frame) Handles m_RealSenseCapture.FrameAvailable
        ' handler for frame available event
        Dim oColorRed As [Structure].MCvScalar = New [Structure].Bgr(System.Drawing.Color.Red).MCvScalar

        Dim oCreateDate As Date = If(oFrame.ColourCreateDate >= oFrame.DepthCreateDate, oFrame.ColourCreateDate, oFrame.DepthCreateDate)
        Select Case oFrame.FrameAction
            Case Frame.FrameActionEnum.StartTransmission
                If m_SaveVideo AndAlso (Not IsNothing(m_CaptureClipData)) Then
                    m_CaptureClipData.ColourFrames = 0
                    m_CaptureClipData.DepthFrames = 0

                    Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name)
                    If Not IsNothing(oFrame.ColourDepthProfile) Then
                        CommonFunctions.SerializeDataContractFile(sFolderName + "\ColourDepthProfile.dat", oFrame.ColourDepthProfile)
                    End If
                End If

                m_FramesReceived = 0
                m_CaptureStart = oCreateDate
                m_CaptureStop = Date.MaxValue
                PlayingStarted()

                Dim TaskDelegate As Action = Sub()
                                                 TextImageL.Text = "Capture"
                                             End Sub

                CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
            Case Frame.FrameActionEnum.StopTransmission
                ' wait until the writer is idle
                If m_ByteFileWriter.Active <> ByteFileWriter.ActiveEnum.Idle Then
                    m_ByteFileWriter.Stop()

                    Do While m_ByteFileWriter.Active <> ByteFileWriter.ActiveEnum.Idle
                        Threading.Thread.Sleep(100)
                    Loop
                End If

                m_CaptureClipData = Nothing
                m_CaptureStart = Date.MaxValue
                m_CaptureStop = oCreateDate
                PlayingFinished()
                m_FramesReceived = 0
            Case Frame.FrameActionEnum.Frame
                If CommonFunctions.MatrixNotNothing(oFrame.ColourFrame) OrElse CommonFunctions.MatrixNotNothing(oFrame.DepthFrame) Then
                    If oCreateDate <= m_CaptureStop Then
                        m_FramesReceived += 1
                        oFrame.Number = m_FramesReceived

                        If m_SaveVideo AndAlso (Not IsNothing(m_CaptureClipData)) Then
                            Dim sTimerText As String = (If(oFrame.ColourCreateDate >= oFrame.DepthCreateDate, oFrame.ColourCreateDate, oFrame.DepthCreateDate) - m_CaptureStart).TotalSeconds.ToString("N1") + " s"
                            Dim TaskDelegate1 As Action = Sub()
                                                              TextTimer.Text = sTimerText
                                                          End Sub

                            CommonFunctions.SafeInvoke(TaskDelegate1, UIDispatcher, True)

                            CaptureVideo(oFrame, m_CaptureClipData)
                        End If

                        If Not m_Processing Then
                            If CommonFunctions.MatrixNotNothing(oFrame.ColourFrame) Then
                                m_Processing = True

                                Dim oColourFrame As Matrix(Of Byte) = oFrame.ColourFrame.Clone

                                Dim TaskDelegate2 As Action = Sub()
                                                                  If CommonFunctions.MatrixNotNothing(oColourFrame) Then
                                                                      Dim oFaceResults As List(Of Processor.FaceResult) = m_Processor.DetectFaces(oColourFrame)
                                                                      If oFaceResults.Count = 1 Then
                                                                          Dim fLineScale As Single = Math.Max(Math.Sqrt((oColourFrame.Width * oColourFrame.Width) + (oColourFrame.Height * oColourFrame.Height)) / 1000, 2)
                                                                          Dim oVertices As System.Drawing.Point() = oFaceResults.First.Region.GetVertices.Select(Function(oPoint) New System.Drawing.Point(oPoint.X, oPoint.Y)).ToArray
                                                                          CvInvoke.Polylines(oColourFrame, oVertices, True, oColorRed, 1 * fLineScale)
                                                                      End If

                                                                      LImage = oColourFrame
                                                                  End If

                                                                  oColourFrame.Dispose()

                                                                  m_Processing = False
                                                              End Sub

                                CommonFunctions.SafeInvoke(TaskDelegate2, UIDispatcher, True)
                            End If
                        End If
                    End If
                End If
        End Select

        ' clean up
        oFrame.Dispose()
    End Sub
    Private Sub SelectedChangedHandler() Handles m_RealSenseCapture.SelectedChanged
        ' fires whenever the realsense selections change
        SetIcons()
    End Sub
    Private Sub PlayingStarted()
        Dim oAction As Action = Sub()
                                    TextImageL.Text = String.Empty
                                    TextImageR.Text = String.Empty
                                    TextTimer.Text = String.Empty
                                    TextTimer1.Text = String.Empty
                                    TextTimer2.Text = String.Empty
                                    TextTimer3.Text = String.Empty
                                    TextTimer4.Text = String.Empty
                                    SetIcons()
                                    m_Processor.Reset()
                                End Sub
        CommonFunctions.SafeInvoke(oAction, UIDispatcher, True)
    End Sub
    Private Sub PlayingFinished()
        Dim oAction As Action = Sub()
                                    LImage = Nothing
                                    RImage = Nothing
                                    TextImageL.Text = String.Empty
                                    TextImageR.Text = String.Empty
                                    TextTimer.Text = String.Empty
                                    TextTimer1.Text = String.Empty
                                    TextTimer2.Text = String.Empty
                                    TextTimer3.Text = String.Empty
                                    TextTimer4.Text = String.Empty
                                    SetIcons()
                                    m_Processor.Reset()
                                End Sub
        CommonFunctions.SafeInvoke(oAction, UIDispatcher, True)
    End Sub
    Private Sub DeviceReset() Handles m_RealSenseCapture.DeviceReset
        Dim bRunning As Boolean = IsRunning
        StopPlaying()

        If bRunning Then
            StartPlaying(-1, New List(Of Integer))
        End If
    End Sub
    Public Sub StartPlaying(ByVal iDuration As Integer, ByVal oAlarms As List(Of Integer))
        m_RealSenseCapture.CaptureStart(iDuration, oAlarms)
    End Sub
    Public Sub StopPlaying()
        If m_RealSenseCapture.Capturing Then
            m_RealSenseCapture.CaptureStop()
        Else
            m_CaptureStop = NTPTime.GetDateTimeNow
            PlayingFinished()
        End If
    End Sub
    Private Sub LoadCurrentFrame()
        If m_FrameDisplay.FrameCount > 0 Then
            m_FrameDisplay.CurrentFrame = Math.Min(Math.Max(m_FrameDisplay.CurrentFrame, 0), m_FrameDisplay.FrameCount - 1)

            Dim oFrame As Tuple(Of Matrix(Of Byte), Matrix(Of Byte), String, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum))) = m_FrameDisplay.GetCurrentFrame

            Dim UpdateDelegate As Action = Sub()
                                               Dim oColourRed As [Structure].MCvScalar = New [Structure].Bgr(System.Drawing.Color.Red).MCvScalar
                                               Dim oColourOrange As [Structure].MCvScalar = New [Structure].Bgr(System.Drawing.Color.Orange).MCvScalar
                                               Dim oColourWhite As [Structure].MCvScalar = New [Structure].Bgr(System.Drawing.Color.White).MCvScalar
                                               Dim oColourBlack As [Structure].MCvScalar = New [Structure].Bgr(System.Drawing.Color.Black).MCvScalar

                                               TextFrame.Text = (m_FrameDisplay.CurrentFrame + 1).ToString
                                               TextTime.Text = oFrame.Item3

                                               If CommonFunctions.MatrixNotNothing(oFrame.Item1) Then
                                                   If m_ViewSelections Then
                                                       Using oUnderlayMatrix As Matrix(Of Byte) = oFrame.Item1.Mul(0.5)
                                                           Using oOverlayMatrix As Matrix(Of Byte) = oFrame.Item1.Sub(oUnderlayMatrix)
                                                               oOverlayMatrix.SetValue(New [Structure].MCvScalar(127, 127, 127), oFrame.Item2)
                                                               CvInvoke.Add(oOverlayMatrix, oUnderlayMatrix, oOverlayMatrix)

                                                               ' add selections
                                                               For Each oSelection In oFrame.Item4
                                                                   Dim oSelectionColour As [Structure].MCvScalar = oColourOrange
                                                                   If oSelection.Item4 = SelectionTypeEnum.Foreground Then
                                                                       oSelectionColour = oColourWhite
                                                                   ElseIf oSelection.Item4 = SelectionTypeEnum.Background Then
                                                                       oSelectionColour = oColourBlack
                                                                   End If

                                                                   Dim oPoints As List(Of System.Drawing.Point) = (From oPoint In oSelection.Item1 Select New System.Drawing.Point(oPoint.X * (oOverlayMatrix.Width - 1), oPoint.Y * (oOverlayMatrix.Height - 1))).ToList
                                                                   For i = 0 To oPoints.Count - 1
                                                                       Dim iCurrentIndex As Integer = i
                                                                       Dim iNextIndex As Integer = (i + 1) Mod oPoints.Count
                                                                       CvInvoke.Line(oOverlayMatrix, oPoints(iCurrentIndex), oPoints(iNextIndex), oSelectionColour)
                                                                   Next

                                                                   Dim oCentroid As New System.Drawing.Point(oSelection.Item2.X * (oOverlayMatrix.Width - 1), oSelection.Item2.Y * (oOverlayMatrix.Height - 1))
                                                                   CvInvoke.Circle(oOverlayMatrix, oCentroid, 5, oColourRed, -1)
                                                                   CvInvoke.Circle(oOverlayMatrix, oCentroid, 4, oSelectionColour, -1)
                                                               Next
                                                               Dim oCurrentPoints As List(Of System.Drawing.Point) = (From oPoint In m_FrameDisplay.CurrentSelection Select New System.Drawing.Point(oPoint.X * (oOverlayMatrix.Width - 1), oPoint.Y * (oOverlayMatrix.Height - 1))).ToList
                                                               For i = 0 To oCurrentPoints.Count - 1
                                                                   Dim iCurrentIndex As Integer = i
                                                                   Dim iNextIndex As Integer = (i + 1) Mod oCurrentPoints.Count

                                                                   CvInvoke.Line(oOverlayMatrix, oCurrentPoints(iCurrentIndex), oCurrentPoints(iNextIndex), oColourRed)
                                                               Next

                                                               LImage = oOverlayMatrix
                                                           End Using
                                                       End Using
                                                       TextImageL.Text = "Colour Overlay"
                                                   Else
                                                       LImage = oFrame.Item1
                                                       TextImageL.Text = "Colour"
                                                   End If
                                               Else
                                                   LImage = Nothing
                                                   TextImageL.Text = String.Empty
                                               End If
                                               If CommonFunctions.MatrixNotNothing(oFrame.Item2) Then
                                                   If m_ViewSelections Then
                                                       If oFrame.Item4.Count = 0 Then
                                                           RImage = oFrame.Item2
                                                       Else
                                                           Using oGrabCutMatrix As Matrix(Of Byte) = GrabCut(oFrame.Item1, oFrame.Item2, oFrame.Item4)
                                                               RImage = oGrabCutMatrix
                                                           End Using
                                                       End If
                                                       TextImageR.Text = "Ground Truth (Adjusted)"
                                                   Else
                                                       RImage = oFrame.Item2
                                                       TextImageR.Text = "Ground Truth"
                                                   End If
                                               Else
                                                   TextImageR.Text = String.Empty
                                               End If

                                               ' clean up
                                               If CommonFunctions.MatrixNotNothing(oFrame.Item1) Then
                                                   oFrame.Item1.Dispose()
                                               End If
                                               If CommonFunctions.MatrixNotNothing(oFrame.Item2) Then
                                                   oFrame.Item2.Dispose()
                                               End If
                                           End Sub
            CommonFunctions.SafeInvoke(UpdateDelegate, UIDispatcher, True)
        Else
            m_FrameDisplay.CurrentFrame = -1
        End If
    End Sub
    Private Sub ContinuousLoadFrame()
        Do
            Dim iStopFrame As Integer = Val(m_FrameDisplay.SelectedFrame.Name) - 1
            Dim iOldFrame As Integer = m_FrameDisplay.CurrentFrame
            Select Case m_FrameDisplay.LoadVideoFrameState
                Case FrameDisplay.PlayFrameEnum.Play, FrameDisplay.PlayFrameEnum.PlayView
                    If m_FrameDisplay.FrameCount > 0 AndAlso m_FrameDisplay.CurrentFrame <> iStopFrame AndAlso m_FrameDisplay.CurrentFrame < m_FrameDisplay.FrameCount - 1 Then
                        m_FrameDisplay.CurrentFrame += 1
                        LoadCurrentFrame()
                        If m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.PlayView Then
                            Threading.Thread.Sleep(PlayViewInterval)
                        End If
                    Else
                        m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused
                        Exit Do
                    End If
                Case FrameDisplay.PlayFrameEnum.Rewind
                    If m_FrameDisplay.FrameCount > 0 AndAlso m_FrameDisplay.CurrentFrame <> iStopFrame AndAlso m_FrameDisplay.CurrentFrame > 0 Then
                        m_FrameDisplay.CurrentFrame -= 1
                        LoadCurrentFrame()
                    Else
                        m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused
                        Exit Do
                    End If
                Case FrameDisplay.PlayFrameEnum.PausedStop
                    m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused
                    StopLoadFrame()
                Case FrameDisplay.PlayFrameEnum.ContinuousPlay
                    If m_FrameDisplay.FrameCount > 0 AndAlso m_FrameDisplay.CurrentFrame < m_FrameDisplay.FrameCount - 1 Then
                        m_FrameDisplay.CurrentFrame += 1
                        LoadCurrentFrame()
                    Else
                        m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.PausedStop
                    End If
                Case Else
                    Exit Do
            End Select
            Dim iNewFrame As Integer = m_FrameDisplay.CurrentFrame
            If m_CopySelections AndAlso iOldFrame <> iNewFrame Then
                m_FrameDisplay.CopySelections(iOldFrame, iNewFrame)
            End If
        Loop
    End Sub
    Private Sub StopLoadFrame()
        ' ends load frame
        m_FrameDisplay.Clear()
        m_FrameDisplay.LoadVideoFrame = False
        m_CaptureClipData = Nothing

        Dim UpdateDelegate As Action = Sub()
                                           LImage = Nothing
                                           RImage = Nothing
                                       End Sub
        CommonFunctions.SafeInvoke(UpdateDelegate, UIDispatcher, True)
    End Sub
    Private Sub CaptureVideo(ByVal oFrame As Frame, ByRef oClipData As ClipData)
        ' captures video to files
        Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(oClipData.Name)
        Dim sColour As String = [Enum].GetName(GetType(RealSenseCapture.StreamTypeEnum), RealSenseCapture.StreamTypeEnum.Colour)
        Dim sDepth As String = [Enum].GetName(GetType(RealSenseCapture.StreamTypeEnum), RealSenseCapture.StreamTypeEnum.Depth)
        Dim sDepthScale As String = oFrame.DepthScale.ToString("E1", Globalization.CultureInfo.CreateSpecificCulture("en-US"))
        Dim sColourScale As String = CSng(0).ToString("E1", Globalization.CultureInfo.CreateSpecificCulture("en-US"))

        If CommonFunctions.MatrixNotNothing(oFrame.ColourFrame) Then
            Dim oTimeSpan As TimeSpan = oFrame.ColourCreateDate - m_CaptureStart
            Dim sDuration As String = CInt(oTimeSpan.TotalMilliseconds).ToString("D10")

            Dim sColourString As String = Converter.DoubleToByteString(oFrame.ColourTimestamp)
            Dim sColourDomainString As String = oFrame.ColourTimestampDomain.ToString
            Dim sFilename As String = "[" + oFrame.Number.ToString("D6") + "][" + sDuration + "][" + sColourScale + "][" + sColourString + "][" + sColourDomainString + "][" + sColour + "].bin"
            oClipData.ColourFrames += 1
            oClipData.ProcessingFrames += 1
            m_ByteFileWriter.EnqueueByte(sFolderName + "\" + sFilename, New MatrixData(Of Byte)(oFrame.ColourFrame), oClipData)

            Dim sIndexFilename As String = "[" + oFrame.Number.ToString("D6") + "][" + sDuration + "][Index].jpg"
            Dim fSmallScale As Double = SmallWidth / oFrame.ColourFrame.Width
            Dim iSmallHeight As Integer = oFrame.ColourFrame.Height * fSmallScale
            Using oIndexMatrix As New Matrix(Of Byte)(iSmallHeight, SmallWidth, 3)
                CvInvoke.Resize(oFrame.ColourFrame, oIndexMatrix, oIndexMatrix.Size)
                Dim bIndexBytes As Byte() = Converter.MatrixToJpgBytes(oIndexMatrix, 50, m_Compressor)
                m_ByteFileWriter.EnqueueData(sFolderName + "\" + sIndexFilename, bIndexBytes, Nothing)
            End Using
        End If

        If CommonFunctions.MatrixNotNothing(oFrame.DepthFrame) Then
            Dim oTimeSpan As TimeSpan = oFrame.DepthCreateDate - m_CaptureStart
            Dim sDuration As String = CInt(oTimeSpan.TotalMilliseconds).ToString("D10")

            Dim sDepthString As String = Converter.DoubleToByteString(oFrame.DepthTimestamp)
            Dim sDepthDomainString As String = oFrame.DepthTimestampDomain.ToString
            Dim sFilename As String = "[" + oFrame.Number.ToString("D6") + "][" + sDuration + "][" + sDepthScale + "][" + sDepthString + "][" + sDepthDomainString + "][" + sDepth + "].bin"
            oClipData.DepthFrames += 1
            oClipData.ProcessingFrames += 1
            m_ByteFileWriter.EnqueueUShort(sFolderName + "\" + sFilename, New MatrixData(Of UShort)(oFrame.DepthFrame), oClipData)
        End If
    End Sub
    Private Function ProcessColour(ByVal sFolderName As String, ByVal sVideoName As String, ByVal fps As Integer) As Boolean
        ' retimes colour images and creates a compressed MP4 file
        Dim TaskDelegate1 As Action = Sub()
                                          TextImageL.Text = "Colour"
                                      End Sub

        CommonFunctions.SafeInvoke(TaskDelegate1, UIDispatcher, True)

        Dim oDirectoryInfo As New IO.DirectoryInfo(sVideoName)
        If Not oDirectoryInfo.Exists Then
            oDirectoryInfo.Create()
        End If

        Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_Colour.avi").ToList
        Dim iSpecialCount As Integer = Aggregate oSpecial In m_Special.Keys From sName In m_Special(oSpecial) Into Count(sName = m_CaptureClipData.Name)
        If oFileInfoList.Count > 0 OrElse iSpecialCount > 0 Then
            ' do not process as output already present
            Return True
        Else
            Dim oPlayDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single))) = GetPlayDictionary(sFolderName)
            Dim oColourInterpolations As Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)) = GetInterpolations(oPlayDictionary).Item1

            Dim sColourVideoFileName As String = sVideoName + "\[" + oColourInterpolations.Keys.Count.ToString("D6") + "]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_Colour.avi"

            Dim iColourCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Count()
            Dim iDepthCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Depth Into Count()

            If iColourCount > 0 Then
                m_CaptureClipData.ColourFrames = iColourCount
                m_CaptureClipData.DepthFrames = iDepthCount

                ' get gamma shift for colour
                Dim oColourFileList As List(Of String) = (From oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select oStream.Item2).ToList
                Dim oBrightnessList As New List(Of Single)

                m_CaptureClipData.ProcessingFrames = oColourFileList.Count
                For Each oColourFileName In oColourFileList
                    Dim oBytes As Byte() = IO.File.ReadAllBytes(oColourFileName)
                    Using oMatrix As Matrix(Of Byte) = MatrixData(Of Byte).GetMatrixData(oBytes).GetMatrix
                        Dim oAverage As [Structure].MCvScalar = CvInvoke.Mean(oMatrix)
                        oBrightnessList.Add((oAverage.V0 + oAverage.V1 + oAverage.V2) / 3.0)
                        m_CaptureClipData.ProcessingFrames -= 1
                    End Using
                Next

                ' get median brightness
                oBrightnessList.Sort()
                Dim bMedianBrightness As Byte = CByte(oBrightnessList((oBrightnessList.Count - 1) / 2.0))

                ' get list of gamma values and corrected brightness
                Dim oGammaList As New List(Of Tuple(Of Single, Byte))
                For i = GammaLow To GammaHigh Step GammaStep
                    Using oTestMatrix As New Matrix(Of Byte)(2, 2)
                        oTestMatrix.SetValue(bMedianBrightness)
                        Using oTestBitmap As System.Drawing.Bitmap = Converter.MatToBitmap(oTestMatrix.Mat, 300)
                            Dim oTestGammaCorrection As New Accord.Imaging.Filters.GammaCorrection(i)
                            oTestGammaCorrection.ApplyInPlace(oTestBitmap)
                            Using oCorrectedMatrix As Matrix(Of Byte) = Converter.BitmapToMatrix(oTestBitmap)
                                Dim oCorrectedAverage As [Structure].MCvScalar = CvInvoke.Mean(oCorrectedMatrix)
                                Dim bCorrectedBrightness As Byte = CByte(oCorrectedAverage.V0)
                                oGammaList.Add(New Tuple(Of Single, Byte)(i, bCorrectedBrightness))
                            End Using
                        End Using
                    End Using
                Next

                Dim fGamma As Single = 1.0
                For i = 1 To oGammaList.Count - 1
                    If oGammaList(i).Item2 >= TargetColourBrightness Then
                        fGamma = (oGammaList(i - 1).Item1 + oGammaList(i).Item1) / 2.0
                        Exit For
                    End If
                Next

                ' create video directory if necessary
                If Not IO.Directory.Exists(sVideoName) Then
                    IO.Directory.CreateDirectory(sVideoName)
                End If

                m_CaptureClipData.ProcessingFrames = oColourInterpolations.Count

                Dim OpticalFlowHeight As Integer = 0
                Dim OpticalFlowWidth As Integer = 0
                Dim oFlowMatrix As Matrix(Of Single) = Nothing
                Dim oMapMatrix As Matrix(Of Single) = Nothing
                Dim bNewFlow As Boolean = False

                Dim oGammaCorrection As New Accord.Imaging.Filters.GammaCorrection(fGamma)
                Using oColourVideoWriter As New Accord.Video.FFMPEG.VideoFileWriter
                    oColourVideoWriter.Open(sColourVideoFileName, SaveVideoWidth, SaveVideoHeight, New Rational(fps), Accord.Video.FFMPEG.VideoCodec.MPEG4, 4000000)
                    For i = 0 To oColourInterpolations.Keys.Count - 1
                        Dim iKey As Integer = oColourInterpolations.Keys(i)
                        Dim iFrame1 As Integer = oColourInterpolations(iKey).Item1
                        Dim iFrame2 As Integer = oColourInterpolations(iKey).Item2
                        Dim fFraction As Single = oColourInterpolations(iKey).Item3
                        Dim sFileName1 As String = (From oStream In oPlayDictionary(iFrame1) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select oStream.Item2).First
                        Dim sFileName2 As String = (From oStream In oPlayDictionary(iFrame2) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select oStream.Item2).First

                        Dim oBytes1 As Byte() = IO.File.ReadAllBytes(sFileName1)
                        Using oMatrix1 As Matrix(Of Byte) = MatrixData(Of Byte).GetMatrixData(oBytes1).GetMatrix
                            Dim oBytes2 As Byte() = IO.File.ReadAllBytes(sFileName2)
                            Using oMatrix2 As Matrix(Of Byte) = MatrixData(Of Byte).GetMatrixData(oBytes2).GetMatrix
                                If Not m_CaptureClipData.AltMotion Then
                                    If OpticalFlowHeight = 0 OrElse OpticalFlowWidth = 0 Then
                                        OpticalFlowHeight = oMatrix1.Height / ScaleDown
                                        OpticalFlowWidth = oMatrix1.Width / ScaleDown
                                        oFlowMatrix = New Matrix(Of Single)(OpticalFlowHeight, OpticalFlowWidth, 2)

                                        Using oMapMatrixX As New Matrix(Of Single)(oMatrix1.Size)
                                            Using oMapMatrixY As New Matrix(Of Single)(oMatrix1.Size)
                                                For x = 0 To oMapMatrixX.Width - 1
                                                    oMapMatrixX.GetCol(x).SetValue(x)
                                                Next
                                                For y = 0 To oMapMatrixY.Height - 1
                                                    oMapMatrixY.GetRow(y).SetValue(y)
                                                Next

                                                Using oVectorMat As New Util.VectorOfMat({oMapMatrixX.Mat, oMapMatrixY.Mat})
                                                    oMapMatrix = New Matrix(Of Single)(oMatrix1.Height, oMatrix1.Width, 2)
                                                    CvInvoke.Merge(oVectorMat, oMapMatrix)
                                                End Using
                                            End Using
                                        End Using

                                        bNewFlow = True
                                    Else
                                        bNewFlow = False
                                    End If
                                End If
                                Using oInterMatrix As Matrix(Of Byte) = If(m_CaptureClipData.AltMotion, GetAltFlowFrame(oMatrix1, oMatrix2, fFraction), GetFlowFrame(oMatrix1, oMatrix2, oFlowMatrix, oMapMatrix, bNewFlow, fFraction))
                                    Using oSmallMatrix As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth, 3)
                                        CvInvoke.Resize(oInterMatrix, oSmallMatrix, oSmallMatrix.Size, 0, 0, CvEnum.Inter.Cubic)
                                        Using oSmallBitmap As System.Drawing.Bitmap = Converter.MatToBitmap(oSmallMatrix.Mat, 300)
                                            oGammaCorrection.ApplyInPlace(oSmallBitmap)
                                            oColourVideoWriter.WriteVideoFrame(oSmallBitmap)
                                        End Using
                                    End Using

                                    Dim oColourMatrix As Matrix(Of Byte) = oInterMatrix.Clone
                                    Dim TaskDelegate As Action = Sub()
                                                                     LImage = oColourMatrix

                                                                     oColourMatrix.Dispose()
                                                                 End Sub

                                    CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
                                End Using
                            End Using
                        End Using

                        m_CaptureClipData.ProcessingFrames -= 1
                    Next
                End Using
                If CommonFunctions.MatrixNotNothing(oFlowMatrix) Then
                    oFlowMatrix.Dispose()
                    oFlowMatrix = Nothing
                End If
                If CommonFunctions.MatrixNotNothing(oMapMatrix) Then
                    oMapMatrix.Dispose()
                    oMapMatrix = Nothing
                End If

                Return True
            Else
                Return False
            End If
        End If
    End Function
    Private Function ConvertVideo(ByVal sFolderName As String, ByVal sVideoName As String) As Boolean
        ' converts raw frames to video
        Dim oPlayDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single))) = GetPlayDictionary(sFolderName)

        Dim iColourCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Count()
        Dim iDepthCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Depth Into Count()
        Dim iDepthSingleCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Into Count()
        Dim iSpecialCount As Integer = Aggregate oSpecial In m_Special.Keys From sName In m_Special(oSpecial) Into Count(sName = m_CaptureClipData.Name)

        Dim bGroundTruthPresent As Boolean = False
        Dim oDirectoryInfo As New IO.DirectoryInfo(sVideoName)
        Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_Colour.avi").ToList
        If oFileInfoList.Count > 0 Then
            Dim oFileInfo As IO.FileInfo = oFileInfoList.First
            Dim sColourVideoFileName As String = oFileInfo.FullName
            Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
            Dim sTotalFrames As String = Mid(sName, 2, 6)
            Dim iTotalFrames As Integer = 0
            If Integer.TryParse(sTotalFrames, iTotalFrames) Then
                Dim sDepthVideoFileName As String = sVideoName + "\[" + iTotalFrames.ToString("D6") + "]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_GroundTruth.avi"
                If IO.File.Exists(sDepthVideoFileName) Then
                    bGroundTruthPresent = True
                End If
            End If
        End If

        If bGroundTruthPresent OrElse iDepthCount = iDepthSingleCount OrElse m_CaptureClipData.IgnoreDepth OrElse iSpecialCount > 0 Then
            ' do not process as output already present
            Return True
        Else
            If iColourCount > 0 Then
                m_CaptureClipData.ColourFrames = iColourCount
                m_CaptureClipData.DepthFrames = iDepthCount
                m_CaptureClipData.ProcessingFrames = iDepthCount

                m_RealSenseCapture.ConvertDepth(sFolderName, oPlayDictionary)

                Return True
            Else
                Return False
            End If
        End If
    End Function
    Private Function ProcessDepth(ByVal sFolderName As String, ByVal sVideoName As String, ByVal fps As Integer) As Boolean
        ' retimes depth images and creates an uncompressed MKV file
        Dim oColorRed As [Structure].MCvScalar = New [Structure].Bgr(System.Drawing.Color.Red).MCvScalar

        Dim TaskDelegate1 As Action = Sub()
                                          TextImageL.Text = "Colour"
                                      End Sub

        CommonFunctions.SafeInvoke(TaskDelegate1, UIDispatcher, True)

        Dim oDirectoryInfo As New IO.DirectoryInfo(sVideoName)
        Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_Colour.avi").ToList
        If oFileInfoList.Count > 0 Then
            Dim oFileInfo As IO.FileInfo = oFileInfoList.First
            Dim sColourVideoFileName As String = oFileInfo.FullName
            Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
            Dim sTotalFrames As String = Mid(sName, 2, 6)
            Dim iTotalFrames As Integer = 0
            If Integer.TryParse(sTotalFrames, iTotalFrames) Then
                Dim sDepthVideoFileName As String = sVideoName + "\[" + iTotalFrames.ToString("D6") + "]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_GroundTruth.avi"

                If IO.File.Exists(sDepthVideoFileName) OrElse m_CaptureClipData.IgnoreDepth Then
                    ' do not process as output already present
                    Return True
                Else
                    Dim oPlayDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single))) = GetPlayDictionary(sFolderName)
                    Dim oDepthInterpolations As Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)) = GetInterpolations(oPlayDictionary).Item2

                    Dim iColourCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Count()
                    Dim iDepthCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Into Count()

                    If iColourCount > 0 Then
                        m_CaptureClipData.ColourFrames = iColourCount
                        m_CaptureClipData.DepthFrames = iDepthCount
                        m_CaptureClipData.ProcessingFrames = iTotalFrames

                        ' get list of bounding rectangles
                        Dim oBoundingDict As New Dictionary(Of Integer, [Structure].RotatedRect)

                        Using oColourVideoReader As New Accord.Video.FFMPEG.VideoFileReader
                            oColourVideoReader.Open(sColourVideoFileName)
                            For i = 0 To iTotalFrames - 1
                                Dim iFrameNumber As Integer = i + 1

                                Using oColourBitmap As System.Drawing.Bitmap = oColourVideoReader.ReadVideoFrame(i)
                                    Dim oColourMatrix As Matrix(Of Byte) = Converter.BitmapToMatrix(oColourBitmap)

                                    Dim oFaceResults As List(Of Processor.FaceResult) = m_Processor.DetectFaces(oColourMatrix)
                                    If oFaceResults.Count = 1 Then
                                        oBoundingDict.Add(iFrameNumber, oFaceResults.First.Region)
                                    Else
                                        oBoundingDict.Add(iFrameNumber, New [Structure].RotatedRect)
                                    End If

                                    Dim TaskDelegate As Action = Sub()
                                                                     If Not oBoundingDict(iFrameNumber).Size.IsEmpty Then
                                                                         Dim fLineScale As Single = Math.Max(Math.Sqrt((oColourMatrix.Width * oColourMatrix.Width) + (oColourMatrix.Height * oColourMatrix.Height)) / 1000, 2)
                                                                         Dim oVertices As System.Drawing.Point() = oBoundingDict(iFrameNumber).GetVertices.Select(Function(oPoint) New System.Drawing.Point(oPoint.X, oPoint.Y)).ToArray
                                                                         CvInvoke.Polylines(oColourMatrix, oVertices, True, oColorRed, 1 * fLineScale)
                                                                     End If

                                                                     LImage = oColourMatrix

                                                                     oColourMatrix.Dispose()
                                                                 End Sub

                                    CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
                                End Using

                                m_CaptureClipData.ProcessingFrames -= 1
                            Next
                        End Using

                        ' fill in gaps
                        Dim iLastFound As Integer = -1
                        For i = 0 To oBoundingDict.Keys.Count - 2
                            Dim iFrameNumber As Integer = oBoundingDict.Keys(i)

                            ' search forward for next valid rect
                            If iLastFound <> -1 AndAlso oBoundingDict(iFrameNumber).Size.IsEmpty Then
                                Dim iNextFound As Integer = -1
                                Dim oInvalidKeys As New List(Of Integer)

                                For j = i To Math.Min(i + MaxMissingFaces, oBoundingDict.Keys.Count - 1)
                                    Dim iCurrentKey As Integer = oBoundingDict.Keys(j)
                                    If oBoundingDict(iCurrentKey).Size.IsEmpty Then
                                        oInvalidKeys.Add(iCurrentKey)
                                    Else
                                        iNextFound = iCurrentKey
                                        Exit For
                                    End If
                                Next

                                ' next valid rect found
                                If iNextFound <> -1 Then
                                    Dim oLastRect As [Structure].RotatedRect = oBoundingDict(iLastFound)
                                    Dim oNextRect As [Structure].RotatedRect = oBoundingDict(iNextFound)

                                    Dim fInvalidCount As Single = oInvalidKeys.Count + 1
                                    For j As Single = 0 To oInvalidKeys.Count - 1
                                        Dim fCurrentFraction As Single = (j + 1) / fInvalidCount
                                        Dim fCurrentDimension As Single = (oLastRect.Size.Width + oLastRect.Size.Height + ((oNextRect.Size.Width + oNextRect.Size.Height) - (oLastRect.Size.Width + oLastRect.Size.Height)) * fCurrentFraction) / 2
                                        Dim oCurrentRect As New [Structure].RotatedRect(
                                New System.Drawing.PointF(oLastRect.Center.X + (oNextRect.Center.X - oLastRect.Center.X) * fCurrentFraction, oLastRect.Center.Y + (oNextRect.Center.Y - oLastRect.Center.Y) * fCurrentFraction),
                                New System.Drawing.SizeF(fCurrentDimension, fCurrentDimension),
                                oLastRect.Angle
                            )

                                        oBoundingDict(oInvalidKeys(j)) = oCurrentRect
                                    Next
                                End If
                            End If

                            If Not oBoundingDict(iFrameNumber).Size.IsEmpty Then
                                iLastFound = iFrameNumber
                            End If
                        Next

                        ' process depth
                        Dim TaskDelegate2 As Action = Sub()
                                                          TextImageL.Text = "Depth"
                                                          TextImageR.Text = "Ground Truth"
                                                      End Sub

                        CommonFunctions.SafeInvoke(TaskDelegate2, UIDispatcher, True)

                        m_CaptureClipData.ProcessingFrames = oDepthInterpolations.Count

                        Dim OpticalFlowHeight As Integer = 0
                        Dim OpticalFlowWidth As Integer = 0
                        Dim oFlowMatrix As Matrix(Of Single) = Nothing
                        Dim oMapMatrix As Matrix(Of Single) = Nothing
                        Dim bNewFlow As Boolean = False
                        Using oColourVideoReader As New Accord.Video.FFMPEG.VideoFileReader
                            oColourVideoReader.Open(sColourVideoFileName)
                            Using oDepthVideoWriter As New Accord.Video.FFMPEG.VideoFileWriter
                                oDepthVideoWriter.Open(sDepthVideoFileName, SaveVideoWidth, SaveVideoHeight, New Rational(fps), Accord.Video.FFMPEG.VideoCodec.FFV1)

                                Using oElement As Mat = CvInvoke.GetStructuringElement(CvEnum.ElementShape.Rectangle, New System.Drawing.Size(3, 3), New System.Drawing.Point(-1, -1))
                                    For i = 0 To oDepthInterpolations.Keys.Count - 1
                                        Dim iKey As Integer = oDepthInterpolations.Keys(i)
                                        Dim iFrame1 As Integer = oDepthInterpolations(iKey).Item1
                                        Dim iFrame2 As Integer = oDepthInterpolations(iKey).Item2
                                        Dim fFraction As Single = oDepthInterpolations(iKey).Item3
                                        Dim sFileName1 As String = (From oStream In oPlayDictionary(iFrame1) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Select oStream.Item2).First
                                        Dim sFileName2 As String = (From oStream In oPlayDictionary(iFrame2) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Select oStream.Item2).First

                                        Dim oBytes1 As Byte() = IO.File.ReadAllBytes(sFileName1)
                                        Using oMatrix1 As Matrix(Of Single) = MatrixData(Of Single).GetMatrixData(oBytes1).GetMatrix
                                            Dim oBytes2 As Byte() = IO.File.ReadAllBytes(sFileName2)
                                            Using oMatrix2 As Matrix(Of Single) = MatrixData(Of Single).GetMatrixData(oBytes2).GetMatrix
                                                If OpticalFlowHeight = 0 OrElse OpticalFlowWidth = 0 Then
                                                    OpticalFlowHeight = oMatrix1.Height / ScaleDown
                                                    OpticalFlowWidth = oMatrix1.Width / ScaleDown
                                                    oFlowMatrix = New Matrix(Of Single)(OpticalFlowHeight, OpticalFlowWidth, 2)

                                                    Using oMapMatrixX As New Matrix(Of Single)(oMatrix1.Size)
                                                        Using oMapMatrixY As New Matrix(Of Single)(oMatrix1.Size)
                                                            For x = 0 To oMapMatrixX.Width - 1
                                                                oMapMatrixX.GetCol(x).SetValue(x)
                                                            Next
                                                            For y = 0 To oMapMatrixY.Height - 1
                                                                oMapMatrixY.GetRow(y).SetValue(y)
                                                            Next

                                                            Using oVectorMat As New Util.VectorOfMat({oMapMatrixX.Mat, oMapMatrixY.Mat})
                                                                oMapMatrix = New Matrix(Of Single)(oMatrix1.Height, oMatrix1.Width, 2)
                                                                CvInvoke.Merge(oVectorMat, oMapMatrix)
                                                            End Using
                                                        End Using
                                                    End Using

                                                    bNewFlow = True
                                                Else
                                                    bNewFlow = False
                                                End If
                                                Using oInterMatrix As Matrix(Of Single) = GetFlowFrame(oMatrix1, oMatrix2, oFlowMatrix, oMapMatrix, bNewFlow, fFraction)
                                                    Dim iFrameNumber As Integer = i + 1

                                                    Using oDepthMatrix As New Matrix(Of Single)(SaveVideoHeight, SaveVideoWidth)
                                                        CvInvoke.Resize(oInterMatrix, oDepthMatrix, oDepthMatrix.Size, 0, 0, CvEnum.Inter.Cubic)

                                                        Using oColourBitmap As System.Drawing.Bitmap = oColourVideoReader.ReadVideoFrame(i)
                                                            Using oColourMatrix As Matrix(Of Byte) = Converter.BitmapToMatrix(oColourBitmap)
                                                                Using oMaskExpanded As New Matrix(Of Byte)(oDepthMatrix.Height + 2, oDepthMatrix.Width + 2)
                                                                    If Not oBoundingDict(iFrameNumber).Size.IsEmpty Then
                                                                        Dim oBounds As New System.Drawing.Rectangle
                                                                        CvInvoke.FloodFill(oDepthMatrix, oMaskExpanded, New System.Drawing.Point(oBoundingDict(iFrameNumber).Center.X, oBoundingDict(iFrameNumber).Center.Y), New [Structure].MCvScalar(255), oBounds, New [Structure].MCvScalar(DepthFillTolerance), New [Structure].MCvScalar(DepthFillTolerance), CvEnum.Connectivity.FourConnected, CvEnum.FloodFillType.MaskOnly)

                                                                        Using oMask As Matrix(Of Byte) = oMaskExpanded.GetSubRect(New System.Drawing.Rectangle(1, 1, oDepthMatrix.Width, oDepthMatrix.Height))
                                                                            Dim fMax As Double = 0
                                                                            Dim fMin As Double = 0
                                                                            CvInvoke.MinMaxLoc(oMask, fMin, fMax, Nothing, Nothing)
                                                                            If fMin = fMax Then
                                                                                oMask.SetZero()
                                                                            Else
                                                                                CvInvoke.Normalize(oMask, oMask, 0, 1, CvEnum.NormType.MinMax)
                                                                            End If

                                                                            ' grabcut processing
                                                                            Using oMarginMask As Matrix(Of Byte) = oMask.Clone
                                                                                Using oSelectionMask As Matrix(Of Byte) = oMask.Clone
                                                                                    CvInvoke.MorphologyEx(oMarginMask, oMarginMask, CvEnum.MorphOp.Dilate, CvInvoke.GetStructuringElement(CvEnum.ElementShape.Rectangle, New System.Drawing.Size(3, 3), New System.Drawing.Point(-1, -1)), New System.Drawing.Point(-1, -1), WatershedMarginOuter, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(Byte.MinValue))
                                                                                    CvInvoke.MorphologyEx(oSelectionMask, oSelectionMask, CvEnum.MorphOp.Erode, CvInvoke.GetStructuringElement(CvEnum.ElementShape.Rectangle, New System.Drawing.Size(3, 3), New System.Drawing.Point(-1, -1)), New System.Drawing.Point(-1, -1), WatershedMarginInner, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(Byte.MinValue))
                                                                                    oMarginMask.SetValue(0, oSelectionMask)

                                                                                    ' selection background -> possible background (2), foreground -> possible foreground
                                                                                    oSelectionMask.SetZero()
                                                                                    oSelectionMask.SetValue(2, oMarginMask)
                                                                                    CvInvoke.Add(oMask, oSelectionMask, oMask)

                                                                                    Using oVector As New Util.VectorOfMat
                                                                                        Using oCLAHEMatrix As New Matrix(Of Byte)(oColourMatrix.Height, oColourMatrix.Width, 3)
                                                                                            ' run CLAHE on luminance channel before watershed
                                                                                            CvInvoke.CvtColor(oColourMatrix, oCLAHEMatrix, CvEnum.ColorConversion.Bgr2Lab)
                                                                                            CvInvoke.Split(oCLAHEMatrix, oVector)
                                                                                            CvInvoke.CLAHE(oVector(0), 2, New System.Drawing.Size(8, 8), oVector(0))
                                                                                            CvInvoke.Merge(oVector, oCLAHEMatrix)
                                                                                            CvInvoke.CvtColor(oCLAHEMatrix, oCLAHEMatrix, CvEnum.ColorConversion.Lab2Bgr)

                                                                                            Using oBackgroundModel As New Mat
                                                                                                Using oForegroundModel As New Mat
                                                                                                    CvInvoke.GrabCut(oCLAHEMatrix, oMask, System.Drawing.Rectangle.Empty, oBackgroundModel, oForegroundModel, 1, CvEnum.GrabcutInitType.InitWithMask)
                                                                                                End Using
                                                                                            End Using
                                                                                        End Using
                                                                                    End Using

                                                                                    CvInvoke.Subtract(oMask, oSelectionMask, oMask)

                                                                                    ' fill small gaps
                                                                                    Using oContours As New Util.VectorOfVectorOfPoint()
                                                                                        CvInvoke.FindContours(oMask, oContours, Nothing, CvEnum.RetrType.External, CvEnum.ChainApproxMethod.ChainApproxSimple)
                                                                                        oMask.SetZero()
                                                                                        CvInvoke.DrawContours(oMask, oContours, -1, New [Structure].MCvScalar(255), -1)
                                                                                        CvInvoke.MorphologyEx(oMask, oMask, CvEnum.MorphOp.Close, CvInvoke.GetStructuringElement(CvEnum.ElementShape.Rectangle, New System.Drawing.Size(3, 3), New System.Drawing.Point(-1, -1)), New System.Drawing.Point(-1, -1), 1, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(Byte.MinValue))
                                                                                    End Using
                                                                                End Using
                                                                            End Using

                                                                            ' remove any black areas at the bottom of the mask
                                                                            Using oInverseMask As Matrix(Of Byte) = oMask.Clone
                                                                                CvInvoke.BitwiseNot(oInverseMask, oInverseMask)

                                                                                Using oLabels As New Matrix(Of Integer)(oMask.Size)
                                                                                    Dim iComponents As Integer = CvInvoke.ConnectedComponents(oInverseMask, oLabels)
                                                                                    If iComponents > 2 Then
                                                                                        ' multiple black areas, need to check
                                                                                        Using oCompare As New Matrix(Of Integer)(oMask.Size)
                                                                                            Using oComponentMask As New Matrix(Of Byte)(oMask.Size)
                                                                                                Using oDilatedMask As New Matrix(Of Byte)(oMask.Size)
                                                                                                    For j = 1 To iComponents - 1
                                                                                                        oCompare.SetValue(j)
                                                                                                        CvInvoke.Compare(oLabels, oCompare, oComponentMask, CvEnum.CmpType.Equal)

                                                                                                        ' get a mask containing just a 1 pixel rim around the component
                                                                                                        CvInvoke.Dilate(oComponentMask, oDilatedMask, oElement, New System.Drawing.Point(-1, -1), 1, CvEnum.BorderType.Constant, New [Structure].MCvScalar(0))
                                                                                                        oDilatedMask.SetValue(0, oComponentMask)

                                                                                                        ' check to see if the rim overlaps with no pixels in the inverse mask
                                                                                                        ' ie. black area surrounded completely by white
                                                                                                        CvInvoke.BitwiseAnd(oInverseMask, oDilatedMask, oDilatedMask)
                                                                                                        Dim iRimCount As Integer = CvInvoke.CountNonZero(oDilatedMask)
                                                                                                        If iRimCount = 0 Then
                                                                                                            ' check to see if the component is confined to the bottom of the image
                                                                                                            oComponentMask.CopyTo(oDilatedMask)
                                                                                                            oDilatedMask.GetSubRect(New System.Drawing.Rectangle(0, oMask.Height - LowerRim, oMask.Width, LowerRim)).SetZero()
                                                                                                            Dim iNonBottomCount As Integer = CvInvoke.CountNonZero(oDilatedMask)
                                                                                                            If iNonBottomCount = 0 Then
                                                                                                                ' component is confined to LowerRim pixels from the image bottom
                                                                                                                oMask.SetValue(255, oComponentMask)
                                                                                                            End If
                                                                                                        End If
                                                                                                    Next
                                                                                                End Using
                                                                                            End Using
                                                                                        End Using
                                                                                    End If
                                                                                End Using
                                                                            End Using

                                                                            Using oResizedMask As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth)
                                                                                CvInvoke.Resize(oMask, oResizedMask, oResizedMask.Size, 0, 0, CvEnum.Inter.Lanczos4)
                                                                                CvInvoke.Threshold(oResizedMask, oResizedMask, 127, 255, CvEnum.ThresholdType.Binary)
                                                                                CvInvoke.MorphologyEx(oResizedMask, oResizedMask, CvEnum.MorphOp.Open, oElement, New System.Drawing.Point(-1, -1), 1, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(0))
                                                                                CvInvoke.MorphologyEx(oResizedMask, oResizedMask, CvEnum.MorphOp.Close, oElement, New System.Drawing.Point(-1, -1), 1, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(0))
                                                                                Using oSmallBitmap As System.Drawing.Bitmap = Converter.MatToBitmap(oResizedMask.Mat, 300)
                                                                                    oDepthVideoWriter.WriteVideoFrame(oSmallBitmap)
                                                                                End Using
                                                                            End Using

                                                                            Dim oDepthFrameSingle As Matrix(Of Single) = oInterMatrix.Clone
                                                                            Dim oDepthFrameMask As Matrix(Of Byte) = oMask.Clone
                                                                            Dim TaskDelegate As Action = Sub()
                                                                                                             Dim oColourImage As Matrix(Of Byte) = Nothing
                                                                                                             Converter.CreateDepthMap(oColourImage, oDepthFrameSingle, CvEnum.ColorMapType.Jet)
                                                                                                             LImage = oColourImage
                                                                                                             oColourImage.Dispose()
                                                                                                             oDepthFrameSingle.Dispose()

                                                                                                             RImage = oDepthFrameMask
                                                                                                             oDepthFrameMask.Dispose()
                                                                                                         End Sub

                                                                            CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
                                                                        End Using

                                                                    Else
                                                                        ' write empty frame
                                                                        Using oSmallBitmap As New System.Drawing.Bitmap(SaveVideoWidth, SaveVideoHeight, System.Drawing.Imaging.PixelFormat.Format8bppIndexed)
                                                                            oDepthVideoWriter.WriteVideoFrame(oSmallBitmap)
                                                                        End Using
                                                                    End If
                                                                End Using
                                                            End Using
                                                        End Using
                                                    End Using
                                                End Using
                                            End Using
                                        End Using

                                        m_CaptureClipData.ProcessingFrames -= 1
                                    Next
                                End Using
                            End Using
                        End Using
                        If CommonFunctions.MatrixNotNothing(oFlowMatrix) Then
                            oFlowMatrix.Dispose()
                            oFlowMatrix = Nothing
                        End If
                        If CommonFunctions.MatrixNotNothing(oMapMatrix) Then
                            oMapMatrix.Dispose()
                            oMapMatrix = Nothing
                        End If

                        Return True
                    Else
                        Return False
                    End If
                End If
            Else
                Return False
            End If
        Else
            Return False
        End If
    End Function
    Private Function CreateNoise() As Boolean
        ' create noise data
        Dim oClipDataList As List(Of ClipData) = (From oClipData In ClipDataStore Where oClipData.Name = NoisePrimary Select oClipData).ToList
        If oClipDataList.Count > 0 Then
            m_CaptureClipData = oClipDataList.First
            Dim sSTDFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name)
            Dim sSTDVideoName As String = sSTDFolderName + "\" + VideoFolder
            If IO.Directory.Exists(sSTDVideoName) Then
                ' do not process when depth is ignored
                Dim oDirectoryInfo As New IO.DirectoryInfo(sSTDVideoName)
                Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_Colour.avi").ToList
                If oFileInfoList.Count > 0 Then
                    Dim oFileInfo As IO.FileInfo = oFileInfoList.First
                    Dim sColourVideoFileName As String = oFileInfo.FullName
                    Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
                    Dim sTotalFrames As String = Mid(sName, 2, 6)
                    Dim iTotalFrames As Integer = 0
                    If Integer.TryParse(sTotalFrames, iTotalFrames) Then
                        oFileInfoList = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_GroundTruth.avi").ToList
                        If oFileInfoList.Count > 0 Then
                            oFileInfo = oFileInfoList.First
                            Dim sGroundTruthVideoFileName As String = oFileInfo.FullName

                            Dim oFolderNames As New Dictionary(Of String, String)
                            Dim oColourFileNames As New Dictionary(Of String, String)
                            Dim oGroundTruthFileNames As New Dictionary(Of String, String)
                            For Each sGAU In m_Special(SpecialEnum.GAU)
                                oFolderNames.Add(sGAU, Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(sGAU) + "\" + VideoFolder)
                                oColourFileNames.Add(sGAU, oFolderNames(sGAU) + "\[" + iTotalFrames.ToString("D6") + "]_" + sGAU + "_Colour.avi")
                                oGroundTruthFileNames.Add(sGAU, oFolderNames(sGAU) + "\[" + iTotalFrames.ToString("D6") + "]_" + sGAU + "_GroundTruth.avi")
                            Next
                            For Each sUNI In m_Special(SpecialEnum.UNI)
                                oFolderNames.Add(sUNI, Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(sUNI) + "\" + VideoFolder)
                                oColourFileNames.Add(sUNI, oFolderNames(sUNI) + "\[" + iTotalFrames.ToString("D6") + "]_" + sUNI + "_Colour.avi")
                                oGroundTruthFileNames.Add(sUNI, oFolderNames(sUNI) + "\[" + iTotalFrames.ToString("D6") + "]_" + sUNI + "_GroundTruth.avi")
                            Next
                            For Each sINI In m_Special(SpecialEnum.INI)
                                oFolderNames.Add(sINI, Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(sINI) + "\" + VideoFolder)
                                oColourFileNames.Add(sINI, oFolderNames(sINI) + "\[" + iTotalFrames.ToString("D6") + "]_" + sINI + "_Colour.avi")
                                oGroundTruthFileNames.Add(sINI, oFolderNames(sINI) + "\[" + iTotalFrames.ToString("D6") + "]_" + sINI + "_GroundTruth.avi")
                            Next

                            ' create noise files
                            Dim oVideoWriters As New Dictionary(Of String, Accord.Video.FFMPEG.VideoFileWriter)
                            For Each sGAU In m_Special(SpecialEnum.GAU)
                                If Not IO.Directory.Exists(oFolderNames(sGAU)) Then
                                    IO.Directory.CreateDirectory(oFolderNames(sGAU))
                                End If
                                If Not IO.File.Exists(oColourFileNames(sGAU)) Then
                                    oVideoWriters.Add(sGAU, New Accord.Video.FFMPEG.VideoFileWriter)
                                    oVideoWriters(sGAU).Open(oColourFileNames(sGAU), SaveVideoWidth, SaveVideoHeight, New Rational(FPS), Accord.Video.FFMPEG.VideoCodec.MPEG4, 16000000)
                                End If
                            Next
                            For Each sUNI In m_Special(SpecialEnum.UNI)
                                If Not IO.Directory.Exists(oFolderNames(sUNI)) Then
                                    IO.Directory.CreateDirectory(oFolderNames(sUNI))
                                End If
                                If Not IO.File.Exists(oColourFileNames(sUNI)) Then
                                    oVideoWriters.Add(sUNI, New Accord.Video.FFMPEG.VideoFileWriter)
                                    oVideoWriters(sUNI).Open(oColourFileNames(sUNI), SaveVideoWidth, SaveVideoHeight, New Rational(FPS), Accord.Video.FFMPEG.VideoCodec.MPEG4, 16000000)
                                End If
                            Next

                            If oVideoWriters.Count > 0 Then
                                Using oColourVideoReader As New Accord.Video.FFMPEG.VideoFileReader
                                    oColourVideoReader.Open(sColourVideoFileName)

                                    m_CaptureClipData.ProcessingFrames = iTotalFrames

                                    For i = 0 To iTotalFrames - 1
                                        Dim iFrameNumber As Integer = i + 1

                                        Using oColourBitmap As System.Drawing.Bitmap = oColourVideoReader.ReadVideoFrame(i)
                                            For Each sGAU In m_Special(SpecialEnum.GAU)
                                                If oVideoWriters.ContainsKey(sGAU) Then
                                                    Using oNoiseBitmap As System.Drawing.Bitmap = oColourBitmap.Clone
                                                        Dim oGenerator As New Accord.Statistics.Distributions.Univariate.NormalDistribution(0.0, 10.0)
                                                        Dim oAdditiveNoise As New Accord.Imaging.Filters.AdditiveNoise(oGenerator)
                                                        oAdditiveNoise.ApplyInPlace(oNoiseBitmap)
                                                        oVideoWriters(sGAU).WriteVideoFrame(oNoiseBitmap)
                                                    End Using
                                                End If
                                            Next
                                            For Each sUNI In m_Special(SpecialEnum.UNI)
                                                If oVideoWriters.ContainsKey(sUNI) Then
                                                    Using oNoiseBitmap As System.Drawing.Bitmap = oColourBitmap.Clone
                                                        Dim oSaltAndPepperNoise As New Accord.Imaging.Filters.SaltAndPepperNoise(5)
                                                        oSaltAndPepperNoise.ApplyInPlace(oNoiseBitmap)
                                                        oVideoWriters(sUNI).WriteVideoFrame(oNoiseBitmap)
                                                    End Using
                                                End If
                                            Next
                                        End Using

                                        m_CaptureClipData.ProcessingFrames -= 1
                                    Next
                                End Using

                                For Each sGAU In m_Special(SpecialEnum.GAU)
                                    If oVideoWriters.ContainsKey(sGAU) Then
                                        oVideoWriters(sGAU).Dispose()
                                        oVideoWriters(sGAU) = Nothing
                                    End If
                                Next
                                For Each sUNI In m_Special(SpecialEnum.UNI)
                                    If oVideoWriters.ContainsKey(sUNI) Then
                                        oVideoWriters(sUNI).Dispose()
                                        oVideoWriters(sUNI) = Nothing
                                    End If
                                Next
                            End If

                            ' copy ground truth files
                            Dim oIndices As New Dictionary(Of String, List(Of Integer))
                            For Each sGAU In m_Special(SpecialEnum.GAU)
                                If Not IO.File.Exists(oGroundTruthFileNames(sGAU)) Then
                                    IO.File.Copy(sGroundTruthVideoFileName, oGroundTruthFileNames(sGAU))
                                End If
                                oIndices.Add(sGAU, (From iIndex In Enumerable.Range(0, ClipDataStore.Count) Where ClipDataStore(iIndex).Name = sGAU Select iIndex).ToList)
                            Next
                            For Each sUNI In m_Special(SpecialEnum.UNI)
                                If Not IO.File.Exists(oGroundTruthFileNames(sUNI)) Then
                                    IO.File.Copy(sGroundTruthVideoFileName, oGroundTruthFileNames(sUNI))
                                End If
                                oIndices.Add(sUNI, (From iIndex In Enumerable.Range(0, ClipDataStore.Count) Where ClipDataStore(iIndex).Name = sUNI Select iIndex).ToList)
                            Next

                            ' copy initialisation videos
                            For Each sINI In m_Special(SpecialEnum.INI)
                                If Not IO.Directory.Exists(oFolderNames(sINI)) Then
                                    IO.Directory.CreateDirectory(oFolderNames(sINI))
                                End If
                                If Not IO.File.Exists(oColourFileNames(sINI)) Then
                                    IO.File.Copy(sColourVideoFileName, oColourFileNames(sINI))
                                End If
                                If Not IO.File.Exists(oGroundTruthFileNames(sINI)) Then
                                    IO.File.Copy(sGroundTruthVideoFileName, oGroundTruthFileNames(sINI))
                                End If
                                oIndices.Add(sINI, (From iIndex In Enumerable.Range(0, ClipDataStore.Count) Where ClipDataStore(iIndex).Name = sINI Select iIndex).ToList)
                            Next
                            For Each sSpecial In oIndices.Keys
                                If oIndices(sSpecial).Count > 0 Then
                                    ClipDataStore(oIndices(sSpecial).First).ColourFrames = iTotalFrames
                                    ClipDataStore(oIndices(sSpecial).First).DepthFrames = iTotalFrames
                                    ClipDataStore(oIndices(sSpecial).First).ClipEntrance = m_CaptureClipData.ClipEntrance
                                    ClipDataStore(oIndices(sSpecial).First).ClipSeated = m_CaptureClipData.ClipSeated
                                End If
                            Next

                            Return True
                        Else
                            Return False
                        End If
                    Else
                        Return False
                    End If
                Else
                    Return False
                End If
            Else
                Return False
            End If

            m_CaptureClipData = Nothing
        Else
            Return False
        End If
    End Function
    Private Function CutClip(ByVal sFolderName As String) As Boolean
        ' trims video clip to total length of ProcessClip
        ' allows PreStart seconds of background without subject
        ' if zero seconds, then trim video after seated
        ' video analysis ProcessBegin seconds after seated
        ' do not trim if entrance and seated are both zero, or seated less or equal to entrance

        ' if depth data is ignored, then accept clips where the seated time is greater than PreStart seconds
        Dim sVideoName As String = sFolderName + "\" + VideoFolder
        If IO.Directory.Exists(sVideoName) Then
            If (m_CaptureClipData.ClipEntrance < m_CaptureClipData.ClipSeated AndAlso m_CaptureClipData.ClipEntrance > 0) OrElse m_CaptureClipData.IgnoreDepth Then
                Dim iTargetFrames As Integer = FPS * ProcessClip

                Dim oDirectoryInfo As New IO.DirectoryInfo(sVideoName)
                Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_Colour.avi").ToList
                If oFileInfoList.Count > 0 Then
                    Dim oFileInfo As IO.FileInfo = oFileInfoList.First
                    Dim sColourVideoFileName As String = oFileInfo.FullName
                    Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
                    Dim sTotalFrames As String = Mid(sName, 2, 6)
                    Dim iTotalFrames As Integer = 0
                    If Integer.TryParse(sTotalFrames, iTotalFrames) Then
                        oFileInfoList = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_GroundTruth.avi").ToList
                        If oFileInfoList.Count > 0 OrElse m_CaptureClipData.IgnoreDepth Then
                            Dim sGroundTruthVideoFileName As String = String.Empty
                            If Not m_CaptureClipData.IgnoreDepth Then
                                oFileInfo = oFileInfoList.First
                                sGroundTruthVideoFileName = oFileInfo.FullName
                            End If

                            Dim iTrimFront As Integer = 0
                            If m_CaptureClipData.Name = "GHO" Then
                                iTrimFront = m_CaptureClipData.ClipSeated - (FPS * (m_CaptureClipData.PreStart - ProcessGhost))
                            ElseIf m_CaptureClipData.PreStart = PreStartEnum.None Then
                                iTrimFront = m_CaptureClipData.ClipSeated
                            ElseIf m_CaptureClipData.IgnoreDepth Then
                                iTrimFront = Math.Max(0, m_CaptureClipData.ClipEntrance - (FPS * m_CaptureClipData.PreStart))
                            Else
                                iTrimFront = m_CaptureClipData.ClipEntrance - (FPS * m_CaptureClipData.PreStart)
                            End If

                            ' start trim process only if enough frames are available
                            If iTrimFront + iTargetFrames <= iTotalFrames Then
                                m_CaptureClipData.ProcessingFrames = iTargetFrames

                                ' try to load selections
                                Dim oSelections As Dictionary(Of Integer, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum))) = Nothing
                                Dim sSelectionFileName As String = sFolderName + "\" + SelectionFileName
                                If IO.File.Exists(sSelectionFileName) Then
                                    oSelections = CommonFunctions.DeserializeDataContractFile(Of Dictionary(Of Integer, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum))))(sSelectionFileName)
                                    If (Not IsNothing(oSelections)) AndAlso oSelections.Count <> iTotalFrames Then
                                        oSelections = Nothing
                                    End If
                                End If

                                Dim sCutColourVideoFileName As String = sVideoName + "\[" + iTargetFrames.ToString("D6") + "]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_Colour_Cut.avi"
                                Dim sCutGroundTruthVideoFileName As String = sVideoName + "\[" + iTargetFrames.ToString("D6") + "]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_GroundTruth_Cut.avi"
                                Dim bCutColorPresent As Boolean = IO.File.Exists(sCutColourVideoFileName)
                                Dim bCutGroundTruthPresent As Boolean = IO.File.Exists(sCutGroundTruthVideoFileName)
                                Using oColourVideoReader As New Accord.Video.FFMPEG.VideoFileReader
                                    Using oGroundTruthVideoReader As New Accord.Video.FFMPEG.VideoFileReader
                                        Using oCutColourVideoWriter As New Accord.Video.FFMPEG.VideoFileWriter
                                            Using oCutGroundTruthVideoWriter As New Accord.Video.FFMPEG.VideoFileWriter
                                                If Not bCutColorPresent Then
                                                    oColourVideoReader.Open(sColourVideoFileName)
                                                    oCutColourVideoWriter.Open(sCutColourVideoFileName, SaveVideoWidth, SaveVideoHeight, New Rational(FPS), Accord.Video.FFMPEG.VideoCodec.MPEG4, 4000000)
                                                End If
                                                If (Not m_CaptureClipData.IgnoreDepth) AndAlso (Not bCutGroundTruthPresent) Then
                                                    oGroundTruthVideoReader.Open(sGroundTruthVideoFileName)
                                                    oCutGroundTruthVideoWriter.Open(sCutGroundTruthVideoFileName, SaveVideoWidth, SaveVideoHeight, New Rational(FPS), Accord.Video.FFMPEG.VideoCodec.FFV1)
                                                End If

                                                Dim bWriteColour As Boolean = Not bCutColorPresent
                                                Dim bWriteGroundTruth As Boolean = (Not m_CaptureClipData.IgnoreDepth) AndAlso (Not bCutGroundTruthPresent)
                                                For i = 0 To iTargetFrames - 1
                                                    Using oColourBitmap As System.Drawing.Bitmap = If(bWriteColour, oColourVideoReader.ReadVideoFrame(i + iTrimFront), New System.Drawing.Bitmap(SaveVideoWidth, SaveVideoHeight))
                                                        Using oGroundTruthBitmap As System.Drawing.Bitmap = If(bWriteGroundTruth, oGroundTruthVideoReader.ReadVideoFrame(i + iTrimFront), New System.Drawing.Bitmap(SaveVideoWidth, SaveVideoHeight))
                                                            If bWriteColour Then
                                                                oCutColourVideoWriter.WriteVideoFrame(oColourBitmap, i)
                                                            End If
                                                            If bWriteGroundTruth Then
                                                                If IsNothing(oSelections) OrElse oSelections(i + iTrimFront).Count = 0 Then
                                                                    oCutGroundTruthVideoWriter.WriteVideoFrame(oGroundTruthBitmap, i)
                                                                Else
                                                                    ' run grabcut
                                                                    Using oMatrixColour As Matrix(Of Byte) = Converter.BitmapToMatrix(oColourBitmap)
                                                                        Using oMatrixGroundTruth As Matrix(Of Byte) = Converter.BitmapToMatrix(oGroundTruthBitmap)
                                                                            Using oMatrixMask As New Matrix(Of Byte)(oMatrixGroundTruth.Size)
                                                                                CvInvoke.CvtColor(oMatrixGroundTruth, oMatrixMask, CvEnum.ColorConversion.Bgr2Gray)
                                                                                Using oGrabCutMatrix As Matrix(Of Byte) = GrabCut(oMatrixColour, oMatrixMask, oSelections(i + iTrimFront))
                                                                                    Using oGrabCutBitmap As System.Drawing.Bitmap = Converter.MatToBitmap(oGrabCutMatrix.Mat, 300)
                                                                                        oCutGroundTruthVideoWriter.WriteVideoFrame(oGrabCutBitmap, i)
                                                                                    End Using
                                                                                End Using
                                                                            End Using
                                                                        End Using
                                                                    End Using
                                                                End If
                                                            End If
                                                        End Using
                                                    End Using

                                                    m_CaptureClipData.ProcessingFrames -= 1
                                                Next
                                            End Using
                                        End Using
                                    End Using
                                End Using

                                Return True
                            Else
                                Return False
                            End If
                        Else
                            Return False
                        End If
                    Else
                        Return False
                    End If
                Else
                    Return False
                End If
            Else
                Return False
            End If
        Else
            Return False
        End If
    End Function
    Private Function GetSegmented(ByVal sVideoName As String) As Boolean
        ' calculates the accuracy across all algorithms
        m_CaptureClipData.SetAccuracy()

        Dim oDirectoryInfo As New IO.DirectoryInfo(sVideoName)
        Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_Colour_Cut.avi").ToList
        If oFileInfoList.Count > 0 Then
            Dim oSegEnumBag As New ConcurrentBag(Of ClipData.SegTypeEnum)([Enum].GetValues(GetType(ClipData.SegTypeEnum)).Cast(Of ClipData.SegTypeEnum))
            m_CaptureClipData.ProcessingFrames = oSegEnumBag.Count

            Dim oFileInfo As IO.FileInfo = oFileInfoList.First
            Dim sColourVideoFileName As String = oFileInfo.FullName

            Dim oActionList As New List(Of Tuple(Of Action(Of Object), Object))
            Dim oAction As Action(Of Object) = Sub(oParam As Integer)
                                                   Dim oSegType As ClipData.SegTypeEnum = Nothing
                                                   Do While oSegEnumBag.TryTake(oSegType)
                                                       Dim sSegName As String = [Enum].GetName(GetType(ClipData.SegTypeEnum), oSegType)
                                                       Dim sSegTypeFileName As String = sVideoName + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_SEG_" + sSegName + "_Cut.avi"
                                                       If (Not oSegType = ClipData.SegTypeEnum.None) AndAlso (Not IO.File.Exists(sSegTypeFileName)) Then
                                                           Try
                                                               initSegmenter(oParam, oSegType)

                                                               Dim bValid As Boolean = validbgs(oParam)
                                                               If bValid AndAlso IO.File.Exists(sColourVideoFileName) Then
                                                                   Dim TaskDelegate2 As Action = Sub()
                                                                                                     Select Case oParam
                                                                                                         Case 0
                                                                                                             TextTimer1.Text = sSegName
                                                                                                         Case 1
                                                                                                             TextTimer2.Text = sSegName
                                                                                                         Case 2
                                                                                                             TextTimer3.Text = sSegName
                                                                                                         Case 3
                                                                                                             TextTimer4.Text = sSegName
                                                                                                     End Select
                                                                                                 End Sub

                                                                   CommonFunctions.SafeInvoke(TaskDelegate2, UIDispatcher, True)

                                                                   Using oColourReader As New VideoCapture(sColourVideoFileName)
                                                                       Using oVideoWriter As New VideoWriter(sSegTypeFileName, FourCC.FFV1, FPS, New System.Drawing.Size(SaveVideoWidth, SaveVideoHeight), True)
                                                                           Dim iFrameNumber As Integer = 0
                                                                           Using oColourMatrix As New Matrix(Of Byte)(oColourReader.Height, oColourReader.Width, 3)
                                                                               Dim iTotalDuration As Long = 0

                                                                               Do While oColourReader.Grab AndAlso oColourReader.Retrieve(oColourMatrix)
                                                                                   iFrameNumber += 1
                                                                                   Using oSegmentedMatrix As Matrix(Of Byte) = Segment(oParam, oColourMatrix, iTotalDuration)
                                                                                       oVideoWriter.Write(oSegmentedMatrix.Mat)
                                                                                   End Using
                                                                               Loop

                                                                               Dim oResults As ClipData.SplitResults = m_CaptureClipData.Accuracy(oSegType)
                                                                               oResults.ProcessTime = TimeSpan.FromMilliseconds(iTotalDuration / 1000)
                                                                               m_CaptureClipData.Accuracy(oSegType) = oResults
                                                                           End Using
                                                                       End Using
                                                                   End Using

                                                                   SaveClip(oParam)

                                                                   Dim TaskDelegate3 As Action = Sub()
                                                                                                     Select Case oParam
                                                                                                         Case 0
                                                                                                             TextTimer1.Text = String.Empty
                                                                                                         Case 1
                                                                                                             TextTimer2.Text = String.Empty
                                                                                                         Case 2
                                                                                                             TextTimer3.Text = String.Empty
                                                                                                         Case 3
                                                                                                             TextTimer4.Text = String.Empty
                                                                                                     End Select
                                                                                                 End Sub

                                                                   CommonFunctions.SafeInvoke(TaskDelegate3, UIDispatcher, True)
                                                               End If
                                                           Catch ex As Exception
                                                               ' erase file and record
                                                               If IO.File.Exists(sSegTypeFileName) Then
                                                                   IO.File.Delete(sSegTypeFileName)
                                                               End If
                                                               m_CaptureClipData.Accuracy(oSegType) = ClipData.SplitResults.GetEmpty
                                                           Finally
                                                               exitSegmenter(oParam)
                                                           End Try
                                                       End If

                                                       Threading.Interlocked.Decrement(m_CaptureClipData.ProcessingFrames)
                                                   Loop
                                               End Sub

            For i = 0 To SegTasks - 1
                oActionList.Add(New Tuple(Of Action(Of Object), Object)(oAction, i))
            Next

            CommonFunctions.ProtectedRunTasks(oActionList)

            Return True
        Else
            Return False
        End If
    End Function
    Private Sub CalculateAccuracy(ByVal sVideoName As String)
        ' final accuracy count
        Dim oDirectoryInfo As New IO.DirectoryInfo(sVideoName)
        Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_GroundTruth_Cut.avi").ToList
        If oFileInfoList.Count > 0 OrElse m_CaptureClipData.IgnoreDepth Then
            Dim sGroundTruthVideoFileName As String = String.Empty
            If Not m_CaptureClipData.IgnoreDepth Then
                Dim oFileInfo As IO.FileInfo = oFileInfoList.First
                sGroundTruthVideoFileName = oFileInfo.FullName
            End If

            Dim iFrameStart As Integer = ProcessBegin * FPS
            Dim iFrameEnd As Integer = (ProcessBegin + ProcessLength) * FPS

            oFileInfoList = oDirectoryInfo.EnumerateFiles(CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_SEG_*_Cut.avi").ToList
            If oFileInfoList.Count > 0 Then
                m_CaptureClipData.SetAccuracy()

                Using oCountMatrix As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth)
                    Using oGroundTruthVideoReader As New Accord.Video.FFMPEG.VideoFileReader
                        If Not m_CaptureClipData.IgnoreDepth Then
                            oGroundTruthVideoReader.Open(sGroundTruthVideoFileName)
                        End If

                        m_CaptureClipData.ProcessingFrames = oFileInfoList.Count
                        For Each oFileInfo In oFileInfoList
                            Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
                            Dim sPrefix As String = CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_SEG_"
                            Dim sSegName As String = Mid(sName, Len(sPrefix) + 1, Len(sName) - Len(sPrefix) - Len("_Cut"))

                            Dim oSegType As ClipData.SegTypeEnum = [Enum].Parse(GetType(ClipData.SegTypeEnum), sSegName)
                            If m_CaptureClipData.Accuracy.ContainsKey(oSegType) AndAlso m_CaptureClipData.Accuracy(oSegType).Empty Then
                                ' only process if result is empty
                                Dim oSplitResults As ClipData.SplitResults = m_CaptureClipData.Accuracy(oSegType)
                                Using oSegmentVideoReader As New Accord.Video.FFMPEG.VideoFileReader
                                    oSegmentVideoReader.Open(oFileInfo.FullName)

                                    For i = iFrameStart To iFrameEnd
                                        Using oGroundTruthBitmap As System.Drawing.Bitmap = If(m_CaptureClipData.IgnoreDepth, New System.Drawing.Bitmap(SaveVideoWidth, SaveVideoHeight), oGroundTruthVideoReader.ReadVideoFrame(i))
                                            Using oSegmentBitmap As System.Drawing.Bitmap = oSegmentVideoReader.ReadVideoFrame(i)
                                                Using oGroundTruthColourMatrix As Matrix(Of Byte) = If(m_CaptureClipData.IgnoreDepth, New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth, 3), Converter.BitmapToMatrix(oGroundTruthBitmap))
                                                    If m_CaptureClipData.IgnoreDepth Then
                                                        oGroundTruthColourMatrix.SetZero()
                                                    End If
                                                    Using oSegmentColourMatrix As Matrix(Of Byte) = Converter.BitmapToMatrix(oSegmentBitmap)
                                                        Using oGroundTruthMatrix As New Matrix(Of Byte)(oGroundTruthColourMatrix.Size)
                                                            Using oSegmentMatrix As New Matrix(Of Byte)(oSegmentColourMatrix.Size)
                                                                ' convert to single channel matrices
                                                                CvInvoke.CvtColor(oGroundTruthColourMatrix, oGroundTruthMatrix, CvEnum.ColorConversion.Bgr2Gray)
                                                                CvInvoke.CvtColor(oSegmentColourMatrix, oSegmentMatrix, CvEnum.ColorConversion.Bgr2Gray)
                                                                CvInvoke.Threshold(oGroundTruthMatrix, oGroundTruthMatrix, 127, 255, CvEnum.ThresholdType.Binary)
                                                                CvInvoke.Threshold(oSegmentMatrix, oSegmentMatrix, 127, 255, CvEnum.ThresholdType.Binary)

                                                                Dim iPositiveCount As Integer = CvInvoke.CountNonZero(oSegmentMatrix)
                                                                Dim iNegativeCount As Integer = (oSegmentMatrix.Width * oSegmentMatrix.Height) - iPositiveCount

                                                                Dim oSplitResult As New ClipData.SplitResults.SplitResult()
                                                                With oSplitResult
                                                                    ' true positive
                                                                    CvInvoke.BitwiseAnd(oGroundTruthMatrix, oSegmentMatrix, oCountMatrix)
                                                                    Dim iTruePositive = CvInvoke.CountNonZero(oCountMatrix)
                                                                    .TP += iTruePositive

                                                                    ' false positive
                                                                    .FP += iPositiveCount - iTruePositive

                                                                    ' invert matrices
                                                                    CvInvoke.BitwiseNot(oGroundTruthMatrix, oGroundTruthMatrix)
                                                                    CvInvoke.BitwiseNot(oSegmentMatrix, oSegmentMatrix)

                                                                    ' true negative
                                                                    CvInvoke.BitwiseAnd(oGroundTruthMatrix, oSegmentMatrix, oCountMatrix)
                                                                    Dim iTrueNegative = CvInvoke.CountNonZero(oCountMatrix)
                                                                    .TN += iTrueNegative

                                                                    ' false negative
                                                                    .FN += iNegativeCount - iTrueNegative
                                                                End With

                                                                oSplitResults.Results.Add(oSplitResult)
                                                            End Using
                                                        End Using
                                                    End Using
                                                End Using
                                            End Using
                                        End Using
                                    Next
                                End Using

                                m_CaptureClipData.Accuracy(oSegType) = oSplitResults
                            End If

                            m_CaptureClipData.ProcessingFrames -= 1
                        Next
                    End Using
                End Using
            End If
        End If
    End Sub
    Private Sub RunAll()
        ' runs all processing on all items
        If ClipDataStore.Count > 0 Then
            For Each oClipData In ClipDataStore
                m_CaptureClipData = oClipData

                ' process video
                Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name)
                Dim sVideoName As String = sFolderName + "\" + VideoFolder

                Dim bProcessed As Boolean = True

                If ProcessColour(sFolderName, sVideoName, FPS) Then
                    oClipData.ProcessedVideo = True
                Else
                    bProcessed = False
                End If

                ' process ground truth
                If bProcessed Then
                    m_ByteFileWriter.Start()

                    bProcessed = ConvertVideo(sFolderName, sVideoName)

                    ' wait until the writer is idle
                    m_ByteFileWriter.Stop()

                    Do While m_ByteFileWriter.Active <> ByteFileWriter.ActiveEnum.Idle
                        Threading.Thread.Sleep(100)
                    Loop

                    If bProcessed Then
                        If ProcessDepth(sFolderName, sVideoName, FPS) Then
                            oClipData.ProcessedGroundTruth = True

                            ' delete depth single files
                            Dim oDirectoryInfo As New IO.DirectoryInfo(sFolderName)
                            Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????][??????????][?.?E????][????????????????][?][DepthSingle].bin").ToList
                            For Each oFileInfo In oFileInfoList
                                oFileInfo.Delete()
                            Next
                        Else
                            bProcessed = False
                        End If
                    End If
                End If
            Next

            CreateNoise()

            For Each oClipData In ClipDataStore
                m_CaptureClipData = oClipData

                ' process video
                Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name)
                Dim sVideoName As String = sFolderName + "\" + VideoFolder

                Dim bProcessed As Boolean = True

                If CutClip(sFolderName) Then
                    m_CaptureClipData.ProcessedCut = True
                Else
                    bProcessed = False
                End If

                If bProcessed Then
                    If GetSegmented(sVideoName) Then
                        m_CaptureClipData.ProcessedSegmented = True
                    Else
                        bProcessed = False
                    End If
                End If

                If bProcessed Then
                    '#If Not DEBUG Then
                    CalculateAccuracy(sVideoName)
                    '#End If
                    SaveClip()
                End If
            Next
        End If
    End Sub
    Private Sub ExportResults()
        ' export to an excel file
        Dim oNoDepthList As List(Of String) = {"GHO", "SLP", "DYN"}.ToList
        Dim sExcelFileName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + ExcelResultsName
        Dim oExcelDocument As New ClosedXML.Excel.XLWorkbook
        Dim oAlgorithmList As List(Of ClipData.SegTypeEnum) = ([Enum].GetValues(GetType(ClipData.SegTypeEnum))).Cast(Of ClipData.SegTypeEnum).ToList
        Dim oType As Type = GetType(ClipData.SegTypeEnum)
        Dim iCurrentLine As Integer = 0
        Dim iCurrentColumn As Integer = 0
        Dim oFuncResult As Func(Of ClipData.SplitResults, Boolean) = Function(ByVal oResult As ClipData.SplitResults) As Boolean
                                                                         Return (oResult.TP >= 0 AndAlso oResult.TN >= 0 AndAlso oResult.FP >= 0 AndAlso oResult.FN >= 0 AndAlso oResult.ProcessTime.TotalMilliseconds > 0)
                                                                     End Function
        Dim oFuncAccuracy As Func(Of Tuple(Of ClipData.SplitResults, String), String) = Function(ByVal oResultTuple As Tuple(Of ClipData.SplitResults, String)) As String
                                                                                            If oNoDepthList.Contains(oResultTuple.Item2) Then
                                                                                                Return String.Empty
                                                                                            Else
                                                                                                Dim oResults As ClipData.SplitResults = oResultTuple.Item1

                                                                                                Dim oAccuracyList As New List(Of Double)
                                                                                                For Each oResult In oResults.Results
                                                                                                    Dim fAccuracy As Double = CDbl(oResult.TP + oResult.TN) / CDbl(oResult.TP + oResult.TN + oResult.FP + oResult.FN)
                                                                                                    oAccuracyList.Add(fAccuracy)
                                                                                                Next

                                                                                                oAccuracyList = oAccuracyList.OrderBy(Function(x) x).ToList
                                                                                                Dim fC1 As Double = oAccuracyList((oResults.Results.Count - 1) * LowerResult)
                                                                                                Dim fC2 As Double = oAccuracyList((oResults.Results.Count - 1) * MidResult)
                                                                                                Dim fC3 As Double = oAccuracyList((oResults.Results.Count - 1) * UpperResult)

                                                                                                Return fC1.ToString("N3") + vbCrLf + fC2.ToString("N3") + vbCrLf + fC3.ToString("N3")
                                                                                            End If
                                                                                        End Function
        Dim oFuncF1Score As Func(Of Tuple(Of ClipData.SplitResults, String), String) = Function(ByVal oResultTuple As Tuple(Of ClipData.SplitResults, String)) As String
                                                                                           If oNoDepthList.Contains(oResultTuple.Item2) Then
                                                                                               Return String.Empty
                                                                                           Else
                                                                                               Dim oResults As ClipData.SplitResults = oResultTuple.Item1

                                                                                               Dim oF1List As New List(Of Double)
                                                                                               For Each oResult In oResults.Results
                                                                                                   Dim fPrecision As Double = CDbl(oResult.TP) / CDbl(oResult.TP + oResult.FP)
                                                                                                   Dim fRecall As Double = CDbl(oResult.TP) / CDbl(oResult.TP + oResult.FN)
                                                                                                   Dim fF1 As Double = 2.0 * (fPrecision * fRecall) / (fPrecision + fRecall)
                                                                                                   If Double.IsNaN(fF1) Then
                                                                                                       oF1List.Add(0.0)
                                                                                                   Else
                                                                                                       oF1List.Add(fF1)
                                                                                                   End If
                                                                                               Next

                                                                                               oF1List = oF1List.OrderBy(Function(x) x).ToList
                                                                                               Dim fC1 As Double = oF1List((oResults.Results.Count - 1) * LowerResult)
                                                                                               Dim fC2 As Double = oF1List((oResults.Results.Count - 1) * MidResult)
                                                                                               Dim fC3 As Double = oF1List((oResults.Results.Count - 1) * UpperResult)

                                                                                               Return fC1.ToString("N3") + vbCrLf + fC2.ToString("N3") + vbCrLf + fC3.ToString("N3")
                                                                                           End If
                                                                                       End Function
        Dim oFuncFPR As Func(Of Tuple(Of ClipData.SplitResults, String), String) = Function(ByVal oResultTuple As Tuple(Of ClipData.SplitResults, String)) As String
                                                                                       If Not oNoDepthList.Contains(oResultTuple.Item2) Then
                                                                                           Return String.Empty
                                                                                       Else
                                                                                           Dim oResults As ClipData.SplitResults = oResultTuple.Item1

                                                                                           Dim oFPRList As New List(Of Double)
                                                                                           For Each oResult In oResults.Results
                                                                                               Dim fFPR As Double = CDbl(oResult.FP) / CDbl(oResult.TN + oResult.FP)
                                                                                               oFPRList.Add(fFPR)
                                                                                           Next

                                                                                           oFPRList = oFPRList.OrderBy(Function(x) x).ToList
                                                                                           Dim fC1 As Double = oFPRList((oResults.Results.Count - 1) * LowerResult)
                                                                                           Dim fC2 As Double = oFPRList((oResults.Results.Count - 1) * MidResult)
                                                                                           Dim fC3 As Double = oFPRList((oResults.Results.Count - 1) * UpperResult)

                                                                                           Return fC1.ToString("N3") + vbCrLf + fC2.ToString("N3") + vbCrLf + fC3.ToString("N3")
                                                                                       End If
                                                                                   End Function
        Dim oFuncTimeText As Func(Of ClipData.SplitResults, String) = Function(ByVal oResult As ClipData.SplitResults) As String
                                                                          Return (CDbl(oResult.ProcessTime.TotalMilliseconds) / CDbl(ProcessClip * FPS)).ToString("N1")
                                                                      End Function

        ' accuracy
        Dim oAccuracyWorksheet As ClosedXML.Excel.IXLWorksheet = oExcelDocument.AddWorksheet("Efficacy")
        oAccuracyWorksheet.Cell(1, 1).Value = "No"
        oAccuracyWorksheet.Cell(1, 1).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 2).Value = "Algorithm"
        oAccuracyWorksheet.Cell(1, 2).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 3).Value = "Constraint"
        oAccuracyWorksheet.Cell(1, 3).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 4).Value = "True Positive"
        oAccuracyWorksheet.Cell(1, 4).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 5).Value = "True Negative"
        oAccuracyWorksheet.Cell(1, 5).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 6).Value = "False Positive"
        oAccuracyWorksheet.Cell(1, 6).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 7).Value = "False Negative"
        oAccuracyWorksheet.Cell(1, 7).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 8).Value = "Total Processing Time (ms)"
        oAccuracyWorksheet.Cell(1, 8).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 9).Value = "Frames Processed"
        oAccuracyWorksheet.Cell(1, 9).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 10).Value = "Processing Time Per Frame (ms)"
        oAccuracyWorksheet.Cell(1, 10).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 11).Value = "Accuracy"
        oAccuracyWorksheet.Cell(1, 11).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 12).Value = "F1 Score"
        oAccuracyWorksheet.Cell(1, 12).Style.Font.Bold = True
        oAccuracyWorksheet.Cell(1, 13).Value = "FPR"
        oAccuracyWorksheet.Cell(1, 13).Style.Font.Bold = True

        Dim iCurrentNumber As Integer = 0
        For i = 0 To oAlgorithmList.Count - 1
            For j = 0 To ClipDataStore.Count - 1
                iCurrentLine = 2 + iCurrentNumber
                Dim oEnum As ClipData.SegTypeEnum = oAlgorithmList(i)

                oAccuracyWorksheet.Cell(iCurrentLine, 1).DataType = ClosedXML.Excel.XLDataType.Text
                oAccuracyWorksheet.Cell(iCurrentLine, 1).Value = (i + 1).ToString

                oAccuracyWorksheet.Cell(iCurrentLine, 2).DataType = ClosedXML.Excel.XLDataType.Text
                oAccuracyWorksheet.Cell(iCurrentLine, 2).Value = [Enum].GetName(oType, oEnum)

                oAccuracyWorksheet.Cell(iCurrentLine, 3).DataType = ClosedXML.Excel.XLDataType.Text
                oAccuracyWorksheet.Cell(iCurrentLine, 3).Value = ClipDataStore(j).Name

                Dim oResult As ClipData.SplitResults = ClipDataStore(j).Accuracy(oAlgorithmList(i))

                ' remove invalid results (all -1) or blank results (all negative pixels)
                If oFuncResult(oResult) Then
                    oAccuracyWorksheet.Cell(iCurrentLine, 4).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 4).Value = oResult.TP

                    oAccuracyWorksheet.Cell(iCurrentLine, 5).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 5).Value = oResult.TN

                    oAccuracyWorksheet.Cell(iCurrentLine, 6).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 6).Value = oResult.FP

                    oAccuracyWorksheet.Cell(iCurrentLine, 7).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 7).Value = oResult.FN

                    oAccuracyWorksheet.Cell(iCurrentLine, 8).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 8).Value = CLng(oResult.ProcessTime.TotalMilliseconds).ToString

                    oAccuracyWorksheet.Cell(iCurrentLine, 9).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 9).Value = oResult.ProcessFrames.ToString

                    oAccuracyWorksheet.Cell(iCurrentLine, 10).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 10).SetValue(Of String)(oFuncTimeText(oResult))

                    oAccuracyWorksheet.Cell(iCurrentLine, 11).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 11).SetValue(Of String)(oFuncAccuracy(New Tuple(Of ClipData.SplitResults, String)(oResult, ClipDataStore(j).Name)))

                    oAccuracyWorksheet.Cell(iCurrentLine, 12).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 12).SetValue(Of String)(oFuncF1Score(New Tuple(Of ClipData.SplitResults, String)(oResult, ClipDataStore(j).Name)))

                    oAccuracyWorksheet.Cell(iCurrentLine, 13).DataType = ClosedXML.Excel.XLDataType.Text
                    oAccuracyWorksheet.Cell(iCurrentLine, 13).SetValue(Of String)(oFuncFPR(New Tuple(Of ClipData.SplitResults, String)(oResult, ClipDataStore(j).Name)))
                End If

                iCurrentNumber += 1
            Next
        Next

        ' autofit columns
        For i = 1 To 13
            oAccuracyWorksheet.Column(i).AdjustToContents()
        Next

        ' summary 1: accuracy
        Dim oSummary1Worksheet As ClosedXML.Excel.IXLWorksheet = oExcelDocument.AddWorksheet("Accuracy")
        Dim oValidClipDataList As New List(Of Tuple(Of Integer, Integer))
        Dim oValidClipData2List As New List(Of Tuple(Of Integer, Integer))
        Dim oValidAlgorithmList As New List(Of Tuple(Of ClipData.SegTypeEnum, Integer))

        oSummary1Worksheet.Cell(1, 1).DataType = ClosedXML.Excel.XLDataType.Text
        oSummary1Worksheet.Cell(1, 1).Style.Font.Bold = True
        oSummary1Worksheet.Cell(1, 1).Value = "Accuracy"
        iCurrentColumn = 2
        For i = 0 To ClipDataStore.Count - 1
            Dim iValidResult As Integer = Aggregate oResult In ClipDataStore(i).Accuracy.Values Where oFuncResult(oResult) Into Count
            If iValidResult > 0 Then
                oValidClipDataList.Add(New Tuple(Of Integer, Integer)(i, iCurrentColumn))

                oSummary1Worksheet.Cell(1, iCurrentColumn).DataType = ClosedXML.Excel.XLDataType.Text
                oSummary1Worksheet.Cell(1, iCurrentColumn).Style.Font.Bold = True
                oSummary1Worksheet.Cell(1, iCurrentColumn).Value = ClipDataStore(i).Name
                iCurrentColumn += 1
            End If
        Next
        iCurrentLine = 2
        For i = 0 To oAlgorithmList.Count - 1
            Dim oEnum As ClipData.SegTypeEnum = oAlgorithmList(i)
            Dim iValidResult As Integer = Aggregate iIndex In Enumerable.Range(0, oValidClipDataList.Count) From oResult In ClipDataStore(oValidClipDataList(iIndex).Item1).Accuracy Where oResult.Key = oEnum AndAlso oFuncResult(oResult.Value) Into Count

            If iValidResult > 0 Then
                oValidAlgorithmList.Add(New Tuple(Of ClipData.SegTypeEnum, Integer)(oEnum, iCurrentLine))

                oSummary1Worksheet.Cell(iCurrentLine, 1).DataType = ClosedXML.Excel.XLDataType.Text
                oSummary1Worksheet.Cell(iCurrentLine, 1).Style.Font.Bold = True
                oSummary1Worksheet.Cell(iCurrentLine, 1).Value = [Enum].GetName(GetType(ClipData.SegTypeEnum), oEnum)
                iCurrentLine += 1
            End If
        Next

        For i = 0 To oValidAlgorithmList.Count - 1
            For j = 0 To oValidClipDataList.Count - 1
                Dim oEnum As ClipData.SegTypeEnum = oValidAlgorithmList(i).Item1
                Dim oResult As ClipData.SplitResults = ClipDataStore(oValidClipDataList(j).Item1).Accuracy(oEnum)

                oSummary1Worksheet.Cell(oValidAlgorithmList(i).Item2, oValidClipDataList(j).Item2).Style.Font.Bold = True
                oSummary1Worksheet.Cell(oValidAlgorithmList(i).Item2, oValidClipDataList(j).Item2).SetValue(Of String)(oFuncAccuracy(New Tuple(Of ClipData.SplitResults, String)(oResult, ClipDataStore(oValidClipDataList(j).Item1).Name)))
            Next
        Next

        ' autofit columns
        oSummary1Worksheet.Column(1).AdjustToContents()

        ' summary 2: F1 Score
        Dim oSummary2Worksheet As ClosedXML.Excel.IXLWorksheet = oExcelDocument.AddWorksheet("F1 Score")
        oValidClipDataList.Clear()
        oValidClipData2List.Clear()
        oValidAlgorithmList.Clear()

        oSummary2Worksheet.Cell(1, 1).DataType = ClosedXML.Excel.XLDataType.Text
        oSummary2Worksheet.Cell(1, 1).Style.Font.Bold = True
        oSummary2Worksheet.Cell(1, 1).Value = "F1 Score"
        iCurrentColumn = 2
        For i = 0 To ClipDataStore.Count - 1
            Dim iValidResult As Integer = Aggregate oResult In ClipDataStore(i).Accuracy.Values Where oFuncResult(oResult) Into Count
            If iValidResult > 0 Then
                oValidClipDataList.Add(New Tuple(Of Integer, Integer)(i, iCurrentColumn))

                oSummary2Worksheet.Cell(1, iCurrentColumn).DataType = ClosedXML.Excel.XLDataType.Text
                oSummary2Worksheet.Cell(1, iCurrentColumn).Style.Font.Bold = True
                oSummary2Worksheet.Cell(1, iCurrentColumn).Value = ClipDataStore(i).Name
                iCurrentColumn += 1
            End If
        Next
        iCurrentLine = 2
        For i = 0 To oAlgorithmList.Count - 1
            Dim oEnum As ClipData.SegTypeEnum = oAlgorithmList(i)
            Dim iValidResult As Integer = Aggregate iIndex In Enumerable.Range(0, oValidClipDataList.Count) From oResult In ClipDataStore(oValidClipDataList(iIndex).Item1).Accuracy Where oResult.Key = oEnum AndAlso oFuncResult(oResult.Value) Into Count

            If iValidResult > 0 Then
                oValidAlgorithmList.Add(New Tuple(Of ClipData.SegTypeEnum, Integer)(oEnum, iCurrentLine))

                oSummary2Worksheet.Cell(iCurrentLine, 1).DataType = ClosedXML.Excel.XLDataType.Text
                oSummary2Worksheet.Cell(iCurrentLine, 1).Style.Font.Bold = True
                oSummary2Worksheet.Cell(iCurrentLine, 1).Value = [Enum].GetName(GetType(ClipData.SegTypeEnum), oEnum)
                iCurrentLine += 1
            End If
        Next

        For i = 0 To oValidAlgorithmList.Count - 1
            For j = 0 To oValidClipDataList.Count - 1
                Dim oEnum As ClipData.SegTypeEnum = oValidAlgorithmList(i).Item1
                Dim oResult As ClipData.SplitResults = ClipDataStore(oValidClipDataList(j).Item1).Accuracy(oEnum)

                oSummary2Worksheet.Cell(oValidAlgorithmList(i).Item2, oValidClipDataList(j).Item2).Style.Font.Bold = True
                oSummary2Worksheet.Cell(oValidAlgorithmList(i).Item2, oValidClipDataList(j).Item2).SetValue(Of String)(oFuncF1Score(New Tuple(Of ClipData.SplitResults, String)(oResult, ClipDataStore(oValidClipDataList(j).Item1).Name)))
            Next
        Next

        ' autofit columns
        oSummary2Worksheet.Column(1).AdjustToContents()

        ' summary 3: False Positive Rate
        Dim oSummary3Worksheet As ClosedXML.Excel.IXLWorksheet = oExcelDocument.AddWorksheet("FPR")
        oValidClipDataList.Clear()
        oValidClipData2List.Clear()
        oValidAlgorithmList.Clear()

        oSummary3Worksheet.Cell(1, 1).DataType = ClosedXML.Excel.XLDataType.Text
        oSummary3Worksheet.Cell(1, 1).Style.Font.Bold = True
        oSummary3Worksheet.Cell(1, 1).Value = "False Positive Rate"
        iCurrentColumn = 2
        For i = 0 To ClipDataStore.Count - 1
            Dim iValidResult As Integer = Aggregate oResult In ClipDataStore(i).Accuracy.Values Where oFuncResult(oResult) Into Count
            If iValidResult > 0 Then
                oValidClipDataList.Add(New Tuple(Of Integer, Integer)(i, iCurrentColumn))

                oSummary3Worksheet.Cell(1, iCurrentColumn).DataType = ClosedXML.Excel.XLDataType.Text
                oSummary3Worksheet.Cell(1, iCurrentColumn).Style.Font.Bold = True
                oSummary3Worksheet.Cell(1, iCurrentColumn).Value = ClipDataStore(i).Name
                iCurrentColumn += 1
            End If
        Next
        iCurrentLine = 2
        For i = 0 To oAlgorithmList.Count - 1
            Dim oEnum As ClipData.SegTypeEnum = oAlgorithmList(i)
            Dim iValidResult As Integer = Aggregate iIndex In Enumerable.Range(0, oValidClipDataList.Count) From oResult In ClipDataStore(oValidClipDataList(iIndex).Item1).Accuracy Where oResult.Key = oEnum AndAlso oFuncResult(oResult.Value) Into Count

            If iValidResult > 0 Then
                oValidAlgorithmList.Add(New Tuple(Of ClipData.SegTypeEnum, Integer)(oEnum, iCurrentLine))

                oSummary3Worksheet.Cell(iCurrentLine, 1).DataType = ClosedXML.Excel.XLDataType.Text
                oSummary3Worksheet.Cell(iCurrentLine, 1).Style.Font.Bold = True
                oSummary3Worksheet.Cell(iCurrentLine, 1).Value = [Enum].GetName(GetType(ClipData.SegTypeEnum), oEnum)
                iCurrentLine += 1
            End If
        Next

        For i = 0 To oValidAlgorithmList.Count - 1
            For j = 0 To oValidClipDataList.Count - 1
                Dim oEnum As ClipData.SegTypeEnum = oValidAlgorithmList(i).Item1
                Dim oResult As ClipData.SplitResults = ClipDataStore(oValidClipDataList(j).Item1).Accuracy(oEnum)

                oSummary3Worksheet.Cell(oValidAlgorithmList(i).Item2, oValidClipDataList(j).Item2).Style.Font.Bold = True
                oSummary3Worksheet.Cell(oValidAlgorithmList(i).Item2, oValidClipDataList(j).Item2).SetValue(Of String)(oFuncFPR(New Tuple(Of ClipData.SplitResults, String)(oResult, ClipDataStore(oValidClipDataList(j).Item1).Name)))
            Next
        Next

        ' autofit columns
        oSummary3Worksheet.Column(1).AdjustToContents()

        ' summary 4: processing time per frame
        Dim oSummary5Worksheet As ClosedXML.Excel.IXLWorksheet = oExcelDocument.AddWorksheet("Time")

        oSummary5Worksheet.Cell(1, 1).DataType = ClosedXML.Excel.XLDataType.Text
        oSummary5Worksheet.Cell(1, 1).Style.Font.Bold = True
        oSummary5Worksheet.Cell(1, 1).Value = "Processing Time Per Frame (ms)"
        iCurrentColumn = 2
        For i = 0 To ClipDataStore.Count - 1
            Dim iValidResult As Integer = Aggregate oResult In ClipDataStore(i).Accuracy.Values Where oFuncResult(oResult) Into Count
            If iValidResult > 0 Then
                oValidClipData2List.Add(New Tuple(Of Integer, Integer)(i, iCurrentColumn))

                oSummary5Worksheet.Cell(1, iCurrentColumn).DataType = ClosedXML.Excel.XLDataType.Text
                oSummary5Worksheet.Cell(1, iCurrentColumn).Style.Font.Bold = True
                oSummary5Worksheet.Cell(1, iCurrentColumn).Value = ClipDataStore(i).Name
                iCurrentColumn += 1
            End If
        Next
        For i = 0 To oValidAlgorithmList.Count - 1
            oSummary5Worksheet.Cell(oValidAlgorithmList(i).Item2, 1).DataType = ClosedXML.Excel.XLDataType.Text
            oSummary5Worksheet.Cell(oValidAlgorithmList(i).Item2, 1).Style.Font.Bold = True
            oSummary5Worksheet.Cell(oValidAlgorithmList(i).Item2, 1).Value = [Enum].GetName(GetType(ClipData.SegTypeEnum), oValidAlgorithmList(i).Item1)
        Next

        For i = 0 To oValidAlgorithmList.Count - 1
            For j = 0 To oValidClipData2List.Count - 1
                Dim oEnum As ClipData.SegTypeEnum = oValidAlgorithmList(i).Item1
                Dim oResult As ClipData.SplitResults = ClipDataStore(oValidClipData2List(j).Item1).Accuracy(oEnum)

                oSummary5Worksheet.Cell(oValidAlgorithmList(i).Item2, oValidClipData2List(j).Item2).Style.Font.Bold = True
                oSummary5Worksheet.Cell(oValidAlgorithmList(i).Item2, oValidClipData2List(j).Item2).SetValue(Of String)(oFuncTimeText(oResult))
            Next
        Next

        ' autofit columns
        oSummary5Worksheet.Column(1).AdjustToContents()

        ' algorithm
        Dim oAlgorithmWorksheet As ClosedXML.Excel.IXLWorksheet = oExcelDocument.AddWorksheet("Algorithm")

        oAlgorithmWorksheet.Cell(1, 1).Value = "No"
        oAlgorithmWorksheet.Cell(1, 1).Style.Font.Bold = True
        oAlgorithmWorksheet.Cell(1, 2).Value = "BGS Name"
        oAlgorithmWorksheet.Cell(1, 2).Style.Font.Bold = True
        oAlgorithmWorksheet.Cell(1, 3).Value = "Article Name"
        oAlgorithmWorksheet.Cell(1, 3).Style.Font.Bold = True

        For i = 0 To oAlgorithmList.Count - 1
            iCurrentLine = 2 + i
            Dim oEnum As ClipData.SegTypeEnum = oAlgorithmList(i)

            oAlgorithmWorksheet.Cell(iCurrentLine, 1).DataType = ClosedXML.Excel.XLDataType.Text
            oAlgorithmWorksheet.Cell(iCurrentLine, 1).Value = (i + 1).ToString

            oAlgorithmWorksheet.Cell(iCurrentLine, 2).DataType = ClosedXML.Excel.XLDataType.Text
            oAlgorithmWorksheet.Cell(iCurrentLine, 2).Value = [Enum].GetName(oType, oEnum)

            Dim oAttribute As DescriptionAttribute = CType(oType.GetField([Enum].GetName(oType, oEnum)).GetCustomAttribute(GetType(DescriptionAttribute)), DescriptionAttribute)
            oAlgorithmWorksheet.Cell(iCurrentLine, 3).DataType = ClosedXML.Excel.XLDataType.Text
            oAlgorithmWorksheet.Cell(iCurrentLine, 3).Value = If(IsNothing(oAttribute), String.Empty, oAttribute.Description)
        Next

        ' autofit columns
        For i = 1 To 3
            oAlgorithmWorksheet.Column(i).AdjustToContents()
        Next

        ' constraints
        Dim oConstraintsWorksheet As ClosedXML.Excel.IXLWorksheet = oExcelDocument.AddWorksheet("Constraints")

        oConstraintsWorksheet.Cell(1, 1).Value = "No"
        oConstraintsWorksheet.Cell(1, 1).Style.Font.Bold = True
        oConstraintsWorksheet.Cell(1, 2).Value = "Name"
        oConstraintsWorksheet.Cell(1, 2).Style.Font.Bold = True
        oConstraintsWorksheet.Cell(1, 3).Value = "Description"
        oConstraintsWorksheet.Cell(1, 3).Style.Font.Bold = True
        For i = 0 To ClipDataStore.Count - 1
            iCurrentLine = 2 + i
            oConstraintsWorksheet.Cell(iCurrentLine, 1).DataType = ClosedXML.Excel.XLDataType.Text
            oConstraintsWorksheet.Cell(iCurrentLine, 1).Value = (i + 1).ToString

            oConstraintsWorksheet.Cell(iCurrentLine, 2).DataType = ClosedXML.Excel.XLDataType.Text
            oConstraintsWorksheet.Cell(iCurrentLine, 2).Value = ClipDataStore(i).Name

            oConstraintsWorksheet.Cell(iCurrentLine, 3).DataType = ClosedXML.Excel.XLDataType.Text
            oConstraintsWorksheet.Cell(iCurrentLine, 3).Value = ClipDataStore(i).Description
        Next

        ' autofit columns
        For i = 1 To 3
            oConstraintsWorksheet.Column(i).AdjustToContents()
        Next

        oExcelDocument.SaveAs(sExcelFileName)
    End Sub
    Private Sub SaveClip(Optional ByVal iTask As Integer = -1)
        ' saves the clip file
        Dim oClipDataList As List(Of ClipData) = ClipDataStore.ToList
        Dim sSaveFileName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + ClipFileName
        Dim oFileInfo As New IO.FileInfo(sSaveFileName)
        If iTask = -1 Then
            ' create backup
            If oFileInfo.Exists Then
                Dim sBakFileName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + ClipBakName
                If IO.File.Exists(sBakFileName) Then
                    IO.File.Delete(sBakFileName)
                End If
                IO.File.Move(sSaveFileName, sBakFileName)
            End If
        Else
            sSaveFileName = Left(oFileInfo.FullName, Len(oFileInfo.FullName) - Len(oFileInfo.Extension)) + "_" + iTask.ToString + oFileInfo.Extension
        End If

        CommonFunctions.SerializeDataContractFile(sSaveFileName, oClipDataList)
    End Sub
    Private Function GrabCut(ByVal oMatrixColour As Matrix(Of Byte), ByVal oMatrixMask As Matrix(Of Byte), ByVal oSelections As List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum))) As Matrix(Of Byte)
        ' returns a grabcut modification of the mask
        ' run through selections in descending order of size
        Dim oOrderedSelections As List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)) = oSelections.OrderByDescending(Function(x) x.Item3).ToList
        Dim oMask As New Matrix(Of Byte)(oMatrixMask.Size)
        CvInvoke.Normalize(oMatrixMask, oMask, 0, 1, CvEnum.NormType.MinMax)

        Using oSelectionMask As New Matrix(Of Byte)(oMatrixMask.Size)
            oSelectionMask.SetZero()

            ' draw all selections on selection mask
            Using oContours As New Util.VectorOfVectorOfPoint()
                For i = 0 To oOrderedSelections.Count - 1
                    If oOrderedSelections(i).Item4 = SelectionTypeEnum.None Then
                        oContours.Push(New Util.VectorOfPoint((From oPoint In oOrderedSelections(i).Item1 Select New System.Drawing.Point(oPoint.X * (oMatrixMask.Width - 1), oPoint.Y * (oMatrixMask.Height - 1))).ToArray))
                    End If
                Next
                CvInvoke.DrawContours(oSelectionMask, oContours, -1, New [Structure].MCvScalar(2), -1)
            End Using

            ' selection background (0) -> possible background (2), foreground (1) -> possible foreground (3)
            CvInvoke.Add(oMask, oSelectionMask, oMask)

            Using oContours As New Util.VectorOfVectorOfPoint()
                For i = 0 To oOrderedSelections.Count - 1
                    Dim oCentroid As New System.Drawing.Point(oOrderedSelections(i).Item2.X * (oMatrixMask.Width - 1), oOrderedSelections(i).Item2.Y * (oMatrixMask.Height - 1))

                    ' only process if centroid is within the selection
                    If oSelectionMask(oCentroid.Y, oCentroid.X) > 0 AndAlso oOrderedSelections(i).Item4 = SelectionTypeEnum.Background Then
                        oContours.Push(New Util.VectorOfPoint((From oPoint In oOrderedSelections(i).Item1 Select New System.Drawing.Point(oPoint.X * (oMatrixMask.Width - 1), oPoint.Y * (oMatrixMask.Height - 1))).ToArray))
                    End If
                Next
                If oContours.Size > 0 Then
                    CvInvoke.DrawContours(oMask, oContours, -1, New [Structure].MCvScalar(0), -1)
                End If
            End Using
            Using oContours As New Util.VectorOfVectorOfPoint()
                For i = 0 To oOrderedSelections.Count - 1
                    Dim oCentroid As New System.Drawing.Point(oOrderedSelections(i).Item2.X * (oMatrixMask.Width - 1), oOrderedSelections(i).Item2.Y * (oMatrixMask.Height - 1))

                    ' only process if centroid is within the selection
                    If oSelectionMask(oCentroid.Y, oCentroid.X) > 0 AndAlso oOrderedSelections(i).Item4 = SelectionTypeEnum.Foreground Then
                        oContours.Push(New Util.VectorOfPoint((From oPoint In oOrderedSelections(i).Item1 Select New System.Drawing.Point(oPoint.X * (oMatrixMask.Width - 1), oPoint.Y * (oMatrixMask.Height - 1))).ToArray))
                    End If
                Next
                If oContours.Size > 0 Then
                    CvInvoke.DrawContours(oMask, oContours, -1, New [Structure].MCvScalar(1), -1)
                End If
            End Using

            Using oVector As New Util.VectorOfMat
                Using oCLAHEMatrix As New Matrix(Of Byte)(oMatrixColour.Height, oMatrixColour.Width, 3)
                    ' run CLAHE on luminance channel before watershed
                    CvInvoke.CvtColor(oMatrixColour, oCLAHEMatrix, CvEnum.ColorConversion.Bgr2Lab)
                    CvInvoke.Split(oCLAHEMatrix, oVector)
                    CvInvoke.CLAHE(oVector(0), 2, New System.Drawing.Size(8, 8), oVector(0))
                    CvInvoke.Merge(oVector, oCLAHEMatrix)
                    CvInvoke.CvtColor(oCLAHEMatrix, oCLAHEMatrix, CvEnum.ColorConversion.Lab2Bgr)

                    Using oBackgroundModel As New Mat
                        Using oForegroundModel As New Mat
                            CvInvoke.GrabCut(oMatrixColour, oMask, System.Drawing.Rectangle.Empty, oBackgroundModel, oForegroundModel, 1, CvEnum.GrabcutInitType.InitWithMask)
                        End Using
                    End Using
                End Using
            End Using

            CvInvoke.Subtract(oMask, oSelectionMask, oMask)

            ' reset marker points to account for subtraction
            Using oContours As New Util.VectorOfVectorOfPoint()
                For i = 0 To oOrderedSelections.Count - 1
                    Dim oCentroid As New System.Drawing.Point(oOrderedSelections(i).Item2.X * (oMatrixMask.Width - 1), oOrderedSelections(i).Item2.Y * (oMatrixMask.Height - 1))

                    ' only process if centroid is within the selection
                    If oSelectionMask(oCentroid.Y, oCentroid.X) > 0 AndAlso oOrderedSelections(i).Item4 = SelectionTypeEnum.Background Then
                        oContours.Push(New Util.VectorOfPoint((From oPoint In oOrderedSelections(i).Item1 Select New System.Drawing.Point(oPoint.X * (oMatrixMask.Width - 1), oPoint.Y * (oMatrixMask.Height - 1))).ToArray))
                    End If
                Next
                If oContours.Size > 0 Then
                    CvInvoke.DrawContours(oMask, oContours, -1, New [Structure].MCvScalar(0), -1)
                End If
            End Using
            Using oContours As New Util.VectorOfVectorOfPoint()
                For i = 0 To oOrderedSelections.Count - 1
                    Dim oCentroid As New System.Drawing.Point(oOrderedSelections(i).Item2.X * (oMatrixMask.Width - 1), oOrderedSelections(i).Item2.Y * (oMatrixMask.Height - 1))

                    ' only process if centroid is within the selection
                    If oSelectionMask(oCentroid.Y, oCentroid.X) > 0 AndAlso oOrderedSelections(i).Item4 = SelectionTypeEnum.Foreground Then
                        oContours.Push(New Util.VectorOfPoint((From oPoint In oOrderedSelections(i).Item1 Select New System.Drawing.Point(oPoint.X * (oMatrixMask.Width - 1), oPoint.Y * (oMatrixMask.Height - 1))).ToArray))
                    End If
                Next
                If oContours.Size > 0 Then
                    CvInvoke.DrawContours(oMask, oContours, -1, New [Structure].MCvScalar(1), -1)
                End If
            End Using

            ' fill small gaps
            Using oContours As New Util.VectorOfVectorOfPoint()
                CvInvoke.FindContours(oMask, oContours, Nothing, CvEnum.RetrType.External, CvEnum.ChainApproxMethod.ChainApproxSimple)
                oMask.SetZero()
                CvInvoke.DrawContours(oMask, oContours, -1, New [Structure].MCvScalar(255), -1)
                CvInvoke.MorphologyEx(oMask, oMask, CvEnum.MorphOp.Close, CvInvoke.GetStructuringElement(CvEnum.ElementShape.Rectangle, New System.Drawing.Size(3, 3), New System.Drawing.Point(-1, -1)), New System.Drawing.Point(-1, -1), 1, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(Byte.MinValue))
            End Using

            For i = 0 To oOrderedSelections.Count - 1
                Dim oCentroid As New System.Drawing.Point(oOrderedSelections(i).Item2.X * (oMatrixMask.Width - 1), oOrderedSelections(i).Item2.Y * (oMatrixMask.Height - 1))

                ' only process if centroid is outside the selection
                ' if the point is not equal to the marker type, then run a flood fill
                If oSelectionMask(oCentroid.Y, oCentroid.X) = 0 AndAlso oOrderedSelections(i).Item4 = SelectionTypeEnum.Background AndAlso oMask(oCentroid.Y, oCentroid.X) > 0 Then
                    Using oFillExpanded As New Matrix(Of Byte)(oMask.Height + 2, oMask.Width + 2)
                        Dim oBounds As New System.Drawing.Rectangle
                        CvInvoke.FloodFill(oMask, oFillExpanded, oCentroid, New [Structure].MCvScalar(255), oBounds, New [Structure].MCvScalar(0), New [Structure].MCvScalar(0), CvEnum.Connectivity.FourConnected, CvEnum.FloodFillType.MaskOnly)
                        oMask.SetValue(0, oFillExpanded.GetSubRect(New System.Drawing.Rectangle(1, 1, oMask.Width, oMask.Height)))
                    End Using
                End If
            Next
            For i = 0 To oOrderedSelections.Count - 1
                Dim oCentroid As New System.Drawing.Point(oOrderedSelections(i).Item2.X * (oMatrixMask.Width - 1), oOrderedSelections(i).Item2.Y * (oMatrixMask.Height - 1))

                ' only process if centroid is outside the selection
                ' if the point is not equal to the marker type, then run a flood fill
                If oSelectionMask(oCentroid.Y, oCentroid.X) = 0 AndAlso oOrderedSelections(i).Item4 = SelectionTypeEnum.Foreground AndAlso oMask(oCentroid.Y, oCentroid.X) = 0 Then
                    Using oFillExpanded As New Matrix(Of Byte)(oMask.Height + 2, oMask.Width + 2)
                        Dim oBounds As New System.Drawing.Rectangle
                        CvInvoke.FloodFill(oMask, oFillExpanded, oCentroid, New [Structure].MCvScalar(255), oBounds, New [Structure].MCvScalar(0), New [Structure].MCvScalar(0), CvEnum.Connectivity.FourConnected, CvEnum.FloodFillType.MaskOnly)
                        oMask.SetValue(255, oFillExpanded.GetSubRect(New System.Drawing.Rectangle(1, 1, oMask.Width, oMask.Height)))
                    End Using
                End If
            Next
        End Using

        Return oMask
    End Function
    Private Function GetInterpolations(ByVal oPlayDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single)))) As Tuple(Of Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)), Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)))
        ' gets the colour and depth interpolations
        Dim iStartColour As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Min(iKey)
        Dim iStartDepth As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Depth Into Min(iKey)
        Dim iEndColour As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Max(iKey)
        Dim iEndDepth As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Depth Into Max(iKey)
        Dim iStart As Integer = Math.Max(iStartColour, iStartDepth)
        Dim iEnd As Integer = Math.Min(iEndColour, iEndDepth)

        Dim oColourList As List(Of Integer) = (From iKey In oPlayDictionary.Keys Where iKey >= iStart AndAlso iKey <= iEnd From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select iKey).ToList
        Dim oDepthList As List(Of Integer) = (From iKey In oPlayDictionary.Keys Where iKey >= iStart AndAlso iKey <= iEnd From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Depth Select iKey).ToList
        If oColourList.First > iStart Then
            ' add the last value to the list
            Dim oLowColourList As List(Of Integer) = (From iKey In oPlayDictionary.Keys Where iKey < iStart From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select iKey).ToList
            oColourList.Insert(0, oLowColourList.Max)
        End If
        If oDepthList.First > iStart Then
            ' add the last value to the list
            Dim oLowDepthList As List(Of Integer) = (From iKey In oPlayDictionary.Keys Where iKey < iStart From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Depth Select iKey).ToList
            oDepthList.Insert(0, oLowDepthList.Max)
        End If

        Dim sColourFileName As String = (From oStream In oPlayDictionary(oColourList.First) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select oStream.Item2).First
        Dim oColourFileInfo As New IO.FileInfo(sColourFileName)
        Dim sColourName As String = Left(oColourFileInfo.Name, Len(oColourFileInfo.Name) - Len(oColourFileInfo.Extension))
        Dim sColourScale As String = Mid(sColourName, 22, 8)

        Dim sDepthFileName As String = (From oStream In oPlayDictionary(oDepthList.First) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Depth Select oStream.Item2).First
        Dim oDepthFileInfo As New IO.FileInfo(sDepthFileName)
        Dim sDepthName As String = Left(oDepthFileInfo.Name, Len(oDepthFileInfo.Name) - Len(oDepthFileInfo.Extension))
        Dim sDepthScale As String = Mid(sDepthName, 22, 8)

        Dim fInterval As Double = 1.0 / CDbl(FPS)
        Dim oTargetList As New List(Of Integer)
        Dim fCurrent As Double = CDbl(iStart) / 1000.0
        Dim fEnd As Double = CDbl(Math.Min(oColourList.Max, oDepthList.Max)) / 1000.0
        Do
            oTargetList.Add(CInt(fCurrent * 1000.0))
            fCurrent += fInterval
        Loop Until fCurrent > fEnd

        Dim oColourInterpolations As Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)) = GetInterpolations(oColourList, oTargetList)
        Dim oDepthInterpolations As Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)) = GetInterpolations(oDepthList, oTargetList)
        Return New Tuple(Of Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)), Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)))(oColourInterpolations, oDepthInterpolations)
    End Function
    Private Function GetFlowFrame(Of T As Structure)(ByVal oFrame1 As Matrix(Of T), ByVal oFrame2 As Matrix(Of T), ByRef oFlowMatrix As Matrix(Of Single), ByVal oMapMatrix As Matrix(Of Single), ByVal bNewFlow As Boolean, ByVal fFraction As Single) As Matrix(Of T)
        ' gets optical flow interpolation between two frames
        ' if the fraction is zero, then return the first frame
        If fFraction = 0 Then
            Return oFrame1.Clone
        Else
            Dim OpticalFlowHeight As Integer = oFlowMatrix.Height
            Dim OpticalFlowWidth As Integer = oFlowMatrix.Width
            Dim fMin1 As Double = 0
            Dim fMax1 As Double = 0
            Dim fMin2 As Double = 0
            Dim fMax2 As Double = 0

            Using oFrameByte1 As New Matrix(Of Byte)(OpticalFlowHeight, OpticalFlowWidth)
                Using oFrameByte2 As New Matrix(Of Byte)(OpticalFlowHeight, OpticalFlowWidth)
                    Select Case GetType(T)
                        Case GetType(Byte)
                            Using oFrameMono1 As New Matrix(Of Byte)(oFrame1.Size)
                                Using oFrameMono2 As New Matrix(Of Byte)(oFrame2.Size)
                                    CvInvoke.CvtColor(oFrame1, oFrameMono1, CvEnum.ColorConversion.Bgr2Gray)
                                    CvInvoke.CvtColor(oFrame2, oFrameMono2, CvEnum.ColorConversion.Bgr2Gray)
                                    CvInvoke.MinMaxLoc(oFrameMono1, fMin1, fMax1, Nothing, Nothing)
                                    CvInvoke.MinMaxLoc(oFrameMono2, fMin2, fMax2, Nothing, Nothing)
                                    Dim fMin As Double = Math.Min(fMin1, fMin2)
                                    Dim fMax As Double = Math.Max(fMax1, fMax2)
                                    If fMax <> fMin Then
                                        Dim fMul As Double = 255.0 / (fMax - fMin)
                                        CvInvoke.Resize(oFrameMono1.Convert(Of Single).Sub(fMin).Mul(fMul).Convert(Of Byte), oFrameByte1, oFrameByte1.Size)
                                        CvInvoke.Resize(oFrameMono2.Convert(Of Single).Sub(fMin).Mul(fMul).Convert(Of Byte), oFrameByte2, oFrameByte2.Size)
                                    Else
                                        CvInvoke.Resize(oFrameMono1.Sub(fMin), oFrameByte1, oFrameByte1.Size)
                                        CvInvoke.Resize(oFrameMono2.Sub(fMin), oFrameByte2, oFrameByte2.Size)
                                    End If
                                End Using
                            End Using
                        Case GetType(Single)
                            CvInvoke.MinMaxLoc(oFrame1, fMin1, fMax1, Nothing, Nothing)
                            CvInvoke.MinMaxLoc(oFrame2, fMin2, fMax2, Nothing, Nothing)
                            Dim fMin As Double = Math.Min(fMin1, fMin2)
                            Dim fMax As Double = Math.Max(fMax1, fMax2)
                            If fMax <> fMin Then
                                Dim fMul As Double = 255.0 / (fMax - fMin)
                                Using oFrameSingle As Matrix(Of Single) = oFrame1.Convert(Of Single)
                                    CvInvoke.Resize(oFrameSingle.Sub(fMin).Mul(fMul).Convert(Of Byte), oFrameByte1, oFrameByte1.Size)
                                End Using
                                Using oFrameSingle As Matrix(Of Single) = oFrame2.Convert(Of Single)
                                    CvInvoke.Resize(oFrameSingle.Sub(fMin).Mul(fMul).Convert(Of Byte), oFrameByte2, oFrameByte2.Size)
                                End Using
                            Else
                                Using oFrameSingle As Matrix(Of Single) = oFrame1.Convert(Of Single)
                                    CvInvoke.Resize(oFrameSingle.Sub(fMin).Convert(Of Byte), oFrameByte1, oFrameByte1.Size)
                                End Using
                                Using oFrameSingle As Matrix(Of Single) = oFrame2.Convert(Of Single)
                                    CvInvoke.Resize(oFrameSingle.Sub(fMin).Convert(Of Byte), oFrameByte2, oFrameByte2.Size)
                                End Using
                            End If
                    End Select

                    CvInvoke.CalcOpticalFlowFarneback(oFrameByte1, oFrameByte2, oFlowMatrix, 0.5, 3, 25, 3, 5, 1.1, If(bNewFlow, CvEnum.OpticalflowFarnebackFlag.Default, CvEnum.OpticalflowFarnebackFlag.UseInitialFlow) Or CvEnum.OpticalflowFarnebackFlag.FarnebackGaussian)
                    Using oFractionFlow As Matrix(Of Single) = oFlowMatrix.Mul(fFraction)
                        Using oResizedFractionFlow As New Matrix(Of Single)(oFrame1.Height, oFrame1.Width, oFractionFlow.NumberOfChannels)
                            CvInvoke.Resize(oFractionFlow, oResizedFractionFlow, oResizedFractionFlow.Size, 0, 0, CvEnum.Inter.Linear)
                            CvInvoke.Add(oResizedFractionFlow, oMapMatrix, oResizedFractionFlow)

                            Using oVector As New Util.VectorOfMat
                                CvInvoke.Split(oResizedFractionFlow, oVector)
                                Dim oInterMatrix As New Matrix(Of T)(oFrame1.Height, oFrame1.Width, oFrame1.NumberOfChannels)
                                CvInvoke.Remap(oFrame1, oInterMatrix, oVector(0), oVector(1), CvEnum.Inter.Lanczos4, CvEnum.BorderType.Transparent)

                                Return oInterMatrix
                            End Using
                        End Using
                    End Using
                End Using
            End Using
        End If
    End Function
    Private Function GetAltFlowFrame(Of T As Structure)(ByVal oFrame1 As Matrix(Of T), ByVal oFrame2 As Matrix(Of T), ByVal fFraction As Single) As Matrix(Of T)
        ' gets weighted average between two frames
        ' if the fraction is zero, then return the first frame
        If fFraction = 0 Then
            Return oFrame1.Clone
        Else
            Dim oInterMatrix As New Matrix(Of T)(oFrame1.Height, oFrame1.Width, oFrame1.NumberOfChannels)
            oInterMatrix.SetValue(New [Structure].MCvScalar(0, 0, 0))
            CvInvoke.AddWeighted(oFrame1, 1.0 - fFraction, oFrame2, fFraction, 0.0, oInterMatrix)

            Return oInterMatrix
        End If
    End Function
    Private Function GetInterpolations(ByVal oTimingList As List(Of Integer), ByVal oTargetList As List(Of Integer)) As Dictionary(Of Integer, Tuple(Of Integer, Integer, Single))
        ' returns a dictionary where the key is the target timings, and the value is a tuple of the first timing, second timing, and the fraction between these
        Dim oInterpolations As New Dictionary(Of Integer, Tuple(Of Integer, Integer, Single))
        For Each iTarget In oTargetList
            Dim iFirstTiming As Integer = Aggregate iTiming In oTimingList Where iTiming <= iTarget Into Max(iTiming)
            Dim iSecondTiming As Integer = Aggregate iTiming In oTimingList Where iTiming >= iTarget Into Min(iTiming)
            Dim fFraction As Single = 0
            If iSecondTiming > iFirstTiming Then
                fFraction = CDbl(iTarget - iFirstTiming) / CDbl(iSecondTiming - iFirstTiming)
            End If
            oInterpolations.Add(iTarget, New Tuple(Of Integer, Integer, Single)(iFirstTiming, iSecondTiming, fFraction))
        Next
        Return oInterpolations
    End Function
    Private Function GetPlayDictionary(ByVal sFolderName As String) As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single)))
        ' gets the play dictionary from a folder for further processing
        Dim oDirectoryInfo As New IO.DirectoryInfo(sFolderName)
        Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????][??????????][?.?E????][????????????????][?][*].bin").ToList

        ' sort files into a dictionary
        Dim oPlayDictionary As New Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single)))
        For Each oFileInfo In oFileInfoList
            Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
            Dim sKey As String = Mid(sName, 2, 6)
            Dim sValue As String = Mid(sName, 10, 10)
            Dim sDepthScale As String = Mid(sName, 22, 8)
            Dim sTimestamp As String = Mid(sName, 32, 16)
            Dim sDomain As String = Mid(sName, 50, 1)
            Dim oDomain As RS.TimestampDomain = RS.TimestampDomain.HardwareClock
            Dim sStream As String = Mid(sName, 53, Len(sName) - 53)
            Dim iKey As Integer = 0
            Dim iValue As Integer = 0
            Dim fTimestamp As Double = Converter.ByteStringToDouble(sTimestamp)
            Dim fDepthScale As Single = 0
            Dim oStreamType As RealSenseCapture.StreamTypeEnum = RealSenseCapture.StreamTypeEnum.None
            If Integer.TryParse(sKey, iKey) AndAlso Integer.TryParse(sValue, iValue) AndAlso Single.TryParse(sDepthScale, fDepthScale) AndAlso Integer.TryParse(sDomain, oDomain) AndAlso [Enum].TryParse(Of RealSenseCapture.StreamTypeEnum)(sStream, oStreamType) Then
                If Not oPlayDictionary.ContainsKey(iValue) Then
                    oPlayDictionary.Add(iValue, New List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single)))
                End If
                oPlayDictionary(iValue).Add(New Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single)(iKey, oFileInfo.FullName, oStreamType, oDomain, fTimestamp, fDepthScale))
            End If
        Next

        ' sort by time
        oPlayDictionary = (From oKeyValuePair In oPlayDictionary Order By oKeyValuePair.Key Ascending Select oKeyValuePair).ToDictionary(Function(x) x.Key, Function(x) x.Value)

        Return oPlayDictionary
    End Function
    Private Function Segment(ByVal iTask As Int32, ByVal oMatrix As Matrix(Of Byte), ByRef iTotalDuration As Long) As Matrix(Of Byte)
        ' segments image
        ' init segmenter first, then exit segmenter once video sequence is complete
        Dim iMatStructSize As Integer = Marshal.SizeOf(GetType(MatStruct))

        Dim oMatStructIn As New MatStruct(oMatrix)
        Dim oMatPointerIn As IntPtr = Marshal.AllocCoTaskMem(iMatStructSize)
        Marshal.StructureToPtr(oMatStructIn, oMatPointerIn, False)

        Dim oMatBufferIn As Byte() = oMatrix.Bytes
        Dim oBufferHandleIn As GCHandle = GCHandle.Alloc(oMatBufferIn, GCHandleType.Pinned)

        Dim oMask As New Matrix(Of Byte)(oMatrix.Height, oMatrix.Width, 3)

        Dim oMatStructOut As New MatStruct(oMask)
        Dim oMatPointerOut As IntPtr = Marshal.AllocCoTaskMem(iMatStructSize)
        Marshal.StructureToPtr(oMatStructOut, oMatPointerOut, False)

        Dim oMatBufferOut As Byte() = oMask.Bytes
        Dim oBufferHandleOut As GCHandle = GCHandle.Alloc(oMatBufferOut, GCHandleType.Pinned)

        Dim iDuration As Integer = 0
        Dim bSuccess As Boolean = segment(iTask, oMatPointerIn, oBufferHandleIn.AddrOfPinnedObject, oMatPointerOut, oBufferHandleOut.AddrOfPinnedObject, iDuration)
        If bSuccess Then
            iTotalDuration += iDuration
        End If

        Marshal.FreeCoTaskMem(oMatPointerIn)
        oBufferHandleIn.Free()

        Marshal.FreeCoTaskMem(oMatPointerOut)
        oBufferHandleOut.Free()

        Using oReturnArray As Matrix(Of Byte) = oMatStructOut.GetMatrix(Of Byte)(oMatBufferOut)
            oReturnArray.Mat.CopyTo(oMask)
        End Using

        Return oMask
    End Function
#End Region
#Region "Classes"
    Private Class ByteFileWriter
        Public Property Active As ActiveEnum
        Private m_QueueByte As ConcurrentQueue(Of Tuple(Of String, MatrixData(Of Byte), ClipData))
        Private m_QueueUShort As ConcurrentQueue(Of Tuple(Of String, MatrixData(Of UShort), ClipData))
        Private m_QueueData As ConcurrentQueue(Of Tuple(Of String, Byte(), ClipData))

        Sub New()
            m_QueueByte = New ConcurrentQueue(Of Tuple(Of String, MatrixData(Of Byte), ClipData))
            m_QueueUShort = New ConcurrentQueue(Of Tuple(Of String, MatrixData(Of UShort), ClipData))
            m_QueueData = New ConcurrentQueue(Of Tuple(Of String, Byte(), ClipData))
        End Sub
        Public Sub Start()
            ' wait until the writer is idle
            Do While Active <> ActiveEnum.Idle
                Threading.Thread.Sleep(100)
            Loop

            Active = ActiveEnum.Active
            ProcessQueue()
        End Sub
        Public Sub [Stop]()
            Active = ActiveEnum.Stopping
        End Sub
        Public Sub EnqueueByte(ByVal sPath As String, ByVal oMatrixData As MatrixData(Of Byte), ByRef oClipData As ClipData)
            m_QueueByte.Enqueue(New Tuple(Of String, MatrixData(Of Byte), ClipData)(sPath, oMatrixData, oClipData))
        End Sub
        Public Sub EnqueueUShort(ByVal sPath As String, ByVal oMatrixData As MatrixData(Of UShort), ByRef oClipData As ClipData)
            m_QueueUShort.Enqueue(New Tuple(Of String, MatrixData(Of UShort), ClipData)(sPath, oMatrixData, oClipData))
        End Sub
        Public Sub EnqueueData(ByVal sPath As String, ByVal oData As Byte(), ByRef oClipData As ClipData)
            m_QueueData.Enqueue(New Tuple(Of String, Byte(), ClipData)(sPath, oData, oClipData))
        End Sub
        Private Sub ProcessQueue()
            ' processes the queue until inactivated and the queue runs out
            Dim oThread = New Threading.Thread(Sub()
                                                   Do
                                                       If m_QueueData.Count > 0 Then
                                                           Dim oItem As Tuple(Of String, Byte(), ClipData) = Nothing
                                                           m_QueueData.TryDequeue(oItem)
                                                           IO.File.WriteAllBytes(oItem.Item1, oItem.Item2)
                                                           If Not IsNothing(oItem.Item3) Then
                                                               oItem.Item3.ProcessingFrames -= 1
                                                           End If
                                                       ElseIf m_QueueByte.Count > 0 Then
                                                           Dim oItem As Tuple(Of String, MatrixData(Of Byte), ClipData) = Nothing
                                                           m_QueueByte.TryDequeue(oItem)
                                                           IO.File.WriteAllBytes(oItem.Item1, oItem.Item2.GetBytes)
                                                           If Not IsNothing(oItem.Item3) Then
                                                               oItem.Item3.ProcessingFrames -= 1
                                                           End If
                                                       ElseIf m_QueueUShort.Count > 0 Then
                                                           Dim oItem As Tuple(Of String, MatrixData(Of UShort), ClipData) = Nothing
                                                           m_QueueUShort.TryDequeue(oItem)
                                                           IO.File.WriteAllBytes(oItem.Item1, oItem.Item2.GetBytes)
                                                           If Not IsNothing(oItem.Item3) Then
                                                               oItem.Item3.ProcessingFrames -= 1
                                                           End If
                                                       Else
                                                           Threading.Thread.Sleep(10)
                                                       End If
                                                   Loop While (Active = ActiveEnum.Active) OrElse m_QueueData.Count > 0 OrElse m_QueueByte.Count > 0 OrElse m_QueueUShort.Count > 0
                                                   Active = ActiveEnum.Idle
                                               End Sub)
            oThread.Priority = Threading.ThreadPriority.Lowest
            oThread.Start()
        End Sub
        Public Enum ActiveEnum As Integer
            Idle = 0
            Active
            Stopping
        End Enum
    End Class
    Public Class Processor
        Implements IDisposable

#Region "Constants"
        Private Const SmallWidth As Integer = 640

        ' number of face landmarks
        Private Const LandmarkCount As Integer = 68
        ' alternate angles for face detection
        Public Shared FaceDetectionAngles As List(Of Integer) = {-90, -60, -30, 30, 60, 90}.ToList
#End Region

        Sub New()
        End Sub
        Public Sub Reset()
        End Sub
        Public Function DetectFaces(ByVal oColourMatrix As Matrix(Of Byte)) As List(Of FaceResult)
            ' returns the detected face regions
            Dim oRotatedRegions As New List(Of FaceResult)
            If Not IsNothing(oColourMatrix) Then
                Dim fSmallScale As Double = SmallWidth / oColourMatrix.Width
                Dim iSmallHeight As Integer = oColourMatrix.Height * fSmallScale

                Dim oRegions As New KeyValuePair(Of Integer, List(Of FaceResult))(0, New List(Of FaceResult))
                Dim oSmallRegions As New List(Of Rect)

                Using oMonoMatrix As New Matrix(Of Byte)(oColourMatrix.Size)
                    CvInvoke.CvtColor(oColourMatrix, oMonoMatrix, CvEnum.ColorConversion.Bgr2Gray)
                    Using oSmallMonoMatrix As New Matrix(Of Byte)(iSmallHeight, SmallWidth)
                        CvInvoke.Resize(oMonoMatrix, oSmallMonoMatrix, oSmallMonoMatrix.Size)

                        ' use CLAHE to improve contrast for face detection
                        CvInvoke.CLAHE(oSmallMonoMatrix, 2, New System.Drawing.Size(8, 8), oSmallMonoMatrix)

                        ' check on unrotated faces to see if detection possible
                        Dim oUnrotatedResizedFaces As List(Of FaceResult) = FaceDetectProcessing(oSmallMonoMatrix, fSmallScale)

                        If oUnrotatedResizedFaces.Count > 0 Then
                            oRegions = New KeyValuePair(Of Integer, List(Of FaceResult))(0, oUnrotatedResizedFaces)
                        Else
                            ' check rotated faces to see if detection possible
                            ' get the matrix dictionary for processing
                            Dim oMatrixDictionary As Dictionary(Of Integer, Tuple(Of Matrix(Of Byte), [Structure].RotatedRect)) = GetDetectionMatrices(oSmallMonoMatrix)
                            Dim oResizedFacesDictionary As New ConcurrentDictionary(Of Integer, List(Of FaceResult))
                            Dim oActionList As New List(Of Tuple(Of Action(Of Object), Object))

                            Dim oAction As Action(Of Object) = Sub(oParam As KeyValuePair(Of Integer, Tuple(Of Matrix(Of Byte), [Structure].RotatedRect)))
                                                                   Dim oLocalResizedFaces As List(Of FaceResult) = FaceDetectProcessing(oParam.Value.Item1, fSmallScale)

                                                                   If oLocalResizedFaces.Count > 0 Then
                                                                       Dim iAngle As Integer = oParam.Key
                                                                       For i = 0 To oLocalResizedFaces.Count - 1
                                                                           oLocalResizedFaces(i).Region.Angle = oLocalResizedFaces(i).Region.Angle - iAngle
                                                                       Next
                                                                       oResizedFacesDictionary.TryAdd(iAngle, oLocalResizedFaces)
                                                                   End If
                                                               End Sub

                            For i = 0 To oMatrixDictionary.Count - 1
                                oActionList.Add(New Tuple(Of Action(Of Object), Object)(oAction, oMatrixDictionary.ToList(i)))
                            Next

                            CommonFunctions.ProtectedRunTasks(oActionList)

                            ' get ordered dictionary
                            If oResizedFacesDictionary.Count > 0 Then
                                Dim oOrderedFacesDictionary As Dictionary(Of Integer, List(Of FaceResult)) = oResizedFacesDictionary.ToList.OrderBy(Function(x) x.Key).ToDictionary(Function(x) x.Key, Function(x) x.Value)
                                Dim iMedianIndex As Integer = (oOrderedFacesDictionary.Count - 1) / 2
                                oRegions = New KeyValuePair(Of Integer, List(Of FaceResult))(oOrderedFacesDictionary.Keys(iMedianIndex), oOrderedFacesDictionary.Values(iMedianIndex))
                            End If

                            ' clean up
                            For i = 0 To oMatrixDictionary.Values.Count - 1
                                If (Not IsNothing(oMatrixDictionary.Values(i).Item1)) AndAlso (Not oMatrixDictionary.Values(i).Item1.Ptr.Equals(IntPtr.Zero)) Then
                                    oMatrixDictionary.Values(i).Item1.Dispose()
                                End If
                            Next
                            oMatrixDictionary.Clear()
                        End If

                    End Using
                End Using

                ' unrotated points
                If oRegions.Value.Count > 0 Then
                    Dim oDestSize As New System.Drawing.Size
                    Using oRotationMatrix As Matrix(Of Double) = CommonFunctions.GetRotationMatrix(oRegions.Key, oColourMatrix.Size, True, False, oDestSize)
                        ' get inverse matrix
                        Using oInverseRotationMatrix As Matrix(Of Double) = New Matrix(Of Double)(2, 3)
                            CvInvoke.InvertAffineTransform(oRotationMatrix, oInverseRotationMatrix)

                            ' process each face
                            Dim oPointsList As New List(Of System.Drawing.PointF)
                            For i = 0 To oRegions.Value.Count - 1
                                oPointsList.Add(oRegions.Value(i).Region.Center)
                                oPointsList.AddRange(oRegions.Value(i))
                            Next

                            ' unrotate points
                            Dim oPointsArray As System.Drawing.PointF() = oPointsList.ToArray
                            CommonFunctions.TransformPoints(oPointsArray, oInverseRotationMatrix)

                            Dim iFacePoints As Integer = 1 + LandmarkCount
                            For i = 0 To oRegions.Value.Count - 1
                                Dim oRotatedRect As [Structure].RotatedRect = oRegions.Value(i).Region
                                Dim oTransformedCenterPoint As System.Drawing.PointF = oPointsArray(i * iFacePoints)
                                oRotatedRect.Center = oTransformedCenterPoint

                                Dim oTransformedLandmarks As New List(Of System.Drawing.PointF)
                                For j = 1 To LandmarkCount
                                    oTransformedLandmarks.Add(oPointsArray((i * iFacePoints) + j))
                                Next

                                oRotatedRegions.Add(New FaceResult(oRotatedRect, oTransformedLandmarks))
                            Next
                        End Using
                    End Using
                End If
            End If
            Return oRotatedRegions
        End Function
        Private Shared Function FaceDetectProcessing(ByVal oMatrix As Matrix(Of Byte), ByVal fSmallScale As Double) As List(Of FaceResult)
            ' face detector using libfacedetector
            Const DetectBufferSize As Integer = &H20000
            Dim oBuffer(DetectBufferSize - 1) As Byte
            Dim oBufferHandle As GCHandle = GCHandle.Alloc(oBuffer, GCHandleType.Pinned)
            Dim pBuffer As IntPtr = oBufferHandle.AddrOfPinnedObject
            Dim oImageBuffer As Byte() = oMatrix.Bytes
            Dim oImageHandle As GCHandle = GCHandle.Alloc(oImageBuffer, GCHandleType.Pinned)
            Dim pImage As IntPtr = oImageHandle.AddrOfPinnedObject

            Dim pResults As IntPtr = FaceWrapper.FaceWrapper.FD_multiview_reinforce(pBuffer, pImage, oMatrix.Width, oMatrix.Height, oMatrix.Mat.Step, 1.2, 2, 48, 0, 1)

            Dim oRegions As New List(Of Rect)
            Dim oFaceResult As New List(Of FaceResult)
            Dim iFacesDetected As Integer = Marshal.ReadInt32(pResults)
            For i = 0 To iFacesDetected - 1
                Dim iOffset As Integer = 4 + ((142 * 2) * i)

                ' extend rectangle to include landmarks
                Dim oLandmarks As New List(Of System.Drawing.PointF)
                For j = 0 To LandmarkCount - 1
                    Dim iPointX As Short = GetShortOffset(pResults, iOffset, 6 + (j * 2))
                    Dim iPointY As Short = GetShortOffset(pResults, iOffset, 6 + (j * 2) + 1)

                    oLandmarks.Add(New System.Drawing.PointF(iPointX / fSmallScale, iPointY / fSmallScale))
                Next

                Dim oFaceRect As [Structure].RotatedRect = CvInvoke.MinAreaRect(New Util.VectorOfPointF(oLandmarks.ToArray))

                ' add face to list
                oFaceResult.Add(New FaceResult(oFaceRect, oLandmarks))
            Next

            ' unpin array
            oImageHandle.Free()
            oBufferHandle.Free()

            Return oFaceResult
        End Function
        Private Shared Function GetShortOffset(ByVal pPointer As IntPtr, ByVal iOffset As Integer, ByVal iCount As Integer) As Short
            Return Marshal.ReadInt16(pPointer, iOffset + (iCount * 2))
        End Function
        Private Shared Function GetDetectionMatrices(ByVal oSmallMonoMatrix As Matrix(Of Byte)) As Dictionary(Of Integer, Tuple(Of Matrix(Of Byte), [Structure].RotatedRect))
            ' generates a dictionary of matrices rotated according to preset detection angles
            Dim oReturnDictionary As New Dictionary(Of Integer, Tuple(Of Matrix(Of Byte), [Structure].RotatedRect))

            For Each iAngle In FaceDetectionAngles
                Dim oDestSize As New System.Drawing.Size
                Using oRotationMatrix As Matrix(Of Double) = CommonFunctions.GetRotationMatrix(iAngle, oSmallMonoMatrix.Size, True, False, oDestSize)
                    Dim oRotatedMonoMatrix As Matrix(Of Byte) = CommonFunctions.RotateMatrix(oSmallMonoMatrix, New [Structure].MCvScalar(0, 0, 0), oDestSize, oRotationMatrix)

                    Dim oDestCenter As New System.Drawing.PointF(oDestSize.Width / 2, oDestSize.Height / 2)
                    Dim oDestRotatedRect As New [Structure].RotatedRect(oDestCenter, oRotatedMonoMatrix.Size, -iAngle)

                    oReturnDictionary.Add(iAngle, New Tuple(Of Matrix(Of Byte), [Structure].RotatedRect)(oRotatedMonoMatrix, oDestRotatedRect))
                End Using
            Next

            Return oReturnDictionary
        End Function
        Public Class FaceResult
            Inherits List(Of System.Drawing.PointF)
            Implements IDisposable

            Public Region As [Structure].RotatedRect

            Sub New()
                Me.Clear()
                Region = [Structure].RotatedRect.Empty
            End Sub
            Sub New(ByVal oRegion As [Structure].RotatedRect, ByVal oPoints As List(Of System.Drawing.PointF))
                Me.Clear()
                Me.AddRange(oPoints)
                Region = oRegion
            End Sub
            Public Function UpdateResult(ByVal oNewLocation As System.Drawing.PointF) As FaceResult
                ' updates with the new location displacement
                Dim fDisplacementX As Double = oNewLocation.X - Region.Center.X
                Dim fDisplacementY As Double = oNewLocation.Y - Region.Center.Y

                Dim oNewFaceResult As New FaceResult
                With oNewFaceResult
                    If Not IsNothing(Region) Then
                        .Region = New [Structure].RotatedRect(New System.Drawing.PointF(Region.Center.X + fDisplacementX, Region.Center.Y + fDisplacementY), Region.Size, Region.Angle)
                    End If

                    For Each oPoint In Me
                        oNewFaceResult.Add(New System.Drawing.PointF(oPoint.X + fDisplacementX, oPoint.Y + fDisplacementY))
                    Next

                    .Region.Center = oNewLocation
                End With
                Return oNewFaceResult
            End Function
#Region "IDisposable Support"
            Private disposedValue As Boolean ' To detect redundant calls

            ' IDisposable
            Protected Overridable Sub Dispose(disposing As Boolean)
                If Not disposedValue Then
                    If disposing Then
                        ' TODO: dispose managed state (managed objects).
                    End If

                    ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
                    ' TODO: set large fields to null.
                End If
                disposedValue = True
            End Sub

            ' TODO: override Finalize() only if Dispose(disposing As Boolean) above has code to free unmanaged resources.
            'Protected Overrides Sub Finalize()
            '    ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
            '    Dispose(False)
            '    MyBase.Finalize()
            'End Sub

            ' This code added by Visual Basic to correctly implement the disposable pattern.
            Public Sub Dispose() Implements IDisposable.Dispose
                ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
                Dispose(True)
                ' TODO: uncomment the following line if Finalize() is overridden above.
                ' GC.SuppressFinalize(Me)
            End Sub
#End Region
        End Class
#Region "IDisposable Support"
        Private disposedValue As Boolean
        Protected Shadows Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    ' TODO: dispose managed state (managed objects).
                End If
            End If
            disposedValue = True
        End Sub
        Public Shadows Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
        End Sub
#End Region
    End Class
    Private Class FrameDisplay
        Implements IDisposable, INotifyPropertyChanged

        Public CurrentFrame As Integer
        Public CurrentSelection As List(Of Point)
        Public LoadVideoFrame As Boolean
        Public LoadVideoFrameState As PlayFrameEnum
        Public FrameCount As Integer
        Private m_SelectedFrame As Integer
        Private m_FPS As Double
        Private m_VideoName As String
        Private m_Name As String
        Private m_Selections As Dictionary(Of Integer, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)))
        Private m_ColourVideoReader As Accord.Video.FFMPEG.VideoFileReader
        Private m_DepthVideoReader As Accord.Video.FFMPEG.VideoFileReader

#Region "INotifyPropertyChanged"
        Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged
        Private Sub OnPropertyChangedLocal(ByVal sName As String)
            RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(sName))
        End Sub
#End Region
        Public ReadOnly Property FramesDisplay As TrueObservableCollection(Of HCBDisplay)
        Public Property SelectedFrame As HCBDisplay
            Get
                If m_SelectedFrame < 0 Or m_SelectedFrame >= FrameCount Then
                    Return New HCBDisplay
                Else
                    Dim fFrameDuration As Double = 1.0 / CDbl(m_FPS)
                    Return New HCBDisplay((m_SelectedFrame + 1).ToString, False, , CInt(CDbl(m_SelectedFrame) * 1000.0 * fFrameDuration))
                End If
            End Get
            Set(value As HCBDisplay)
                Dim oFramesDisplay As List(Of HCBDisplay) = FramesDisplay.ToList
                If FramesDisplay.Contains(value) Then
                    m_SelectedFrame = oFramesDisplay.IndexOf(value)
                Else
                    m_SelectedFrame = -1
                End If
                OnPropertyChangedLocal("SelectedFrame")
            End Set
        End Property
        Sub New(ByRef oFramesElement As FrameworkElement)
            LoadVideoFrame = False
            LoadVideoFrameState = PlayFrameEnum.Paused

            FramesDisplay = New TrueObservableCollection(Of HCBDisplay)
            FramesDisplay.SetBoundElement(oFramesElement)

            FrameCount = 0
            m_FPS = 0
            m_VideoName = String.Empty
            m_Name = String.Empty
            m_Selections = New Dictionary(Of Integer, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)))
            m_ColourVideoReader = Nothing
            m_DepthVideoReader = Nothing
            CurrentSelection = New List(Of Point)

            ResetFrames()
            OnPropertyChangedLocal("SelectedFrame")
        End Sub
        Public Sub SetVideo(ByVal sVideoName As String, ByVal sClipName As String)
            ' sets the video
            Dim iTotalFrames As Integer = 0
            Dim sColourVideoFileName As String = String.Empty
            Dim sDepthVideoFileName As String = String.Empty
            Dim bParsed As Boolean = False
            If IO.Directory.Exists(sVideoName) Then
                Dim oDirectoryInfo As New IO.DirectoryInfo(sVideoName)
                Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(sClipName) + "_Colour.avi").ToList
                If oFileInfoList.Count > 0 Then
                    Dim oFileInfo As IO.FileInfo = oFileInfoList.First
                    Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
                    sColourVideoFileName = oFileInfo.FullName
                    Dim sTotalFrames As String = Mid(sName, 2, 6)
                    Dim iParseFrames As Integer = -1
                    If Integer.TryParse(sTotalFrames, iParseFrames) Then
                        iTotalFrames = iParseFrames
                        bParsed = True
                    End If
                End If

                oFileInfoList = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(sClipName) + "_GroundTruth.avi").ToList
                If oFileInfoList.Count > 0 Then
                    Dim oFileInfo As IO.FileInfo = oFileInfoList.First
                    Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
                    sDepthVideoFileName = oFileInfo.FullName
                    Dim sTotalFrames As String = Mid(sName, 2, 6)
                    Dim iParseFrames As Integer = -1
                    If Integer.TryParse(sTotalFrames, iParseFrames) Then
                        iTotalFrames = iParseFrames
                        bParsed = True
                    End If
                End If
            End If

            If bParsed Then
                FrameCount = iTotalFrames
                m_VideoName = sVideoName
                m_Name = CommonFunctions.SafeFileName(sClipName)

                If Not IsNothing(m_ColourVideoReader) Then
                    m_ColourVideoReader.Dispose()
                    m_ColourVideoReader = Nothing
                End If
                If Not IsNothing(m_DepthVideoReader) Then
                    m_DepthVideoReader.Dispose()
                    m_DepthVideoReader = Nothing
                End If

                If IO.File.Exists(sColourVideoFileName) Then
                    m_ColourVideoReader = New Accord.Video.FFMPEG.VideoFileReader
                    m_ColourVideoReader.Open(sColourVideoFileName)
                    m_FPS = m_ColourVideoReader.FrameRate.ToDouble
                End If
                If IO.File.Exists(sDepthVideoFileName) Then
                    m_DepthVideoReader = New Accord.Video.FFMPEG.VideoFileReader
                    m_DepthVideoReader.Open(sDepthVideoFileName)
                    m_FPS = m_DepthVideoReader.FrameRate.ToDouble
                End If

                ResetFrames()
            End If
        End Sub
        Public Function GetCurrentFrame() As Tuple(Of Matrix(Of Byte), Matrix(Of Byte), String, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)))
            ' gets the current frame
            Dim oColourMatrix As Matrix(Of Byte) = Nothing
            Dim oDepthMatrix As Matrix(Of Byte) = Nothing

            If (Not IsNothing(m_ColourVideoReader)) AndAlso CurrentFrame >= 0 AndAlso CurrentFrame < FrameCount Then
                Using oColourBitmap As System.Drawing.Bitmap = m_ColourVideoReader.ReadVideoFrame(CurrentFrame)
                    oColourMatrix = Converter.BitmapToMatrix(oColourBitmap)
                End Using
            End If
            If (Not IsNothing(m_DepthVideoReader)) AndAlso CurrentFrame >= 0 AndAlso CurrentFrame < FrameCount Then
                Using oDepthBitmap As System.Drawing.Bitmap = m_DepthVideoReader.ReadVideoFrame(CurrentFrame)
                    Using oRawDepthMatrix = Converter.BitmapToMatrix(oDepthBitmap)
                        oDepthMatrix = New Matrix(Of Byte)(oRawDepthMatrix.Size)
                        CvInvoke.CvtColor(oRawDepthMatrix, oDepthMatrix, CvEnum.ColorConversion.Bgr2Gray)
                    End Using
                End Using
            End If

            Dim fFrameDuration As Double = 1.0 / CDbl(m_FPS)
            Dim sTiming As String = CInt(CDbl(CurrentFrame) * 1000.0 * fFrameDuration).ToString
            Return New Tuple(Of Matrix(Of Byte), Matrix(Of Byte), String, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)))(oColourMatrix, oDepthMatrix, sTiming, m_Selections(CurrentFrame))
        End Function
        Public Sub FrameClick(ByVal oNormalisedPoint As Point, ByVal oButtonType As ButtonTypeEnum, ByVal bMarkSelection As Boolean)
            If oButtonType = ButtonTypeEnum.LeftButton Then
                If bMarkSelection Then
                    CurrentSelection.Add(oNormalisedPoint)
                End If
            Else
                If bMarkSelection Then
                    ' remove nearest mark in selection mode
                    If CurrentSelection.Count > 0 Then
                        Dim oDistanceList As List(Of Tuple(Of Integer, Double)) = (From iIndex In Enumerable.Range(0, CurrentSelection.Count) Select New Tuple(Of Integer, Double)(iIndex, GetDistance(oNormalisedPoint, CurrentSelection(iIndex)))).OrderBy(Function(x) x.Item2).ToList
                        CurrentSelection.RemoveAt(oDistanceList.First.Item1)
                    End If
                Else
                    ' remove nearest selection when not in selection mode
                    If m_Selections(CurrentFrame).Count > 0 Then
                        Dim oDistanceList As List(Of Tuple(Of Integer, Double)) = (From iIndex In Enumerable.Range(0, m_Selections(CurrentFrame).Count) Select New Tuple(Of Integer, Double)(iIndex, GetDistance(oNormalisedPoint, m_Selections(CurrentFrame)(iIndex).Item2))).OrderBy(Function(x) x.Item2).ToList
                        m_Selections(CurrentFrame).RemoveAt(oDistanceList.First.Item1)
                    End If
                End If
            End If
        End Sub
        Public Sub AddCurrentSelection(ByVal bSelectForeground As Boolean, ByVal bSelectBackground As Boolean)
            ' adds the current selection of points to the frame contour list
            If CurrentSelection.Count >= 3 Then
                Dim oContourPoints As System.Drawing.PointF() = (From oPoint In CurrentSelection Select New System.Drawing.PointF(oPoint.X, oPoint.Y)).ToArray
                Using oContour As New Util.VectorOfPointF(oContourPoints)
                    Using oMoments As Moments = CvInvoke.Moments(oContour)
                        Dim oCentroid As New Point(oMoments.M10 / oMoments.M00, oMoments.M01 / oMoments.M00)
                        Dim oExpandedPoints As System.Drawing.Point() = oContourPoints.Select(Function(x) New System.Drawing.Point(x.X * 1000.0, x.Y * 1000.0)).ToArray
                        Dim oBoundingRect As System.Drawing.Rectangle = CvInvoke.BoundingRectangle(New Util.VectorOfPoint(oExpandedPoints))
                        Dim oSelectionType As SelectionTypeEnum = SelectionTypeEnum.None
                        If bSelectForeground Then
                            oSelectionType = SelectionTypeEnum.Foreground
                        ElseIf bSelectBackground Then
                            oSelectionType = SelectionTypeEnum.Background
                        End If

                        m_Selections(CurrentFrame).Add(New Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)(New List(Of Point)(CurrentSelection), oCentroid, CDbl(oBoundingRect.Width * oBoundingRect.Height) / 1000000.0, oSelectionType))
                    End Using
                End Using
            End If

            CurrentSelection.Clear()
        End Sub
        Public Function LoadSelections(ByVal oClipData As ClipData) As Boolean
            ' loads the clip selections if present
            Dim bProcessed As Boolean = False
            Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(oClipData.Name)
            Dim sSaveFileName As String = sFolderName + "\" + SelectionFileName

            If IO.File.Exists(sSaveFileName) Then
                Dim oSelections As Dictionary(Of Integer, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum))) = CommonFunctions.DeserializeDataContractFile(Of Dictionary(Of Integer, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum))))(sSaveFileName)
                If oSelections.Count = FrameCount Then
                    m_Selections.Clear()
                    For Each oSelection In oSelections
                        m_Selections.Add(oSelection.Key, oSelection.Value)
                    Next
                    bProcessed = True
                End If
            End If

            Return bProcessed
        End Function
        Public Sub SaveSelections(ByVal oClipData As ClipData)
            ' saves the clip selections
            Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(oClipData.Name)
            Dim sSaveFileName As String = sFolderName + "\" + SelectionFileName

            ' create backup
            If IO.File.Exists(sSaveFileName) Then
                Dim sBakFileName As String = sFolderName + "\" + SelectionBakName
                If IO.File.Exists(sBakFileName) Then
                    IO.File.Delete(sBakFileName)
                End If
                IO.File.Move(sSaveFileName, sBakFileName)
            End If

            CommonFunctions.SerializeDataContractFile(sSaveFileName, m_Selections)
        End Sub
        Public Sub CopySelections(ByVal iOldFrame As Integer, ByVal iNewFrame As Integer)
            ' copies markers from the old to the new frame
            m_Selections(iNewFrame).Clear()
            m_Selections(iNewFrame).AddRange(m_Selections(iOldFrame))
        End Sub
        Public Sub Clear()
            ' clears the frame dictionary
            FrameCount = 0
            m_FPS = 0
            m_VideoName = String.Empty
            m_Name = String.Empty
            If Not IsNothing(m_ColourVideoReader) Then
                m_ColourVideoReader.Dispose()
                m_ColourVideoReader = Nothing
            End If
            If Not IsNothing(m_DepthVideoReader) Then
                m_DepthVideoReader.Dispose()
                m_DepthVideoReader = Nothing
            End If

            ResetFrames()
            OnPropertyChangedLocal("SelectedFrame")
        End Sub
        Private Sub ResetFrames()
            ' resets the frame list
            Dim iTotalFrames As Integer = 0
            Dim bParsed As Boolean = False
            If IO.Directory.Exists(m_VideoName) Then
                Dim oDirectoryInfo As New IO.DirectoryInfo(m_VideoName)
                Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_Name) + "_Colour.avi").ToList
                If oFileInfoList.Count > 0 Then
                    Dim oFileInfo As IO.FileInfo = oFileInfoList.First
                    Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
                    Dim sTotalFrames As String = Mid(sName, 2, 6)
                    Dim iParseFrames As Integer = -1
                    If Integer.TryParse(sTotalFrames, iParseFrames) Then
                        iTotalFrames = iParseFrames
                        bParsed = True
                    End If
                End If

                oFileInfoList = oDirectoryInfo.EnumerateFiles("[??????]_" + CommonFunctions.SafeFileName(m_Name) + "_GroundTruth.avi").ToList
                If oFileInfoList.Count > 0 Then
                    Dim oFileInfo As IO.FileInfo = oFileInfoList.First
                    Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
                    Dim sTotalFrames As String = Mid(sName, 2, 6)
                    Dim iParseFrames As Integer = -1
                    If Integer.TryParse(sTotalFrames, iParseFrames) Then
                        iTotalFrames = iParseFrames
                        bParsed = True
                    End If
                End If
            End If

            m_Selections.Clear()
            For i = 0 To FrameCount - 1
                m_Selections.Add(i, New List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)))
            Next

            FramesDisplay.Clear()
            CurrentSelection.Clear()

            If bParsed Then
                m_SelectedFrame = 0
                CurrentFrame = 0

                Dim fFrameDuration As Double = 1.0 / CDbl(m_FPS)
                Dim oFramesDisplay As List(Of HCBDisplay) = (From iFrameNumber In Enumerable.Range(0, FrameCount) Select New HCBDisplay((iFrameNumber + 1).ToString, False, , CInt(CDbl(iFrameNumber) * 1000.0 * fFrameDuration))).ToList
                FramesDisplay.AddRange(oFramesDisplay)
                LoadVideoFrame = True
                SelectedFrame = FramesDisplay(m_SelectedFrame)
            Else
                m_SelectedFrame = -1
                CurrentFrame = -1
                LoadVideoFrame = False
                SelectedFrame = Nothing
            End If
        End Sub
        Private Function GetDistance(ByVal oPoint1 As Point, ByVal oPoint2 As Point) As Double
            Return Math.Sqrt((oPoint1.X - oPoint2.X) * (oPoint1.X - oPoint2.X) + (oPoint1.Y - oPoint2.Y) * (oPoint1.Y - oPoint2.Y))
        End Function
        Public Enum PlayFrameEnum As Integer
            Paused = 0
            Rewind
            Play
            PlayView
            PausedStop
            ContinuousPlay
        End Enum
#Region " IDisposable Support "
        Public Overloads Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub
        Public Sub Close()
            Dispose()
        End Sub
        Private Overloads Sub Dispose(ByVal disposing As Boolean)
            Dim oSyncObject As New Object
            SyncLock (oSyncObject)
                If disposing Then
                    'dispose of unmanaged resources
                    If Not IsNothing(m_ColourVideoReader) Then
                        m_ColourVideoReader.Dispose()
                        m_ColourVideoReader = Nothing
                    End If
                    If Not IsNothing(m_DepthVideoReader) Then
                        m_DepthVideoReader.Dispose()
                        m_DepthVideoReader = Nothing
                    End If
                End If
            End SyncLock
        End Sub
#End Region
    End Class
    <StructLayout(LayoutKind.Sequential, Pack:=1)> Public Structure MatStruct
        Dim Width As Int32 ''4 bytes
        Dim Height As Int32 ''4 bytes
        Dim Channels As Int32 ''4 bytes
        Dim Length As Int32 ''4 bytes
        Dim BaseType As Int32 ''4 bytes

        Sub New(ByVal oMatrixObject As Object)
            Select Case oMatrixObject.GetType
                Case GetType(Matrix(Of Byte))
                    Dim oMatrix As Matrix(Of Byte) = oMatrixObject
                    Width = Convert.ToInt32(oMatrix.Width)
                    Height = Convert.ToInt32(oMatrix.Height)
                    Channels = Convert.ToInt32(oMatrix.NumberOfChannels)
                    Length = Convert.ToInt32(oMatrix.Bytes.Length)
                    BaseType = 0
                Case GetType(Matrix(Of UShort))
                    Dim oMatrix As Matrix(Of UShort) = oMatrixObject
                    Width = Convert.ToInt32(oMatrix.Width)
                    Height = Convert.ToInt32(oMatrix.Height)
                    Channels = Convert.ToInt32(oMatrix.NumberOfChannels)
                    Length = Convert.ToInt32(oMatrix.Bytes.Length)
                    BaseType = 2
                Case GetType(Matrix(Of Short))
                    Dim oMatrix As Matrix(Of Short) = oMatrixObject
                    Width = Convert.ToInt32(oMatrix.Width)
                    Height = Convert.ToInt32(oMatrix.Height)
                    Channels = Convert.ToInt32(oMatrix.NumberOfChannels)
                    Length = Convert.ToInt32(oMatrix.Bytes.Length)
                    BaseType = 3
                Case GetType(Matrix(Of Integer))
                    Dim oMatrix As Matrix(Of Integer) = oMatrixObject
                    Width = Convert.ToInt32(oMatrix.Width)
                    Height = Convert.ToInt32(oMatrix.Height)
                    Channels = Convert.ToInt32(oMatrix.NumberOfChannels)
                    Length = Convert.ToInt32(oMatrix.Bytes.Length)
                    BaseType = 4
                Case GetType(Matrix(Of Single))
                    Dim oMatrix As Matrix(Of Single) = oMatrixObject
                    Width = Convert.ToInt32(oMatrix.Width)
                    Height = Convert.ToInt32(oMatrix.Height)
                    Channels = Convert.ToInt32(oMatrix.NumberOfChannels)
                    Length = Convert.ToInt32(oMatrix.Bytes.Length)
                    BaseType = 5
                Case GetType(Matrix(Of Double))
                    Dim oMatrix As Matrix(Of Double) = oMatrixObject
                    Width = Convert.ToInt32(oMatrix.Width)
                    Height = Convert.ToInt32(oMatrix.Height)
                    Channels = Convert.ToInt32(oMatrix.NumberOfChannels)
                    Length = Convert.ToInt32(oMatrix.Bytes.Length)
                    BaseType = 6
            End Select
        End Sub
        Function GetMatrix(Of T As Structure)(ByVal Bytes As Byte()) As Matrix(Of T)
            Return Converter.ArrayToMatrix(Of T)(Bytes, Width, Height, Channels)
        End Function
    End Structure
#End Region
#Region "Enumerations"
    Private Enum SpecialEnum
        GAU
        UNI
        INI
    End Enum
    Private Enum ButtonTypeEnum
        LeftButton
        RightButton
    End Enum
    Private Enum SelectionTypeEnum
        None
        Foreground
        Background
    End Enum
#End Region
#Region "Declarations"
    <DllImport("SegTestC.dll", EntryPoint:="initSegmenter", CallingConvention:=CallingConvention.StdCall)> Private Shared Sub initSegmenter(ByVal iTask As Int32, ByVal iSegType As Int32)
    End Sub
    <DllImport("SegTestC.dll", EntryPoint:="exitSegmenter", CallingConvention:=CallingConvention.StdCall)> Private Shared Sub exitSegmenter(ByVal iTask As Int32)
    End Sub
    <DllImport("SegTestC.dll", EntryPoint:="validbgs", CallingConvention:=CallingConvention.StdCall)> Private Shared Function validbgs(ByVal iTask As Int32) As Boolean
    End Function
    <DllImport("SegTestC.dll", EntryPoint:="segment", CallingConvention:=CallingConvention.StdCall)> Private Shared Function segment(ByVal iTask As Int32, ByVal oMatStructIn As IntPtr, ByVal oBufferIn As IntPtr, ByVal oMatStructOut As IntPtr, ByVal oBufferOut As IntPtr, ByRef iDuration As Int32) As Boolean
    End Function
#End Region
#Region "UI"
    Private Sub StartStop_Handler(sender As Object, e As EventArgs) Handles ButtonStartStop.Click
        Dim oAction As Action = Sub()
                                    If IsRunning Then
                                        StopPlaying()
                                    Else
                                        Dim iDuration As Integer = -1
                                        Dim oAlarms As New List(Of Integer)

                                        Dim oClipData As ClipData = TryCast(ClipDataGrid.SelectedItem, ClipData)
                                        If m_SaveVideo AndAlso (Not IsNothing(oClipData)) Then
                                            Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(oClipData.Name)
                                            If IO.Directory.Exists(sFolderName) Then
                                                If MessageBox.Show("Replace " + oClipData.Name + "?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                                    IO.Directory.Delete(sFolderName, True)
                                                    Do While IO.Directory.Exists(sFolderName)
                                                        Threading.Thread.Sleep(100)
                                                    Loop
                                                Else
                                                    Exit Sub
                                                End If
                                            End If

                                            IO.Directory.CreateDirectory(sFolderName)

                                            ' starts the byte file writer
                                            m_CaptureClipData = oClipData
                                            m_ByteFileWriter.Start()

                                            iDuration = ProcessCapture
                                            oAlarms.Add(oClipData.PreStart)
                                        End If

                                        StartPlaying(iDuration, oAlarms)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonSaveVideo_Handler(sender As Object, e As EventArgs) Handles ButtonSaveVideo.Click
        Dim oAction As Action = Sub()
                                    m_SaveVideo = Not m_SaveVideo
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonLoadFrame_Handler(sender As Object, e As EventArgs) Handles ButtonLoadFrame.Click
        Dim oAction As Action = Sub()
                                    If m_FrameDisplay.LoadVideoFrame Then
                                        If m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused Then
                                            StopLoadFrame()
                                        Else
                                            m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.PausedStop
                                        End If
                                    Else
                                        Dim oClipData As ClipData = TryCast(ClipDataGrid.SelectedItem, ClipData)
                                        If Not IsNothing(oClipData) Then
                                            Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(oClipData.Name)
                                            Dim sVideoName As String = sFolderName + "\" + VideoFolder
                                            If IO.Directory.Exists(sVideoName) Then
                                                m_CaptureClipData = oClipData
                                                m_ViewSelections = False
                                                m_CopySelections = False
                                                m_MarkSelection = False
                                                m_FrameDisplay.CurrentSelection.Clear()

                                                m_FrameDisplay.SetVideo(sVideoName, CommonFunctions.SafeFileName(oClipData.Name))

                                                m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused
                                                Task.Run(Sub()
                                                             LoadCurrentFrame()
                                                         End Sub)
                                            End If
                                        End If
                                    End If
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub Config_Handler(sender As Object, e As EventArgs) Handles ButtonConfig.Click
        Dim oAction As Action = Sub()
                                    Me.NavigationService.Navigate(PageConfiguration)
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub Exit_Handler(sender As Object, e As EventArgs) Handles ButtonExit.Click
        Dim oAction As Action = Sub()
                                    If MessageBox.Show("Exit " + MainWindow.ModuleName + "?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                        Page_Unloaded(Nothing, Nothing)
                                        Application.Current.Shutdown()
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ComboBoxDevice_SelectionChanged_Handler(sender As Object, e As SelectionChangedEventArgs) Handles ComboBoxDevice.SelectionChanged
        Dim oComboBoxMain As ComboBox = CType(sender, ComboBox)
        If Not IsNothing(oComboBoxMain.SelectedItem) Then
            m_RealSenseCapture.SelectedDevice = oComboBoxMain.SelectedItem
        End If
    End Sub
    Private Sub ComboBoxProfile_SelectionChanged_Handler(sender As Object, e As SelectionChangedEventArgs) Handles ComboBoxProfile.SelectionChanged
        Dim oComboBoxMain As ComboBox = CType(sender, ComboBox)
        If Not IsNothing(oComboBoxMain.SelectedItem) Then
            m_RealSenseCapture.SelectedProfile = oComboBoxMain.SelectedItem
        End If
    End Sub
    Private Sub ComboBoxResolution_SelectionChanged_Handler(sender As Object, e As SelectionChangedEventArgs) Handles ComboBoxResolution.SelectionChanged
        Dim oComboBoxMain As ComboBox = CType(sender, ComboBox)
        If Not IsNothing(oComboBoxMain.SelectedItem) Then
            m_RealSenseCapture.SelectedResolution = oComboBoxMain.SelectedItem
        End If
    End Sub
    Private Sub ComboBoxFrames_SelectionChanged_Handler(sender As Object, e As SelectionChangedEventArgs) Handles ComboBoxFrames.SelectionChanged
        Dim oComboBoxMain As ComboBox = CType(sender, ComboBox)
        If Not IsNothing(oComboBoxMain.SelectedItem) Then
            m_FrameDisplay.SelectedFrame = oComboBoxMain.SelectedItem
        End If
    End Sub
    Private Sub AddClip_Handler(sender As Object, e As EventArgs) Handles ButtonAddClip.Click
        Dim oAction As Action = Sub()
                                    If ClipDataGrid.SelectedIndex = -1 Then
                                        ClipDataStore.Add(New ClipData)
                                    Else
                                        ClipDataStore.Insert(ClipDataGrid.SelectedIndex, New ClipData)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub RemoveClip_Handler(sender As Object, e As EventArgs) Handles ButtonRemoveClip.Click
        Dim oAction As Action = Sub()
                                    If ClipDataGrid.SelectedIndex >= 0 Then
                                        ClipDataStore.RemoveAt(ClipDataGrid.SelectedIndex)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub LoadClips_Handler(sender As Object, e As EventArgs) Handles ButtonLoadClips.Click
        Dim oAction As Action = Sub()
                                    If MessageBox.Show("Load " + ClipFileName + "?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                        Dim sSaveFileName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + ClipFileName
                                        Dim oClipDataList As List(Of ClipData) = CommonFunctions.DeserializeDataContractFile(Of List(Of ClipData))(sSaveFileName)
                                        ClipDataStore.Clear()
                                        ClipDataStore.AddRange(oClipDataList)

                                        MessageBox.Show("Loaded " + ClipFileName, MainWindow.ModuleName, MessageBoxButton.OK, MessageBoxImage.Information)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub SaveClips_Handler(sender As Object, e As EventArgs) Handles ButtonSaveClips.Click
        Dim oAction As Action = Sub()
                                    If MessageBox.Show("Save " + ClipFileName + "?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                        SaveClip()

                                        MessageBox.Show("Saved " + ClipFileName, MainWindow.ModuleName, MessageBoxButton.OK, MessageBoxImage.Information)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ProcessVideo_Handler(sender As Object, e As EventArgs) Handles ButtonProcessVideo.Click
        Dim oAction As Action = Sub()
                                    Dim oClipData As ClipData = TryCast(ClipDataGrid.SelectedItem, ClipData)
                                    If Not IsNothing(oClipData) Then
                                        Task.Run(Sub()
                                                     PlayingStarted()

                                                     m_Converting = True
                                                     m_CaptureClipData = oClipData

                                                     Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name)
                                                     Dim sVideoName As String = sFolderName + "\" + VideoFolder

                                                     If ProcessColour(sFolderName, sVideoName, FPS) Then
                                                         oClipData.ProcessedVideo = True
                                                     End If

                                                     m_Converting = False
                                                     m_CaptureClipData = Nothing

                                                     PlayingFinished()
                                                 End Sub)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub GroundTruth_Handler(sender As Object, e As EventArgs) Handles ButtonGroundTruth.Click
        Dim oAction As Action = Sub()
                                    Dim oClipData As ClipData = TryCast(ClipDataGrid.SelectedItem, ClipData)
                                    If Not IsNothing(oClipData) Then
                                        m_CaptureClipData = oClipData
                                        Task.Run(Sub()
                                                     PlayingStarted()

                                                     m_Converting = True
                                                     m_CaptureClipData = oClipData

                                                     Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name)
                                                     Dim sVideoName As String = sFolderName + "\" + VideoFolder

                                                     m_ByteFileWriter.Start()

                                                     Dim bProcessed As Boolean = ConvertVideo(sFolderName, sVideoName)

                                                     ' wait until the writer is idle
                                                     m_ByteFileWriter.Stop()

                                                     Do While m_ByteFileWriter.Active <> ByteFileWriter.ActiveEnum.Idle
                                                         Threading.Thread.Sleep(100)
                                                     Loop

                                                     If bProcessed Then
                                                         If ProcessDepth(sFolderName, sVideoName, FPS) Then
                                                             oClipData.ProcessedGroundTruth = True
                                                         End If
                                                     End If

                                                     m_Converting = False
                                                     m_CaptureClipData = Nothing

                                                     PlayingFinished()
                                                 End Sub)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub Noise_Handler(sender As Object, e As EventArgs) Handles ButtonNoise.Click
        Dim oAction As Action = Sub()
                                    If MessageBox.Show("Generate Noise and Initialisation Clips?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                        Task.Run(Sub()
                                                     m_Converting = True

                                                     CreateNoise()

                                                     m_Converting = False
                                                 End Sub)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub Cut_Handler(sender As Object, e As EventArgs) Handles ButtonCut.Click
        Dim oAction As Action = Sub()
                                    Dim oClipData As ClipData = TryCast(ClipDataGrid.SelectedItem, ClipData)
                                    If Not IsNothing(oClipData) Then
                                        If MessageBox.Show("Cut Video for " + oClipData.Name + "?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                            Task.Run(Sub()
                                                         m_Converting = True
                                                         m_CaptureClipData = oClipData

                                                         Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name)

                                                         If CutClip(sFolderName) Then
                                                             m_CaptureClipData.ProcessedCut = True
                                                         End If

                                                         m_Converting = False
                                                         m_CaptureClipData = Nothing
                                                     End Sub)
                                        End If
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub Accuracy_Handler(sender As Object, e As EventArgs) Handles ButtonAccuracy.Click
        Dim oAction As Action = Sub()
                                    Dim oClipData As ClipData = TryCast(ClipDataGrid.SelectedItem, ClipData)
                                    If Not IsNothing(oClipData) Then
                                        m_CaptureClipData = oClipData
                                        Task.Run(Sub()
                                                     PlayingStarted()

                                                     m_Converting = True
                                                     m_CaptureClipData = oClipData

                                                     Dim sFolderName As String = Settings.DefaultSave + "\" + CaptureFolder + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name)
                                                     Dim sVideoName As String = sFolderName + "\" + VideoFolder

                                                     If GetSegmented(sVideoName) Then
                                                         m_CaptureClipData.ProcessedSegmented = True
                                                         CalculateAccuracy(sVideoName)
                                                     End If

                                                     m_Converting = False
                                                     m_CaptureClipData = Nothing

                                                     PlayingFinished()
                                                 End Sub)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub All_Handler(sender As Object, e As EventArgs) Handles ButtonAll.Click
        Dim oAction As Action = Sub()
                                    Task.Run(Sub()
                                                 PlayingStarted()

                                                 Dim UpdateDelegate1 As Action = Sub()
                                                                                     ClipDataGrid.IsEnabled = False
                                                                                 End Sub

                                                 CommonFunctions.SafeInvoke(UpdateDelegate1, UIDispatcher, True)

                                                 m_Converting = True

                                                 RunAll()

                                                 m_Converting = False

                                                 Dim UpdateDelegate2 As Action = Sub()
                                                                                     ClipDataGrid.IsEnabled = True
                                                                                 End Sub

                                                 CommonFunctions.SafeInvoke(UpdateDelegate2, UIDispatcher, True)

                                                 PlayingFinished()
                                             End Sub)
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub Export_Handler(sender As Object, e As EventArgs) Handles ButtonExport.Click
        Dim oAction As Action = Sub()
                                    If MessageBox.Show("Export Results?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                        ExportResults()
                                        MessageBox.Show("Exported", MainWindow.ModuleName, MessageBoxButton.OK, MessageBoxImage.Information)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonRewind_Handler(sender As Object, e As EventArgs) Handles ButtonRewind.Click
        Dim oAction As Action = Sub()
                                    Select Case m_FrameDisplay.LoadVideoFrameState
                                        Case FrameDisplay.PlayFrameEnum.Play, FrameDisplay.PlayFrameEnum.PlayView
                                            m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Rewind
                                        Case FrameDisplay.PlayFrameEnum.Paused
                                            m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Rewind
                                            m_FrameDisplay.CurrentSelection.Clear()

                                            Task.Run(Sub()
                                                         ContinuousLoadFrame()
                                                     End Sub)
                                    End Select
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonBack_Handler(sender As Object, e As EventArgs) Handles ButtonBack.Click
        Dim oAction As Action = Sub()
                                    If m_FrameDisplay.FrameCount > 0 AndAlso m_FrameDisplay.CurrentFrame > 0 Then
                                        If m_FrameDisplay.LoadVideoFrameState <> FrameDisplay.PlayFrameEnum.Paused And m_FrameDisplay.LoadVideoFrameState <> FrameDisplay.PlayFrameEnum.PausedStop Then
                                            m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused
                                        Else
                                            Dim iOldFrame As Integer = m_FrameDisplay.CurrentFrame
                                            m_FrameDisplay.CurrentFrame -= 1
                                            Dim iNewFrame As Integer = m_FrameDisplay.CurrentFrame
                                            If m_CopySelections AndAlso iOldFrame <> iNewFrame Then
                                                m_FrameDisplay.CopySelections(iOldFrame, iNewFrame)
                                            End If
                                            m_FrameDisplay.CurrentSelection.Clear()

                                            Task.Run(Sub()
                                                         LoadCurrentFrame()
                                                     End Sub)
                                        End If
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonPause_Handler(sender As Object, e As EventArgs) Handles ButtonPause.Click
        Dim oAction As Action = Sub()
                                    If m_FrameDisplay.LoadVideoFrameState <> FrameDisplay.PlayFrameEnum.PausedStop Then
                                        m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonForward_Handler(sender As Object, e As EventArgs) Handles ButtonForward.Click
        Dim oAction As Action = Sub()
                                    If m_FrameDisplay.FrameCount > 0 AndAlso m_FrameDisplay.CurrentFrame < m_FrameDisplay.FrameCount - 1 Then
                                        If m_FrameDisplay.LoadVideoFrameState <> FrameDisplay.PlayFrameEnum.Paused And m_FrameDisplay.LoadVideoFrameState <> FrameDisplay.PlayFrameEnum.PausedStop Then
                                            m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused
                                        Else
                                            Dim iOldFrame As Integer = m_FrameDisplay.CurrentFrame
                                            m_FrameDisplay.CurrentFrame += 1
                                            Dim iNewFrame As Integer = m_FrameDisplay.CurrentFrame
                                            If m_CopySelections AndAlso iOldFrame <> iNewFrame Then
                                                m_FrameDisplay.CopySelections(iOldFrame, iNewFrame)
                                            End If
                                            m_FrameDisplay.CurrentSelection.Clear()

                                            Task.Run(Sub()
                                                         LoadCurrentFrame()
                                                     End Sub)
                                        End If
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonPlay_Handler(sender As Object, e As EventArgs) Handles ButtonPlay.Click
        Dim oAction As Action = Sub()
                                    Select Case m_FrameDisplay.LoadVideoFrameState
                                        Case FrameDisplay.PlayFrameEnum.Rewind
                                            m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Play
                                        Case FrameDisplay.PlayFrameEnum.Paused
                                            m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Play
                                            m_FrameDisplay.CurrentSelection.Clear()

                                            Task.Run(Sub()
                                                         ContinuousLoadFrame()
                                                     End Sub)
                                    End Select
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonPlayView_Handler(sender As Object, e As EventArgs) Handles ButtonPlayView.Click
        Dim oAction As Action = Sub()
                                    Select Case m_FrameDisplay.LoadVideoFrameState
                                        Case FrameDisplay.PlayFrameEnum.Rewind
                                            m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.PlayView
                                        Case FrameDisplay.PlayFrameEnum.Paused
                                            m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.PlayView
                                            m_FrameDisplay.CurrentSelection.Clear()

                                            Task.Run(Sub()
                                                         ContinuousLoadFrame()
                                                     End Sub)
                                    End Select
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonEntrance_Handler(sender As Object, e As EventArgs) Handles ButtonEntrance.Click
        Dim oAction As Action = Sub()
                                    Dim oClipData As ClipData = TryCast(ClipDataGrid.SelectedItem, ClipData)
                                    If Not IsNothing(oClipData) Then
                                        If m_FrameDisplay.FrameCount > 0 AndAlso m_FrameDisplay.CurrentFrame >= 0 AndAlso m_FrameDisplay.CurrentFrame < m_FrameDisplay.FrameCount Then
                                            oClipData.ClipEntrance = m_FrameDisplay.CurrentFrame + 1
                                        End If
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ButtonSeated_Handler(sender As Object, e As EventArgs) Handles ButtonSeated.Click
        Dim oAction As Action = Sub()
                                    Dim oClipData As ClipData = TryCast(ClipDataGrid.SelectedItem, ClipData)
                                    If Not IsNothing(oClipData) Then
                                        If m_FrameDisplay.FrameCount > 0 AndAlso m_FrameDisplay.CurrentFrame >= 0 AndAlso m_FrameDisplay.CurrentFrame < m_FrameDisplay.FrameCount Then
                                            oClipData.ClipSeated = m_FrameDisplay.CurrentFrame + 1
                                        End If
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub Adjust_Handler(sender As Object, e As EventArgs) Handles ButtonAdjust.Click
        Dim oAction As Action = Sub()
                                    m_ViewSelections = Not m_ViewSelections
                                    SetIcons()

                                    If m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused Then
                                        Task.Run(Sub()
                                                     LoadCurrentFrame()
                                                 End Sub)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub Selection_Handler(sender As Object, e As EventArgs) Handles ButtonSelection.Click
        Dim oAction As Action = Sub()
                                    If m_MarkSelection Then
                                        m_FrameDisplay.AddCurrentSelection(m_SelectForeground, m_SelectBackground)
                                    End If
                                    m_MarkSelection = Not m_MarkSelection
                                    SetIcons()

                                    If m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused Then
                                        Task.Run(Sub()
                                                     LoadCurrentFrame()
                                                 End Sub)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub SelectForeground_Handler(sender As Object, e As EventArgs) Handles ButtonSelectForeground.Click
        Dim oAction As Action = Sub()
                                    m_SelectForeground = Not m_SelectForeground
                                    m_SelectBackground = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub SelectBackground_Handler(sender As Object, e As EventArgs) Handles ButtonSelectBackground.Click
        Dim oAction As Action = Sub()
                                    m_SelectBackground = Not m_SelectBackground
                                    m_SelectForeground = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub CopySelections_Handler(sender As Object, e As EventArgs) Handles ButtonCopySelections.Click
        Dim oAction As Action = Sub()
                                    m_CopySelections = Not m_CopySelections
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub LoadSelections_Handler(sender As Object, e As EventArgs) Handles ButtonLoadSelections.Click
        Dim oAction As Action = Sub()
                                    If MessageBox.Show("Load Selections for " + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                        If m_FrameDisplay.LoadSelections(m_CaptureClipData) Then
                                            If m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused Then
                                                Task.Run(Sub()
                                                             LoadCurrentFrame()
                                                         End Sub)
                                            End If

                                            MessageBox.Show("Loaded Selections", MainWindow.ModuleName, MessageBoxButton.OK, MessageBoxImage.Information)
                                        End If
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub SaveSelections_Handler(sender As Object, e As EventArgs) Handles ButtonSaveSelections.Click
        Dim oAction As Action = Sub()
                                    If MessageBox.Show("Save Selections for " + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                        m_FrameDisplay.SaveSelections(m_CaptureClipData)
                                        MessageBox.Show("Saved Selections", MainWindow.ModuleName, MessageBoxButton.OK, MessageBoxImage.Information)
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ImageL_LeftClick_Handler(sender As Object, e As MouseButtonEventArgs) Handles ImageL.MouseLeftButtonDown
        Dim oAction As Action = Sub()
                                    If m_FrameDisplay.LoadVideoFrame AndAlso m_ViewSelections Then
                                        Dim oElement As FrameworkElement = CType(sender, FrameworkElement)
                                        Dim oPoint As Point = e.MouseDevice.GetPosition(oElement)
                                        Dim oNormalisedPoint As New Point(oPoint.X / oElement.ActualWidth, oPoint.Y / oElement.ActualHeight)
                                        m_FrameDisplay.FrameClick(oNormalisedPoint, ButtonTypeEnum.LeftButton, m_MarkSelection)

                                        If m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused Then
                                            Task.Run(Sub()
                                                         LoadCurrentFrame()
                                                     End Sub)
                                        End If
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ImageL_RightClick_Handler(sender As Object, e As MouseButtonEventArgs) Handles ImageL.MouseRightButtonDown
        Dim oAction As Action = Sub()
                                    If m_FrameDisplay.LoadVideoFrame AndAlso m_ViewSelections Then
                                        Dim oElement As FrameworkElement = CType(sender, FrameworkElement)
                                        Dim oPoint As Point = e.MouseDevice.GetPosition(oElement)
                                        Dim oNormalisedPoint As New Point(oPoint.X / oElement.ActualWidth, oPoint.Y / oElement.ActualHeight)
                                        m_FrameDisplay.FrameClick(oNormalisedPoint, ButtonTypeEnum.RightButton, m_MarkSelection)

                                        If m_FrameDisplay.LoadVideoFrameState = FrameDisplay.PlayFrameEnum.Paused Then
                                            Task.Run(Sub()
                                                         LoadCurrentFrame()
                                                     End Sub)
                                        End If
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ClipRightClick_Handler(sender As Object, e As MouseButtonEventArgs)
        Dim oAction As Action = Sub()
                                    Dim oDataGrid As DataGrid = TryCast(sender, DataGrid)
                                    If Not IsNothing(oDataGrid) Then
                                        oDataGrid.SelectedIndex = -1
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
#End Region
End Class

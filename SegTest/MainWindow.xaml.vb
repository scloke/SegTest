Imports SegTest.Common
Imports System.IO

Class MainWindow
#Region "ProgramConstantsAndVariables"
    Public Const ModuleName As String = "SegTest"
#End Region
#Region "MainProgram"
    Public Sub New()
        InitializeComponent()
        AppReload()
        AppInit()
    End Sub
    Protected Overrides Sub OnClosed(e As EventArgs)
        MyBase.OnClosed(e)
        AppClose()
        AppPersist()
    End Sub
    Private Sub AppReload()
        ' Load configuration data
        If My.Settings.SettingStore = String.Empty Then
            Settings = New SettingsClass
        Else
            Try
                Using oMemoryStream As New MemoryStream(Convert.FromBase64String(My.Settings.SettingStore))
                    Settings = CommonFunctions.DeserializeDataContractStream(Of SettingsClass)(oMemoryStream, SettingsClass.GetKnownTypes)
                    If IsNothing(Settings) Then
                        Settings = New SettingsClass
                        MessageBox.Show("Settings file corrupted. New Settings file initialised.", ModuleName, MessageBoxButton.OK, MessageBoxImage.Warning)
                    End If
                End Using
            Catch ex As Exception
                Settings = New SettingsClass
                MessageBox.Show("Settings file corrupted. New Settings file initialised.", ModuleName, MessageBoxButton.OK, MessageBoxImage.Warning)
            End Try
        End If
    End Sub
    Private Sub AppPersist()
        ' save configuration data
        Using oMemoryStream As New MemoryStream
            CommonFunctions.SerializeDataContractStream(oMemoryStream, Settings, SettingsClass.GetKnownTypes)
            My.Settings.SettingStore = Convert.ToBase64String(oMemoryStream.ToArray)
            My.Settings.Save()
        End Using
    End Sub
    Private Sub AppInit()
        NTPTime.SetOffset()

        SetCommonIcons()

        PageMainWindow = Me
        PageMainPage = New MainPage
        PageConfiguration = New Configuration

        Me.NavigationService.Navigate(PageMainPage)
    End Sub
    Private Sub AppClose()
    End Sub
    Private Sub SetCommonIcons()
        CommonIcons = New Dictionary(Of String, ImageSource)

        CommonIcons.Add("XAMLConfiguration", Converter.XamlToDrawingImage(My.Resources.XAMLConfiguration))
        CommonIcons.Add("XAMLConfigurationGray", Converter.XamlToDrawingImage(My.Resources.XAMLConfigurationGray))
        CommonIcons.Add("XAMLExit", Converter.XamlToDrawingImage(My.Resources.XAMLExit))
        CommonIcons.Add("XAMLMain", Converter.XamlToDrawingImage(My.Resources.XAMLMain))
        CommonIcons.Add("XAMLSave", Converter.XamlToDrawingImage(My.Resources.XAMLSave))
        CommonIcons.Add("XAMLYes", Converter.XamlToDrawingImage(My.Resources.XAMLYes))
        CommonIcons.Add("XAMLNo", Converter.XamlToDrawingImage(My.Resources.XAMLNo))
        CommonIcons.Add("XAMLYesGrey", Converter.XamlToDrawingImage(My.Resources.XAMLYesGrey))
        CommonIcons.Add("XAMLNoGrey", Converter.XamlToDrawingImage(My.Resources.XAMLNoGrey))
        CommonIcons.Add("XAMLLeft", Converter.XamlToDrawingImage(My.Resources.XAMLLeft))
        CommonIcons.Add("XAMLRight", Converter.XamlToDrawingImage(My.Resources.XAMLRight))
        CommonIcons.Add("XAMLSaveVideoActive", Converter.XamlToDrawingImage(My.Resources.XAMLSaveVideoActive))
        CommonIcons.Add("XAMLSaveVideoInactive", Converter.XamlToDrawingImage(My.Resources.XAMLSaveVideoInactive))
        CommonIcons.Add("XAMLPlus", Converter.XamlToDrawingImage(My.Resources.XAMLPlus))
        CommonIcons.Add("XAMLMinus", Converter.XamlToDrawingImage(My.Resources.XAMLMinus))
        CommonIcons.Add("XAMLLoadFile", Converter.XamlToDrawingImage(My.Resources.XAMLLoadFile))
        CommonIcons.Add("XAMLSaveFile", Converter.XamlToDrawingImage(My.Resources.XAMLSaveFile))
        CommonIcons.Add("XAMLMarkerAccuracy", Converter.XamlToDrawingImage(My.Resources.XAMLMarkerAccuracy))
        CommonIcons.Add("XAMLMarkerGround", Converter.XamlToDrawingImage(My.Resources.XAMLMarkerGround))
        CommonIcons.Add("XAMLMarkerNoise", Converter.XamlToDrawingImage(My.Resources.XAMLMarkerNoise))
        CommonIcons.Add("XAMLMarkerVideo", Converter.XamlToDrawingImage(My.Resources.XAMLMarkerVideo))
        CommonIcons.Add("XAMLMarkerScissors", Converter.XamlToDrawingImage(My.Resources.XAMLMarkerScissors))
        CommonIcons.Add("XAMLMarkerStar", Converter.XamlToDrawingImage(My.Resources.XAMLMarkerStar))
        CommonIcons.Add("XAMLMarkerSave", Converter.XamlToDrawingImage(My.Resources.XAMLMarkerSave))
        CommonIcons.Add("XAMLBack", Converter.XamlToDrawingImage(My.Resources.XAMLBack))
        CommonIcons.Add("XAMLBackBack", Converter.XamlToDrawingImage(My.Resources.XAMLBackBack))
        CommonIcons.Add("XAMLForward", Converter.XamlToDrawingImage(My.Resources.XAMLForward))
        CommonIcons.Add("XAMLForwardForward", Converter.XamlToDrawingImage(My.Resources.XAMLForwardForward))
        CommonIcons.Add("XAMLHold", Converter.XamlToDrawingImage(My.Resources.XAMLHold))
        CommonIcons.Add("XAMLBackGray", Converter.XamlToDrawingImage(My.Resources.XAMLBackGray))
        CommonIcons.Add("XAMLBackBackGray", Converter.XamlToDrawingImage(My.Resources.XAMLBackBackGray))
        CommonIcons.Add("XAMLForwardGray", Converter.XamlToDrawingImage(My.Resources.XAMLForwardGray))
        CommonIcons.Add("XAMLForwardForwardGray", Converter.XamlToDrawingImage(My.Resources.XAMLForwardForwardGray))
        CommonIcons.Add("XAMLHoldGray", Converter.XamlToDrawingImage(My.Resources.XAMLHoldGray))
        CommonIcons.Add("XAMLReplay", Converter.XamlToDrawingImage(My.Resources.XAMLReplay))
        CommonIcons.Add("XAMLReplayGray", Converter.XamlToDrawingImage(My.Resources.XAMLReplayGray))
        CommonIcons.Add("XAMLLoadStepActive", Converter.XamlToDrawingImage(My.Resources.XAMLLoadStepActive))
        CommonIcons.Add("XAMLLoadStepInactive", Converter.XamlToDrawingImage(My.Resources.XAMLLoadStepInactive))
        CommonIcons.Add("XAMLLoadStepGray", Converter.XamlToDrawingImage(My.Resources.XAMLLoadStepGray))
        CommonIcons.Add("XAMLSitting", Converter.XamlToDrawingImage(My.Resources.XAMLSitting))
        CommonIcons.Add("XAMLStanding", Converter.XamlToDrawingImage(My.Resources.XAMLStanding))
        CommonIcons.Add("XAMLSittingGray", Converter.XamlToDrawingImage(My.Resources.XAMLSittingGray))
        CommonIcons.Add("XAMLStandingGray", Converter.XamlToDrawingImage(My.Resources.XAMLStandingGray))
        CommonIcons.Add("XAMLGrabCut", Converter.XamlToDrawingImage(My.Resources.XAMLGrabCut))
        CommonIcons.Add("XAMLGrabCutGray", Converter.XamlToDrawingImage(My.Resources.XAMLGrabCutGray))
        CommonIcons.Add("XAMLView", Converter.XamlToDrawingImage(My.Resources.XAMLView))
        CommonIcons.Add("XAMLViewGray", Converter.XamlToDrawingImage(My.Resources.XAMLViewGray))
        CommonIcons.Add("XAMLMarkForeground", Converter.XamlToDrawingImage(My.Resources.XAMLMarkForeground))
        CommonIcons.Add("XAMLMarkBackground", Converter.XamlToDrawingImage(My.Resources.XAMLMarkBackground))
        CommonIcons.Add("XAMLGroundOrange", Converter.XamlToDrawingImage(My.Resources.XAMLGroundOrange))
        CommonIcons.Add("XAMLPencil", Converter.XamlToDrawingImage(My.Resources.XAMLPencil))
        CommonIcons.Add("XAMLPencilOrange", Converter.XamlToDrawingImage(My.Resources.XAMLPencilOrange))
    End Sub
#End Region
End Class

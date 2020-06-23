Imports SegTest.Common

Class Configuration
#Region "Variables"
    ' PluginName defines the friendly name of the plugin
    ' Priority determines the order in which the buttons are arranged on the main page. The lower the number, the earlier it is placed
    Private Const PluginName As String = "Configuration"
    Public UIDispatcher As Threading.Dispatcher
#End Region
#Region "Main"
    Sub New()
        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        UIDispatcher = Threading.Dispatcher.CurrentDispatcher

        SetIcons()
        SetBindings()
    End Sub
    Private Sub Page_Loaded(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles Me.Loaded
        PageMainWindow.Title = MainWindow.ModuleName + " - " + PluginName
    End Sub
    Private Sub Page_Unloaded(ByVal sender As Object, ByVal e As RoutedEventArgs) Handles Me.Unloaded
        ' checks to perform when navigating away from this page
    End Sub
    Private Sub SetBindings()
        Dim oBinding1 As New Binding
        oBinding1.Path = New PropertyPath("DefaultSave")
        oBinding1.Mode = BindingMode.TwoWay
        oBinding1.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ConfigSaveLocation.SetBinding(HighlightTextBox.HTBTextProperty, oBinding1)

        Dim oBinding2 As New Binding
        oBinding2.Path = New PropertyPath("PowerlineFrequencyText")
        oBinding2.Mode = BindingMode.OneWay
        oBinding2.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        TextPowerFrequency.SetBinding(TextBlock.TextProperty, oBinding2)

        ConfigSaveLocation.DataContext = Settings
        TextPowerFrequency.DataContext = Settings
    End Sub
    Private Sub SetIcons()
        ButtonMain.HBSource = CommonFunctions.GetIcon("XAMLMain")
        ButtonExit.HBSource = CommonFunctions.GetIcon("XAMLExit")
        ConfigSelectSave.HBSource = CommonFunctions.GetIcon("XAMLSave")
        PowerFrequencyPrevious.HBSource = CommonFunctions.GetIcon("XAMLLeft")
        PowerFrequencyNext.HBSource = CommonFunctions.GetIcon("XAMLRight")

        If Settings.DisparityFilter Then
            DisparityFilterOn.HBSource = CommonFunctions.GetIcon("XAMLYes")
            DisparityFilterOff.HBSource = CommonFunctions.GetIcon("XAMLNoGrey")
        Else
            DisparityFilterOn.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            DisparityFilterOff.HBSource = CommonFunctions.GetIcon("XAMLNo")
        End If

        If Settings.SpatialFilter Then
            SpatialFilterOn.HBSource = CommonFunctions.GetIcon("XAMLYes")
            SpatialFilterOff.HBSource = CommonFunctions.GetIcon("XAMLNoGrey")
        Else
            SpatialFilterOn.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            SpatialFilterOff.HBSource = CommonFunctions.GetIcon("XAMLNo")
        End If

        If Settings.TemporalFilter Then
            TemporalFilterOn.HBSource = CommonFunctions.GetIcon("XAMLYes")
            TemporalFilterOff.HBSource = CommonFunctions.GetIcon("XAMLNoGrey")
        Else
            TemporalFilterOn.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            TemporalFilterOff.HBSource = CommonFunctions.GetIcon("XAMLNo")
        End If

        If Settings.HoleFillingFilter Then
            HoleFillingFilterOn.HBSource = CommonFunctions.GetIcon("XAMLYes")
            HoleFillingFilterOff.HBSource = CommonFunctions.GetIcon("XAMLNoGrey")
        Else
            HoleFillingFilterOn.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            HoleFillingFilterOff.HBSource = CommonFunctions.GetIcon("XAMLNo")
        End If

        If Settings.BacklightCompensation Then
            BacklightCompensationOn.HBSource = CommonFunctions.GetIcon("XAMLYes")
            BacklightCompensationOff.HBSource = CommonFunctions.GetIcon("XAMLNoGrey")
        Else
            BacklightCompensationOn.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            BacklightCompensationOff.HBSource = CommonFunctions.GetIcon("XAMLNo")
        End If

        If Settings.EnableAutoExposure Then
            EnableAutoExposureOn.HBSource = CommonFunctions.GetIcon("XAMLYes")
            EnableAutoExposureOff.HBSource = CommonFunctions.GetIcon("XAMLNoGrey")
        Else
            EnableAutoExposureOn.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            EnableAutoExposureOff.HBSource = CommonFunctions.GetIcon("XAMLNo")
        End If

        If Settings.EnableAutoWhiteBalance Then
            EnableAutoWhiteBalanceOn.HBSource = CommonFunctions.GetIcon("XAMLYes")
            EnableAutoWhiteBalanceOff.HBSource = CommonFunctions.GetIcon("XAMLNoGrey")
        Else
            EnableAutoWhiteBalanceOn.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            EnableAutoWhiteBalanceOff.HBSource = CommonFunctions.GetIcon("XAMLNo")
        End If

        If Settings.EmitterEnabled Then
            EmitterEnabledOn.HBSource = CommonFunctions.GetIcon("XAMLYes")
            EmitterEnabledOff.HBSource = CommonFunctions.GetIcon("XAMLNoGrey")
        Else
            EmitterEnabledOn.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            EmitterEnabledOff.HBSource = CommonFunctions.GetIcon("XAMLNo")
        End If

        If Settings.CaptureSetting Then
            CaptureSettingOn.HBSource = CommonFunctions.GetIcon("XAMLYes")
            CaptureSettingOff.HBSource = CommonFunctions.GetIcon("XAMLNoGrey")
        Else
            CaptureSettingOn.HBSource = CommonFunctions.GetIcon("XAMLYesGrey")
            CaptureSettingOff.HBSource = CommonFunctions.GetIcon("XAMLNo")
        End If
    End Sub
#End Region
#Region "UI"
    Private Sub Main_Handler(sender As Object, e As EventArgs) Handles ButtonMain.Click
        Dim oAction As Action = Sub()
                                    Me.NavigationService.Navigate(PageMainPage)
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
    Private Sub ConfigSelectSave_Handler(sender As Object, e As EventArgs) Handles ConfigSelectSave.Click
        Dim oAction As Action = Sub()
                                    Dim oFolderBrowserDialog As New Forms.FolderBrowserDialog
                                    oFolderBrowserDialog.Description = "Select Default Save Location"
                                    oFolderBrowserDialog.ShowNewFolderButton = True
                                    oFolderBrowserDialog.RootFolder = Environment.SpecialFolder.Desktop
                                    If oFolderBrowserDialog.ShowDialog = Forms.DialogResult.OK Then
                                        Settings.DefaultSave = oFolderBrowserDialog.SelectedPath
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub ConfigSelectClear_Handler(sender As Object, e As EventArgs) Handles ConfigSelectSave.RightClick
        Dim oAction As Action = Sub()
                                    If MessageBox.Show("Clear default save?", MainWindow.ModuleName, MessageBoxButton.YesNo, MessageBoxImage.Question) = MessageBoxResult.Yes Then
                                        Settings.DefaultSave = String.Empty
                                    End If
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub PowerFrequencyPrevious_Handler(sender As Object, e As EventArgs) Handles PowerFrequencyPrevious.Click
        Dim oAction As Action = Sub()
                                    Settings.PreviousPowerlineFrequency()
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub PowerFrequencyNext_Handler(sender As Object, e As EventArgs) Handles PowerFrequencyNext.Click
        Dim oAction As Action = Sub()
                                    Settings.NextPowerlineFrequency()
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub DisparityFilterOn_Handler(sender As Object, e As EventArgs) Handles DisparityFilterOn.Click
        Dim oAction As Action = Sub()
                                    Settings.DisparityFilter = True
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub DisparityFilterOff_Handler(sender As Object, e As EventArgs) Handles DisparityFilterOff.Click
        Dim oAction As Action = Sub()
                                    Settings.DisparityFilter = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub SpatialFilterOn_Handler(sender As Object, e As EventArgs) Handles SpatialFilterOn.Click
        Dim oAction As Action = Sub()
                                    Settings.SpatialFilter = True
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub SpatialFilterOff_Handler(sender As Object, e As EventArgs) Handles SpatialFilterOff.Click
        Dim oAction As Action = Sub()
                                    Settings.SpatialFilter = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub TemporalFilterOn_Handler(sender As Object, e As EventArgs) Handles TemporalFilterOn.Click
        Dim oAction As Action = Sub()
                                    Settings.TemporalFilter = True
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub TemporalFilterOff_Handler(sender As Object, e As EventArgs) Handles TemporalFilterOff.Click
        Dim oAction As Action = Sub()
                                    Settings.TemporalFilter = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub HoleFillingFilterOn_Handler(sender As Object, e As EventArgs) Handles HoleFillingFilterOn.Click
        Dim oAction As Action = Sub()
                                    Settings.HoleFillingFilter = True
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub HoleFillingFilterOff_Handler(sender As Object, e As EventArgs) Handles HoleFillingFilterOff.Click
        Dim oAction As Action = Sub()
                                    Settings.HoleFillingFilter = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub BacklightCompensationOn_Handler(sender As Object, e As EventArgs) Handles BacklightCompensationOn.Click
        Dim oAction As Action = Sub()
                                    Settings.BacklightCompensation = True
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub BacklightCompensationOff_Handler(sender As Object, e As EventArgs) Handles BacklightCompensationOff.Click
        Dim oAction As Action = Sub()
                                    Settings.BacklightCompensation = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub EnableAutoExposureOn_Handler(sender As Object, e As EventArgs) Handles EnableAutoExposureOn.Click
        Dim oAction As Action = Sub()
                                    Settings.EnableAutoExposure = True
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub EnableAutoExposureOff_Handler(sender As Object, e As EventArgs) Handles EnableAutoExposureOff.Click
        Dim oAction As Action = Sub()
                                    Settings.EnableAutoExposure = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub EnableAutoWhiteBalanceOn_Handler(sender As Object, e As EventArgs) Handles EnableAutoWhiteBalanceOn.Click
        Dim oAction As Action = Sub()
                                    Settings.EnableAutoWhiteBalance = True
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub EnableAutoWhiteBalanceOff_Handler(sender As Object, e As EventArgs) Handles EnableAutoWhiteBalanceOff.Click
        Dim oAction As Action = Sub()
                                    Settings.EnableAutoWhiteBalance = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub EmitterEnabledOn_Handler(sender As Object, e As EventArgs) Handles EmitterEnabledOn.Click
        Dim oAction As Action = Sub()
                                    Settings.EmitterEnabled = True
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub EmitterEnabledOff_Handler(sender As Object, e As EventArgs) Handles EmitterEnabledOff.Click
        Dim oAction As Action = Sub()
                                    Settings.EmitterEnabled = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub CaptureSettingOn_Handler(sender As Object, e As EventArgs) Handles CaptureSettingOn.Click
        Dim oAction As Action = Sub()
                                    Settings.CaptureSetting = True
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    Private Sub CaptureSettingOff_Handler(sender As Object, e As EventArgs) Handles CaptureSettingOff.Click
        Dim oAction As Action = Sub()
                                    Settings.CaptureSetting = False
                                    SetIcons()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
#End Region
End Class

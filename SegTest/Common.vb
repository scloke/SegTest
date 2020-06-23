Imports RS = Intel.RealSense
Imports Emgu.CV
Imports System.Collections.Concurrent
Imports System.Collections.ObjectModel
Imports System.Collections.Specialized
Imports System.ComponentModel
Imports System.Globalization
Imports System.Runtime.InteropServices
Imports System.Runtime.Serialization

Public Class Common
#Region "ProgramConstantsAndVariables"
    Public Const TaskCount As Integer = 8
    Public Const Resolution096 As Single = 96
    Public Const Resolution150 As Single = 150
    Public Const Resolution300 As Single = 300
    Public Const Resolution600 As Single = 600

    Public Shared PageMainWindow As MainWindow
    Public Shared PageMainPage As MainPage
    Public Shared PageConfiguration As Configuration
    Public Shared ExcludedSegTypes As List(Of ClipData.SegTypeEnum) = {ClipData.SegTypeEnum.None, ClipData.SegTypeEnum.GMG, ClipData.SegTypeEnum.MixtureOfGaussianV1}.ToList

    Public Shared Settings As SettingsClass
    Public Shared CommonIcons As Dictionary(Of String, ImageSource)
    Public Shared ParallelDictionary As New Dictionary(Of Integer, Tuple(Of List(Of Integer), List(Of Integer)))
    Public Shared ParallelcacheLock As New Threading.ReaderWriterLockSlim
#End Region
#Region "Classes"
    <DataContract()> Public Class SettingsClass
        ' Persistent application variables
        Implements IDisposable, INotifyPropertyChanged

        <DataMember> Private m_DefaultSave As String
        <DataMember> Private m_PowerlineFrequency As PowerlineFrequencyEnum
        <DataMember> Private m_DisparityFilter As Boolean
        <DataMember> Private m_SpatialFilter As Boolean
        <DataMember> Private m_TemporalFilter As Boolean
        <DataMember> Private m_HoleFillingFilter As Boolean
        <DataMember> Private m_BacklightCompensation As Boolean
        <DataMember> Private m_EnableAutoExposure As Boolean
        <DataMember> Private m_EnableAutoWhiteBalance As Boolean
        <DataMember> Private m_EmitterEnabled As Boolean
        <DataMember> Private m_CaptureSetting As Boolean

        Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged
        Protected Sub OnPropertyChangedLocal(ByVal sName As String)
            RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(sName))
        End Sub
        Public Sub New()
            m_DefaultSave = Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory)
            m_PowerlineFrequency = PowerlineFrequencyEnum.FQ50Hz
            m_DisparityFilter = True
            m_SpatialFilter = True
            m_TemporalFilter = True
            m_HoleFillingFilter = True
            m_BacklightCompensation = False
            m_EnableAutoExposure = False
            m_EnableAutoWhiteBalance = False
            m_EmitterEnabled = True
            m_CaptureSetting = False
        End Sub
        Public Property DefaultSave As String
            Get
                Return m_DefaultSave
            End Get
            Set(value As String)
                m_DefaultSave = value
                OnPropertyChangedLocal("DefaultSave")
            End Set
        End Property
        Public Property PowerlineFrequency As PowerlineFrequencyEnum
            Get
                Return m_PowerlineFrequency
            End Get
            Set(value As PowerlineFrequencyEnum)
                m_PowerlineFrequency = value
                OnPropertyChangedLocal("PowerlineFrequency")
                OnPropertyChangedLocal("PowerlineFrequencyText")
            End Set
        End Property
        Public ReadOnly Property PowerlineFrequencyText As String
            Get
                Select Case m_PowerlineFrequency
                    Case PowerlineFrequencyEnum.FQ50Hz
                        Return "50 Hz"
                    Case PowerlineFrequencyEnum.FQ60Hz
                        Return "60 Hz"
                    Case PowerlineFrequencyEnum.Auto
                        Return "Auto Compensation"
                    Case Else
                        Return "No Compensation"
                End Select
            End Get
        End Property
        Public Sub PreviousPowerlineFrequency()
            Dim oNames As String() = [Enum].GetNames(GetType(PowerlineFrequencyEnum))
            Dim iIndex As Integer = Array.IndexOf(oNames, [Enum].GetName(GetType(PowerlineFrequencyEnum), m_PowerlineFrequency))
            Dim iNewIndex As Integer = (iIndex + oNames.Count - 1) Mod oNames.Count
            m_PowerlineFrequency = [Enum].GetValues(GetType(PowerlineFrequencyEnum))(iNewIndex)
            OnPropertyChangedLocal("PowerlineFrequency")
            OnPropertyChangedLocal("PowerlineFrequencyText")
        End Sub
        Public Sub NextPowerlineFrequency()
            Dim oNames As String() = [Enum].GetNames(GetType(PowerlineFrequencyEnum))
            Dim iIndex As Integer = Array.IndexOf(oNames, [Enum].GetName(GetType(PowerlineFrequencyEnum), m_PowerlineFrequency))
            Dim iNewIndex As Integer = (iIndex + oNames.Count + 1) Mod oNames.Count
            m_PowerlineFrequency = [Enum].GetValues(GetType(PowerlineFrequencyEnum))(iNewIndex)
            OnPropertyChangedLocal("PowerlineFrequency")
            OnPropertyChangedLocal("PowerlineFrequencyText")
        End Sub
        <DataContract()> Public Enum PowerlineFrequencyEnum As Integer
            <EnumMember(Value:="0")> NotSet
            <EnumMember(Value:="1")> FQ50Hz
            <EnumMember(Value:="2")> FQ60Hz
            <EnumMember(Value:="3")> Auto
        End Enum
        Public Property DisparityFilter As Boolean
            Get
                Return m_DisparityFilter
            End Get
            Set(value As Boolean)
                m_DisparityFilter = value
                OnPropertyChangedLocal("DisparityFilter")
            End Set
        End Property
        Public Property SpatialFilter As Boolean
            Get
                Return m_SpatialFilter
            End Get
            Set(value As Boolean)
                m_SpatialFilter = value
                OnPropertyChangedLocal("SpatialFilter")
            End Set
        End Property
        Public Property TemporalFilter As Boolean
            Get
                Return m_TemporalFilter
            End Get
            Set(value As Boolean)
                m_TemporalFilter = value
                OnPropertyChangedLocal("TemporalFilter")
            End Set
        End Property
        Public Property HoleFillingFilter As Boolean
            Get
                Return m_HoleFillingFilter
            End Get
            Set(value As Boolean)
                m_HoleFillingFilter = value
                OnPropertyChangedLocal("HoleFillingFilter")
            End Set
        End Property
        Public Property BacklightCompensation As Boolean
            Get
                Return m_BacklightCompensation
            End Get
            Set(value As Boolean)
                m_BacklightCompensation = value
                OnPropertyChangedLocal("BacklightCompensation")
            End Set
        End Property
        Public Property EnableAutoExposure As Boolean
            Get
                Return m_EnableAutoExposure
            End Get
            Set(value As Boolean)
                m_EnableAutoExposure = value
                OnPropertyChangedLocal("EnableAutoExposure")
            End Set
        End Property
        Public Property EnableAutoWhiteBalance As Boolean
            Get
                Return m_EnableAutoWhiteBalance
            End Get
            Set(value As Boolean)
                m_EnableAutoWhiteBalance = value
                OnPropertyChangedLocal("EnableAutoWhiteBalance")
            End Set
        End Property
        Public Property EmitterEnabled As Boolean
            Get
                Return m_EmitterEnabled
            End Get
            Set(value As Boolean)
                m_EmitterEnabled = value
                OnPropertyChangedLocal("EmitterEnabled")
            End Set
        End Property
        Public Property CaptureSetting As Boolean
            Get
                Return m_CaptureSetting
            End Get
            Set(value As Boolean)
                m_CaptureSetting = value
                OnPropertyChangedLocal("CaptureSetting")
            End Set
        End Property
        Public Shared Function GetKnownTypes() As List(Of Type)
            ' returns the list of additonal types
            Return New List(Of Type) From {}
        End Function
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
                End If
            End SyncLock
        End Sub
#End Region
    End Class
    Public Class NTPTime
        Public Shared Offset As TimeSpan = TimeSpan.Zero

        Public Shared Sub SetOffset()
            Try
                Using oNtp As New GuerrillaNtp.NtpClient(Net.Dns.GetHostAddresses("pool.ntp.org").First)
                    Offset = oNtp.GetCorrectionOffset()
                End Using
            Catch ex As Exception
            End Try
        End Sub
        Public Shared Function GetDateTimeNow() As DateTime
            Return DateTime.UtcNow + Offset
        End Function
        Public Shared Function GetDateTimeLocalNow() As DateTime
            Return DateTime.Now + Offset
        End Function
    End Class
    Public Class RealSenseCapture
        Implements IDisposable, INotifyPropertyChanged

        Public Property Capturing As CaptureStateEnum
        Private WithEvents m_Context As RS.Context = Nothing
        Private m_Align As RS.Align
        Private m_Decimate As RS.DecimationFilter
        Private m_DisparityTransform As RS.DisparityTransform
        Private m_DepthTransform As RS.DisparityTransform
        Private m_Spatial As RS.SpatialFilter
        Private m_Temporal As RS.TemporalFilter
        Private m_HoleFilling As RS.HoleFillingFilter
        Private m_Devices As Dictionary(Of String, List(Of Device))
        Private m_Block As RS.CustomProcessingBlock
        Private m_FrameDictionary As Dictionary(Of Integer, Tuple(Of Single, Date, String))
        Private m_OptimalDepthDictionary As Dictionary(Of String, Tuple(Of Integer, Integer))
        Private m_CommonDevices As List(Of CommonDevice)
        Private m_SelectedDevice As Integer = -1
        Private m_SelectedProfile As Integer = -1
        Private m_SelectedResolution As Integer = -1
        Private m_DeviceBlacklist As List(Of String)
        Private m_ColourVideoStreamProfile As RS.VideoStreamProfile
        Private m_DepthVideoStreamProfile As RS.VideoStreamProfile
        Private m_DepthToColourExtrinsics As RS.Extrinsics
        Private m_Duration As Integer
        Private m_Alarms As List(Of Integer)
        Private m_CaptureStart As Date
        Private Const TimestampRemovalTime As Integer = 5
        Private Const MinFrameRate As Integer = 30
        Private Const DecimateMagnitude As Integer = 2
        Public Event FrameAvailable(ByVal oFrame As Frame)
        Public Event SoftwareFrameAvailable(ByVal oFrame As Frame)
        Public Event DeviceReset()
        Public Event SendMessage(ByVal oMessage As Message)
        Public Event SelectedChanged()
        Public Event RingAlarm()

#Region "INotifyPropertyChanged"
        Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged
        Private Sub OnPropertyChangedLocal(ByVal sName As String)
            RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(sName))
        End Sub
#End Region
        Sub New(ByRef oDevicesElement As FrameworkElement, ByRef oProfilesElement As FrameworkElement, ByRef oResolutionsElement As FrameworkElement)
            Capturing = CaptureStateEnum.Idle
            CaptureLoopActive = New Processing.BooleanObject(False)

            m_Align = New RS.Align(RS.Stream.Color)
            m_Decimate = New RS.DecimationFilter()
            For Each oOption In m_Decimate.Options
                If oOption.Key = RS.Option.FilterMagnitude Then
                    oOption.Value = DecimateMagnitude
                End If
            Next

            m_DisparityTransform = New RS.DisparityTransform(True)
            m_DepthTransform = New RS.DisparityTransform(False)
            m_Spatial = New RS.SpatialFilter()
            m_Temporal = New RS.TemporalFilter()
            m_HoleFilling = New RS.HoleFillingFilter
            m_Block = New RS.CustomProcessingBlock(New RS.CustomProcessingBlock.FrameProcessorCallback(AddressOf FrameProcessorCallback))
            m_Block.Start(New RS.CustomProcessingBlock.FrameCallback(AddressOf FrameCallback))
            m_FrameDictionary = New Dictionary(Of Integer, Tuple(Of Single, Date, String))
            m_OptimalDepthDictionary = New Dictionary(Of String, Tuple(Of Integer, Integer))
            m_OptimalDepthDictionary.Add("Intel RealSense D415", New Tuple(Of Integer, Integer)(1280, 720))
            m_OptimalDepthDictionary.Add("Intel RealSense D435", New Tuple(Of Integer, Integer)(848, 480))
            m_DeviceBlacklist = New List(Of String)
            m_DeviceBlacklist.Add("Platform Camera")
            m_ColourVideoStreamProfile = Nothing
            m_DepthVideoStreamProfile = Nothing
            m_DepthToColourExtrinsics = Nothing
            m_Duration = 0
            m_Alarms = New List(Of Integer)
            m_CaptureStart = Date.MinValue

            ConfigureDevices()

            m_CommonDevices = New List(Of CommonDevice)
            DevicesDisplay = New TrueObservableCollection(Of HCBDisplay)
            ProfilesDisplay = New TrueObservableCollection(Of HCBDisplay)
            ResolutionsDisplay = New TrueObservableCollection(Of HCBDisplay)

            DevicesDisplay.SetBoundElement(oDevicesElement)
            ProfilesDisplay.SetBoundElement(oProfilesElement)
            ResolutionsDisplay.SetBoundElement(oResolutionsElement)

            Using oSuspendDevices As New TrueObservableCollection(Of HCBDisplay).SuspendUpdates(DevicesDisplay)
                Using oSuspendProfiles As New TrueObservableCollection(Of HCBDisplay).SuspendUpdates(ProfilesDisplay)
                    Using oSuspendResolutions As New TrueObservableCollection(Of HCBDisplay).SuspendUpdates(ResolutionsDisplay)
                        SetDevices()
                        ResetDevice()
                        ResetProfile()
                        ResetResolution()
                    End Using
                End Using
            End Using

            OnPropertyChangedLocal("SelectedDevice")
            OnPropertyChangedLocal("SelectedProfile")
            OnPropertyChangedLocal("SelectedResolution")
        End Sub
        Private ReadOnly Property Name
            Get
                Return "RealSense Camera"
            End Get
        End Property
        Private Property CaptureLoopActive As Processing.BooleanObject
        Public Sub DeviceChangedHandler(ByVal removed As RS.DeviceList, ByVal added As RS.DeviceList) Handles m_Context.OnDevicesChanged
            ' realsense camera changed
            OnDeviceReset()
        End Sub
        Public ReadOnly Property DevicesDisplay As TrueObservableCollection(Of HCBDisplay)
        Public ReadOnly Property ProfilesDisplay As TrueObservableCollection(Of HCBDisplay)
        Public ReadOnly Property ResolutionsDisplay As TrueObservableCollection(Of HCBDisplay)
        Public ReadOnly Property CaptureReady As Boolean
            Get
                Return m_SelectedDevice >= 0 AndAlso m_SelectedProfile >= 0 AndAlso m_SelectedResolution >= 0
            End Get
        End Property
        Public Property SelectedDevice As HCBDisplay
            Get
                If m_SelectedDevice < 0 Or m_SelectedDevice >= m_CommonDevices.Count Then
                    Return New HCBDisplay
                Else
                    Return m_CommonDevices(m_SelectedDevice).DeviceDisplay
                End If
            End Get
            Set(value As HCBDisplay)
                Dim oOldSelectedDevice As HCBDisplay = SelectedDevice
                Dim oDevicesDisplay As List(Of HCBDisplay) = DevicesDisplay.ToList
                If DevicesDisplay.Contains(value) Then
                    m_SelectedDevice = oDevicesDisplay.IndexOf(value)
                Else
                    m_SelectedDevice = -1
                End If

                ' selected display changed
                If Not SelectedDevice.Equals(oOldSelectedDevice) Then
                    ' reset selected streams
                    SelectedDeviceChanged()
                    OnCaptureReset()
                    RaiseEvent SelectedChanged()
                End If
            End Set
        End Property
        Public Property SelectedProfile As HCBDisplay
            Get
                If (m_SelectedDevice < 0 Or m_SelectedDevice >= m_CommonDevices.Count) OrElse (m_SelectedProfile < 0 Or m_SelectedProfile >= m_CommonDevices(m_SelectedDevice).ColourProfiles.Keys.Count) Then
                    Return New HCBDisplay
                Else
                    Return m_CommonDevices(m_SelectedDevice).ColourProfiles.Values.Cast(Of List(Of CommonProfile)).ToList(m_SelectedProfile).First.ProfileDisplay
                End If
            End Get
            Set(value As HCBDisplay)
                Dim oOldSelectedProfile As HCBDisplay = SelectedProfile
                Dim oProfilesDisplay As List(Of HCBDisplay) = ProfilesDisplay.ToList
                If oProfilesDisplay.Contains(value) Then
                    m_SelectedProfile = oProfilesDisplay.IndexOf(value)
                Else
                    m_SelectedProfile = -1
                End If

                ' selected display changed
                If Not SelectedProfile.Equals(oOldSelectedProfile) Then
                    ' reset selected streams
                    SelectedProfileChanged()
                    OnCaptureReset()
                    RaiseEvent SelectedChanged()
                End If
            End Set
        End Property
        Public Property SelectedResolution As HCBDisplay
            Get
                If (m_SelectedDevice < 0 Or m_SelectedDevice >= m_CommonDevices.Count) OrElse (m_SelectedProfile < 0 Or m_SelectedProfile >= m_CommonDevices(m_SelectedDevice).ColourProfiles.Keys.Count) OrElse (m_SelectedResolution < 0 Or m_SelectedResolution >= m_CommonDevices(m_SelectedDevice).ColourProfiles.Values.Count) Then
                    Return New HCBDisplay
                Else
                    Return m_CommonDevices(m_SelectedDevice).ColourProfiles.Values.Cast(Of List(Of CommonProfile)).ToList(m_SelectedProfile).ToList(m_SelectedResolution).ResolutionDisplay
                End If
            End Get
            Set(value As HCBDisplay)
                Dim oOldSelectedResolution As HCBDisplay = SelectedResolution
                Dim oResolutionsDisplay As List(Of HCBDisplay) = ResolutionsDisplay.ToList
                If oResolutionsDisplay.Contains(value) Then
                    m_SelectedResolution = oResolutionsDisplay.IndexOf(value)
                Else
                    m_SelectedResolution = -1
                End If

                ' selected display changed
                If Not SelectedResolution.Equals(oOldSelectedResolution) Then
                    ' reset selected streams
                    OnPropertyChangedLocal("SelectedResolution")
                    OnCaptureReset()
                    RaiseEvent SelectedChanged()
                End If
            End Set
        End Property
        Private Sub SelectedDeviceChanged()
            ResetProfile()
            ResetResolution()
            OnPropertyChangedLocal("SelectedDevice")
            OnPropertyChangedLocal("SelectedProfile")
            OnPropertyChangedLocal("SelectedResolution")
        End Sub
        Private Sub SelectedProfileChanged()
            ResetResolution()
            OnPropertyChangedLocal("SelectedProfile")
            OnPropertyChangedLocal("SelectedResolution")
        End Sub
        Public Sub SetDevices()
            ' sets the common devices list
            ' checks connected devices and refreshes device list
            Dim oRSDeviceList As New List(Of Tuple(Of String, RS.Device))
            If IsNothing(m_Context) Then
                m_Context = New RS.Context
            End If

            Dim iDeviceCount As Integer = m_Context.Devices.Count
            For i = 0 To iDeviceCount - 1
                Try
                    Dim oDevice As RS.Device = m_Context.Devices()(i)
                    Dim sDeviceName As String = oDevice.Info(RS.CameraInfo.Name)

                    ' skip platform devices (webcams)
                    If Not m_DeviceBlacklist.Contains(sDeviceName) Then
                        oRSDeviceList.Add(New Tuple(Of String, RS.Device)(sDeviceName, oDevice))
                    End If
                Catch ex As Exception
                    RaiseEvent SendMessage(New Message(Name, Colors.Red, NTPTime.GetDateTimeNow, ex.Message))
                End Try
            Next

            ' clear device list
            If IsNothing(m_Devices) Then
                m_Devices = New Dictionary(Of String, List(Of Device))
            End If
            m_Devices.Clear()

            ' sort
            oRSDeviceList = (From oRSDevice In oRSDeviceList Order By oRSDevice.Item1 Descending).ToList
            For i = 0 To oRSDeviceList.Count - 1
                Dim sName As String = oRSDeviceList(i).Item1
                If Not m_Devices.ContainsKey(sName) Then
                    m_Devices.Add(sName, New List(Of Device))
                End If
                m_Devices(sName).Add(New Device(oRSDeviceList(i).Item2))
            Next

            ' initialise common devices
            m_CommonDevices.Clear()
            For Each oDevicePair In m_Devices
                Dim oDevices As Dictionary(Of Guid, String) = (From oDevice In oDevicePair.Value Select New KeyValuePair(Of Guid, String)(oDevice.GUID, String.Empty)).ToDictionary(Function(x) x.Key, Function(x) x.Value)
                Dim oFirstDevice As Device = oDevicePair.Value.First
                Dim oProfiles As New List(Of CommonProfile)
                For Each oProfile In oFirstDevice.Profiles
                    Dim oStreamType As StreamTypeEnum = FormatToStreamType(oProfile.Format)

                    ' only add valid profiles
                    If oStreamType <> StreamTypeEnum.None Then
                        Dim sFormatName As String = [Enum].GetName(GetType(RS.Format), oProfile.Format)
                        oProfiles.Add(New CommonProfile(oFirstDevice.Name, sFormatName, oProfile.Width, oProfile.Height, oStreamType, Nothing))
                    End If
                Next

                Dim oCommonDevice As New CommonDevice(oDevices, oFirstDevice.Name, oProfiles)
                m_CommonDevices.Add(oCommonDevice)
            Next
        End Sub
        Private Sub OnDeviceReset()
            ' a device has changed, so stop all capturing and reset all devices
            If Capturing = CaptureStateEnum.Capturing Then
                CaptureStop()
            End If

            Using oSuspendDevices As New TrueObservableCollection(Of HCBDisplay).SuspendUpdates(DevicesDisplay)
                Using oSuspendProfiles As New TrueObservableCollection(Of HCBDisplay).SuspendUpdates(ProfilesDisplay)
                    Using oSuspendResolutions As New TrueObservableCollection(Of HCBDisplay).SuspendUpdates(ResolutionsDisplay)
                        SetDevices()
                        ResetDevice()
                        ResetProfile()
                        ResetResolution()
                    End Using
                End Using
            End Using

            OnPropertyChangedLocal("SelectedDevice")
            OnPropertyChangedLocal("SelectedProfile")
            OnPropertyChangedLocal("SelectedResolution")

            RaiseEvent DeviceReset()
        End Sub
        Private Sub ResetDevice()
            ' resets the device
            If m_CommonDevices.Count = 0 Then
                m_SelectedDevice = -1
            Else
                m_SelectedDevice = 0
            End If

            DevicesDisplay.Clear()
            Dim oDevicesDisplay As List(Of HCBDisplay) = m_CommonDevices.Select(Function(x) x.DeviceDisplay).ToList
            DevicesDisplay.AddRange(oDevicesDisplay)
        End Sub
        Private Sub ResetProfile()
            ' resets the profile name
            If (m_SelectedDevice < 0 Or m_SelectedDevice >= m_CommonDevices.Count) OrElse m_CommonDevices(m_SelectedDevice).ColourProfiles.Keys.Count = 0 Then
                m_SelectedProfile = -1
            Else
                m_SelectedProfile = 0
            End If

            ProfilesDisplay.Clear()
            If m_SelectedDevice <> -1 Then
                Dim oProfilesDisplay As List(Of HCBDisplay) = m_CommonDevices(m_SelectedDevice).ColourProfiles.Values.Cast(Of List(Of CommonProfile)).ToList.Select(Function(x) x.First.ProfileDisplay).ToList
                ProfilesDisplay.AddRange(oProfilesDisplay)
            End If
        End Sub
        Private Sub ResetResolution()
            ' resets the profile resolution
            If (m_SelectedDevice < 0 Or m_SelectedDevice >= m_CommonDevices.Count) OrElse (m_SelectedProfile < 0 Or m_SelectedProfile >= m_CommonDevices(m_SelectedDevice).ColourProfiles.Keys.Count) OrElse m_CommonDevices(m_SelectedDevice).ColourProfiles.Values.Count = 0 Then
                m_SelectedResolution = -1
            Else
                m_SelectedResolution = 0
            End If

            ResolutionsDisplay.Clear()
            If m_SelectedDevice <> -1 AndAlso m_SelectedProfile <> -1 Then
                Dim sProfileName As String = m_CommonDevices(m_SelectedDevice).ColourProfiles.Keys.Cast(Of String).ToList(m_SelectedProfile)
                Dim oResolutionsDisplay As List(Of HCBDisplay) = m_CommonDevices(m_SelectedDevice).ColourProfiles(sProfileName).ToList.Select(Function(x) x.ResolutionDisplay).ToList
                ResolutionsDisplay.AddRange(oResolutionsDisplay)
            End If
        End Sub
        Public Sub OnCaptureReset()
            ' reset capture only if active
            If Capturing = CaptureStateEnum.Capturing Then
                CaptureStop()
                CaptureStart(-1, New List(Of Integer))
            End If
        End Sub
        Public Sub CaptureStart(ByVal iDuration As Integer, ByVal oAlarms As List(Of Integer))
            ' starts capture
            If m_SelectedDevice >= 0 AndAlso m_SelectedProfile >= 0 AndAlso m_SelectedResolution >= 0 Then
                Dim oSelectedDevice As CommonDevice = m_CommonDevices(m_SelectedDevice)
                Dim oSelectedProfile As CommonProfile = oSelectedDevice.ColourProfiles.Values(m_SelectedProfile).ToList(m_SelectedResolution)

                ' wait until transition to idle state
                Do Until Capturing = CaptureStateEnum.Idle
                    Threading.Thread.Sleep(100)
                Loop

                m_Duration = iDuration
                m_Alarms = oAlarms
                m_CaptureStart = NTPTime.GetDateTimeNow

                Capturing = CaptureStateEnum.Capturing
                Task.Run(Sub() CaptureLoop(oSelectedDevice.Name, oSelectedProfile))
            End If
        End Sub
        Public Sub CaptureStop()
            ' stops capture
            If Capturing = CaptureStateEnum.Capturing Then
                Threading.Interlocked.Exchange(Capturing, CaptureStateEnum.Stopping)
            End If

            ' wait until transition to idle state
            Do Until Capturing = CaptureStateEnum.Idle
                Threading.Thread.Sleep(100)
            Loop
        End Sub
        Public Sub ConvertDepth(ByVal sFolderName As String, ByVal oPlayDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, StreamTypeEnum, RS.TimestampDomain, Double, Single))))
            ' starts the depth conversion process
            Dim oColourDepthProfile As Frame.StreamProfiles = Nothing
            If IO.File.Exists(sFolderName + "\ColourDepthProfile.dat") Then
                oColourDepthProfile = CommonFunctions.DeserializeDataContractFile(Of Frame.StreamProfiles)(sFolderName + "\ColourDepthProfile.dat")
            End If

            If (Not IsNothing(oColourDepthProfile.ColourVideoStreamProfile)) AndAlso (Not IsNothing(oColourDepthProfile.DepthVideoStreamProfile)) Then
                Using oPipeline As New RS.Pipeline
                    Using oConfig As New RS.Config
                        oConfig.EnableStream(RS.Stream.Color, oColourDepthProfile.ColourVideoStreamProfile.width, oColourDepthProfile.ColourVideoStreamProfile.height, CType(oColourDepthProfile.ColourVideoStreamProfile.format, RS.Format), oColourDepthProfile.ColourVideoStreamProfile.fps)
                        oConfig.EnableStream(RS.Stream.Depth, oColourDepthProfile.DepthVideoStreamProfile.width, oColourDepthProfile.DepthVideoStreamProfile.height, CType(oColourDepthProfile.DepthVideoStreamProfile.format, RS.Format), oColourDepthProfile.DepthVideoStreamProfile.fps)
                        Using oPipelineProfile As RS.PipelineProfile = oPipeline.Start(oConfig)
                            Using oSoftwareDevice As New RS.SoftwareDevice
                                Using oColourSensor As RS.SoftwareSensor = oSoftwareDevice.AddSensor("Color")
                                    Using oDepthSensor As RS.SoftwareSensor = oSoftwareDevice.AddSensor("Depth")
                                        m_ColourVideoStreamProfile = oColourSensor.AddVideoStream(oColourDepthProfile.ColourVideoStreamProfile.GetVideoStream())
                                        m_DepthVideoStreamProfile = oDepthSensor.AddVideoStream(oColourDepthProfile.DepthVideoStreamProfile.GetVideoStream())
                                        m_DepthToColourExtrinsics = If(IsNothing(oColourDepthProfile.DepthToColour), Nothing, oColourDepthProfile.DepthToColour.GetExtrinsics)

                                        oColourSensor.Open(m_ColourVideoStreamProfile)
                                        oDepthSensor.Open(m_DepthVideoStreamProfile)

                                        oColourSensor.Start(AddressOf m_Block.ProcessFrame)
                                        oDepthSensor.Start(AddressOf m_Block.ProcessFrame)

                                        Dim oStartTime As Date = NTPTime.GetDateTimeNow
                                        OnSoftwareFrameAvailable(Frame.StartTransmission(m_ColourVideoStreamProfile, m_DepthVideoStreamProfile, m_DepthToColourExtrinsics, oStartTime))

                                        Dim oFrameList As List(Of Integer) = oPlayDictionary.Keys.OrderBy(Function(x) x).ToList
                                        For Each iFrame In oFrameList
                                            Dim fDepthScale As Single = 0
                                            For Each oFrame In oPlayDictionary(iFrame)
                                                If oFrame.Item3 = StreamTypeEnum.Colour Then
                                                    Dim oBytes As Byte() = IO.File.ReadAllBytes(oFrame.Item2)
                                                    Using oMatrix As Matrix(Of Byte) = MatrixData(Of Byte).GetMatrixData(oBytes).GetMatrix
                                                        m_FrameDictionary.Add(oFrame.Item1, New Tuple(Of Single, Date, String)(0, oStartTime.AddMilliseconds(iFrame), oFrame.Item2))
                                                        oColourSensor.AddVideoFrame(oMatrix.Bytes, oMatrix.Mat.Step, oColourDepthProfile.ColourVideoStreamProfile.bpp / 8, oFrame.Item5, oFrame.Item4, oFrame.Item1, m_ColourVideoStreamProfile)
                                                    End Using
                                                ElseIf oFrame.Item3 = StreamTypeEnum.Depth Then
                                                    Dim oBytes As Byte() = IO.File.ReadAllBytes(oFrame.Item2)
                                                    fDepthScale = oFrame.Item6
                                                    Using oMatrix As Matrix(Of UShort) = MatrixData(Of UShort).GetMatrixData(oBytes).GetMatrix
                                                        m_FrameDictionary.Add(oFrame.Item1, New Tuple(Of Single, Date, String)(fDepthScale, oStartTime.AddMilliseconds(iFrame), oFrame.Item2))
                                                        oDepthSensor.AddVideoFrame(oMatrix.Bytes, oMatrix.Mat.Step, oColourDepthProfile.DepthVideoStreamProfile.bpp / 8, oFrame.Item5, oFrame.Item4, oFrame.Item1, m_DepthVideoStreamProfile)
                                                    End Using
                                                End If
                                            Next
                                        Next

                                        OnSoftwareFrameAvailable(Frame.StopTransmission)
                                    End Using
                                End Using
                            End Using
                        End Using
                    End Using
                End Using
            End If
        End Sub
        Private Sub ConfigureDevices()
            ' called to pre-configure devices before startup
            Using oContext As New RS.Context
                Dim iDeviceCount As Integer = oContext.Devices.Count
                For i = 0 To iDeviceCount - 1
                    Dim oDevice As RS.Device = oContext.Devices()(i)
                    Dim iSensorCount As Integer = oDevice.Sensors.Count

                    For j = 0 To iSensorCount - 1
                        ' sets device options
                        Dim oSensor As RS.Sensor = oDevice.Sensors()(j)
                        Dim oOption_DepthUnits = oSensor.Options()(RS.Option.DepthUnits)
                        Dim bDepth As Boolean = oOption_DepthUnits.Supported

                        Dim oOption_BacklightCompensation = oSensor.Options()(RS.Option.BacklightCompensation)
                        Dim oOption_EnableAutoExposure = oSensor.Options()(RS.Option.EnableAutoExposure)
                        Dim oOption_Exposure = oSensor.Options()(RS.Option.Exposure)
                        Dim oOption_EnableAutoWhiteBalance = oSensor.Options()(RS.Option.EnableAutoWhiteBalance)
                        Dim oOption_EmitterEnabled = oSensor.Options()(RS.Option.EmitterEnabled)
                        Dim oOption_PowerLineFrequency = oSensor.Options()(RS.Option.PowerLineFrequency)
                        Dim oOption_Brightness = oSensor.Options()(RS.Option.Brightness)
                        Dim oOption_Contrast = oSensor.Options()(RS.Option.Contrast)
                        Dim oOption_Gain = oSensor.Options()(RS.Option.Gain)
                        Dim oOption_Gamma = oSensor.Options()(RS.Option.Gamma)

                        If oOption_BacklightCompensation.Supported Then
                            If Settings.BacklightCompensation Then
                                oOption_BacklightCompensation.Value = If(Settings.CaptureSetting, 0, 1)
                            Else
                                oOption_BacklightCompensation.Value = 0
                            End If
                        End If

                        If oOption_EnableAutoExposure.Supported Then
                            If bDepth Then
                                ' depth camera always set autoexposure on
                                oOption_EnableAutoExposure.Value = 1
                            ElseIf Settings.EnableAutoExposure Then
                                oOption_EnableAutoExposure.Value = If(Settings.CaptureSetting, 0, 1)
                            Else
                                oOption_EnableAutoExposure.Value = 0
                            End If

                            ' only fix exposure if autoexposure not set
                            If oOption_EnableAutoExposure.Value = 0 Then
                                If oOption_Exposure.Supported Then
                                    oOption_Exposure.Value = Math.Min(oOption_Exposure.Max, Math.Max(oOption_Exposure.Min, 100))
                                End If
                                If oOption_Brightness.Supported Then
                                    oOption_Brightness.Value = ((oOption_Brightness.Max - oOption_Brightness.Min) * 3 / 4) + oOption_Brightness.Min
                                End If
                                If oOption_Contrast.Supported Then
                                    oOption_Contrast.Value = ((oOption_Contrast.Max - oOption_Contrast.Min) * 3 / 4) + oOption_Contrast.Min
                                End If
                                If oOption_Gain.Supported Then
                                    oOption_Gain.Value = ((oOption_Gain.Max - oOption_Gain.Min) * 3 / 4) + oOption_Gain.Min
                                End If
                                If oOption_Gamma.Supported Then
                                    oOption_Gamma.Value = ((oOption_Gamma.Max - oOption_Gamma.Min) * 3 / 4) + oOption_Gamma.Min
                                End If
                            Else
                                If oOption_Brightness.Supported Then
                                    oOption_Brightness.Value = oOption_Brightness.Default
                                End If
                                If oOption_Contrast.Supported Then
                                    oOption_Contrast.Value = oOption_Contrast.Default
                                End If
                                If oOption_Gain.Supported Then
                                    oOption_Gain.Value = oOption_Gain.Default
                                End If
                                If oOption_Gamma.Supported Then
                                    oOption_Gamma.Value = oOption_Gamma.Default
                                End If
                            End If
                        End If

                        If oOption_EnableAutoWhiteBalance.Supported Then
                            If Settings.EnableAutoWhiteBalance Then
                                oOption_EnableAutoWhiteBalance.Value = If(Settings.CaptureSetting, 0, 1)
                            Else
                                oOption_EnableAutoWhiteBalance.Value = 0
                            End If
                        End If

                        If oOption_EmitterEnabled.Supported Then
                            If Settings.EmitterEnabled Then
                                oOption_EmitterEnabled.Value = 1
                            Else
                                oOption_EmitterEnabled.Value = If(Settings.CaptureSetting, 1, 0)
                            End If
                        End If

                        If oOption_PowerLineFrequency.Supported Then
                            oOption_PowerLineFrequency.Value = Settings.PowerlineFrequency
                        End If
                    Next
                Next
            End Using
        End Sub
        Private Sub FrameProcessorCallback(frame As RS.Frame, source As RS.FrameSource)
            ' custom frame processor
            Using oReleaser As New RS.FramesReleaser
                Dim oFrames = source.AllocateCompositeFrame(oReleaser, frame)
                Dim oColourFrame As RS.VideoFrame = Nothing
                Dim oDepthFrame As RS.DepthFrame = Nothing
                If Not IsNothing(oFrames.ColorFrame) Then
                    oColourFrame = RS.FramesReleaser.ScopedReturn(oReleaser, oFrames.ColorFrame)
                End If
                If Not IsNothing(oFrames.DepthFrame) Then
                    oDepthFrame = RS.FramesReleaser.ScopedReturn(oReleaser, oFrames.DepthFrame)
                End If

                ' special processing for depth frame
                If Not IsNothing(oDepthFrame) Then
                    oDepthFrame = m_Decimate.ApplyFilter(oDepthFrame, oReleaser)
                    If Settings.DisparityFilter Then
                        oDepthFrame = m_DisparityTransform.ApplyFilter(oDepthFrame, oReleaser)
                    End If
                    If Settings.SpatialFilter Then
                        oDepthFrame = m_Spatial.ApplyFilter(oDepthFrame, oReleaser)
                    End If
                    If Settings.TemporalFilter Then
                        oDepthFrame = m_Temporal.ApplyFilter(oDepthFrame, oReleaser)
                    End If
                    If Settings.DisparityFilter Then
                        oDepthFrame = m_DepthTransform.ApplyFilter(oDepthFrame, oReleaser)
                    End If
                    If Settings.HoleFillingFilter Then
                        oDepthFrame = m_HoleFilling.ApplyFilter(oDepthFrame)
                    End If
                End If

                ' create new frameset
                Dim oFrameSet As RS.FrameSet = Nothing
                If (Not IsNothing(oColourFrame)) AndAlso (Not IsNothing(oDepthFrame)) Then
                    oFrameSet = source.AllocateCompositeFrame(oReleaser, oDepthFrame, oColourFrame)
                ElseIf Not IsNothing(oColourFrame) Then
                    oFrameSet = source.AllocateCompositeFrame(oReleaser, oColourFrame)
                ElseIf Not IsNothing(oDepthFrame) Then
                    oFrameSet = source.AllocateCompositeFrame(oReleaser, oDepthFrame)
                End If

                ' pass on frameset
                If Not IsNothing(oFrameSet) Then
                    source.FramesReady(oFrameSet)
                End If
            End Using
        End Sub
        Private Sub FrameCallback(frame As RS.Frame)
            ' custom frame callback
            Using oReleaser As New RS.FramesReleaser
                Dim oFrames = RS.FrameSet.FromFrame(frame, oReleaser)
                Dim oColourFrame As RS.VideoFrame = Nothing
                Dim oDepthFrame As RS.VideoFrame = Nothing
                Dim oColourCreateDate As Date = Date.MinValue
                Dim oDepthCreateDate As Date = Date.MinValue
                Dim fDepthScale As Single = 0
                Dim sColourFileName As String = String.Empty
                Dim sDepthFileName As String = String.Empty

                ' process depth frame
                If Not IsNothing(oFrames.DepthFrame) Then
                    oDepthFrame = RS.FramesReleaser.ScopedReturn(oReleaser, oFrames.DepthFrame)

                    ' set create date
                    If m_FrameDictionary.ContainsKey(oDepthFrame.Number) Then
                        fDepthScale = m_FrameDictionary(oDepthFrame.Number).Item1
                        oDepthCreateDate = m_FrameDictionary(oDepthFrame.Number).Item2
                        sDepthFileName = m_FrameDictionary(oDepthFrame.Number).Item3
                        m_FrameDictionary.Remove(oDepthFrame.Number)
                    End If
                End If

                ' process colour frame
                If Not IsNothing(oFrames.ColorFrame) Then
                    oColourFrame = RS.FramesReleaser.ScopedReturn(oReleaser, oFrames.ColorFrame)

                    ' set create date
                    If m_FrameDictionary.ContainsKey(oColourFrame.Number) Then
                        oColourCreateDate = m_FrameDictionary(oColourFrame.Number).Item2
                        sColourFileName = m_FrameDictionary(oColourFrame.Number).Item3
                        m_FrameDictionary.Remove(oColourFrame.Number)
                    End If
                End If

                ' remove all timestamp entries older than the threshold
                For i = m_FrameDictionary.Keys.Count - 1 To 0 Step -1
                    Dim oCurrentDate As Date = NTPTime.GetDateTimeNow
                    Dim sKey As String = m_FrameDictionary.Keys(i)
                    If (oCurrentDate - m_FrameDictionary(sKey).Item2).TotalSeconds > TimestampRemovalTime Then
                        m_FrameDictionary.Remove(sKey)
                    End If
                Next

                ' pass on frame
                If (Not IsNothing(oColourFrame)) OrElse (Not IsNothing(oDepthFrame)) Then
                    Dim oColourMatrix As Matrix(Of Byte) = If(IsNothing(oColourFrame), Nothing, ColourFrameToMatrix(oColourFrame))
                    Dim oDepthMatrix As Matrix(Of UShort) = If(IsNothing(oDepthFrame), Nothing, DepthFrameToMatrix(oDepthFrame))
                    Dim oDepthSingleMatrix As Matrix(Of Single) = Nothing
                    Dim fColourTimestamp As Double = If(IsNothing(oColourFrame), Double.NaN, oColourFrame.Timestamp)
                    Dim fDepthTimestamp As Double = If(IsNothing(oDepthFrame), Double.NaN, oDepthFrame.Timestamp)
                    Dim oColourTimestampDomain As RS.TimestampDomain = If(IsNothing(oColourFrame), RS.TimestampDomain.HardwareClock, oColourFrame.TimestampDomain)
                    Dim oDepthTimestampDomain As RS.TimestampDomain = If(IsNothing(oDepthFrame), RS.TimestampDomain.HardwareClock, oDepthFrame.TimestampDomain)

                    ' process only if both are present
                    If (Not IsNothing(m_ColourVideoStreamProfile)) AndAlso (Not IsNothing(m_DepthVideoStreamProfile)) AndAlso (Not IsNothing(oDepthFrame)) Then
                        ' restore depth matrix to original size
                        Using oResizedDepthMatrix As New Matrix(Of UShort)(m_DepthVideoStreamProfile.Height, m_DepthVideoStreamProfile.Width)
                            CvInvoke.Resize(oDepthMatrix, oResizedDepthMatrix, oResizedDepthMatrix.Size)
                            Using oAlignedDepthMatrix As Matrix(Of UShort) = AlignDepthToOther(oResizedDepthMatrix, m_DepthVideoStreamProfile, m_ColourVideoStreamProfile, m_DepthToColourExtrinsics, fDepthScale, oDepthFrame.BitsPerPixel)
                                oDepthSingleMatrix = New Matrix(Of Single)(oAlignedDepthMatrix.Size)
                                CvInvoke.cvConvertScale(oAlignedDepthMatrix, oDepthSingleMatrix, fDepthScale, 0)
                            End Using
                        End Using
                    End If

                    Dim oFrame As New Frame(Common.Frame.FrameActionEnum.Frame, oColourCreateDate, oDepthCreateDate, oColourMatrix, Nothing, oDepthSingleMatrix, fColourTimestamp, fDepthTimestamp, oColourTimestampDomain, oDepthTimestampDomain, sColourFileName, sDepthFileName, fDepthScale)
                    oFrame.Number = frame.Number
                    OnSoftwareFrameAvailable(oFrame)
                End If
            End Using
        End Sub
        Private Sub CaptureLoop(ByVal sDeviceName As String, ByVal oProfile As CommonProfile)
            ' capture loop
            Dim m_Pipelines = New Dictionary(Of String, Dictionary(Of RS.Stream, Tuple(Of Single, RS.Pipeline, RS.VideoStreamProfile)))

            If (Not CaptureLoopActive.Value) AndAlso Capturing = CaptureStateEnum.Capturing Then
                ' check for valid device
                If m_Devices.ContainsKey(sDeviceName) Then
                    m_FrameDictionary.Clear()

                    Dim oQueuedPipelines As New Dictionary(Of String, Dictionary(Of String, List(Of Tuple(Of RS.Stream, RS.Pipeline, RS.Config, RS.VideoStreamProfile, Single))))
                    Try
                        For Each oDevice In m_Devices(sDeviceName)
                            Dim bPipelineActive As Boolean = False

                            Dim oFormat As RS.Format = FormatNameToFormat(oProfile.Name)
                            If oFormat <> RS.Format.Any Then
                                Dim oMatchedProfiles As List(Of RS.VideoStreamProfile) = oDevice.Profiles(oFormat).Where(Function(x) x.Width = oProfile.Width AndAlso x.Height = oProfile.Height).OrderByDescending(Function(x) x.Framerate).ToList
                                If oMatchedProfiles.Count > 0 Then
                                    ' enable devices
                                    Dim oPipeline As New RS.Pipeline
                                    Dim oConfig As New RS.Config
                                    oConfig.EnableDevice(oDevice.Serial)

                                    m_ColourVideoStreamProfile = oMatchedProfiles.First
                                    oConfig.EnableStream(m_ColourVideoStreamProfile.Stream, m_ColourVideoStreamProfile.Width, m_ColourVideoStreamProfile.Height, m_ColourVideoStreamProfile.Format, m_ColourVideoStreamProfile.Framerate)

                                    ' add to dictionary
                                    If Not oQueuedPipelines.ContainsKey(sDeviceName) Then
                                        oQueuedPipelines.Add(sDeviceName, New Dictionary(Of String, List(Of Tuple(Of RS.Stream, RS.Pipeline, RS.Config, RS.VideoStreamProfile, Single))))
                                    End If
                                    If Not oQueuedPipelines(sDeviceName).ContainsKey(oDevice.Serial) Then
                                        oQueuedPipelines(sDeviceName).Add(oDevice.Serial, New List(Of Tuple(Of RS.Stream, RS.Pipeline, RS.Config, RS.VideoStreamProfile, Single)))
                                    End If

                                    oQueuedPipelines(sDeviceName)(oDevice.Serial).Add(New Tuple(Of RS.Stream, RS.Pipeline, RS.Config, RS.VideoStreamProfile, Single)(m_ColourVideoStreamProfile.Stream, oPipeline, oConfig, m_ColourVideoStreamProfile, 0))

                                    bPipelineActive = True
                                End If
                            End If

                            If bPipelineActive Then
                                Dim oMatchedProfiles As List(Of RS.VideoStreamProfile) = Nothing
                                If m_OptimalDepthDictionary.ContainsKey(sDeviceName) Then
                                    Dim iOptimalWidth As Integer = m_OptimalDepthDictionary(sDeviceName).Item1
                                    Dim iOptimalHeight As Integer = m_OptimalDepthDictionary(sDeviceName).Item2
                                    oMatchedProfiles = oDevice.Profiles.Where(Function(x) x.Stream = RS.Stream.Depth AndAlso x.Width = iOptimalWidth AndAlso x.Height = iOptimalHeight).OrderByDescending(Function(x) x.Width * x.Height).ThenByDescending(Function(x) x.Framerate).ToList
                                Else
                                    oMatchedProfiles = oDevice.Profiles.Where(Function(x) x.Stream = RS.Stream.Depth).OrderByDescending(Function(x) x.Width * x.Height).ThenByDescending(Function(x) x.Framerate).ToList
                                End If

                                If oMatchedProfiles.Count > 0 Then
                                    ' enable devices
                                    Dim oPipeline As New RS.Pipeline
                                    Dim oConfig As New RS.Config
                                    oConfig.EnableDevice(oDevice.Serial)

                                    m_DepthVideoStreamProfile = oMatchedProfiles.First
                                    oConfig.EnableStream(m_DepthVideoStreamProfile.Stream, m_DepthVideoStreamProfile.Width, m_DepthVideoStreamProfile.Height, m_DepthVideoStreamProfile.Format, m_DepthVideoStreamProfile.Framerate)

                                    oQueuedPipelines(sDeviceName)(oDevice.Serial).Add(New Tuple(Of RS.Stream, RS.Pipeline, RS.Config, RS.VideoStreamProfile, Single)(m_DepthVideoStreamProfile.Stream, oPipeline, oConfig, m_DepthVideoStreamProfile, oDevice.DepthScale))
                                End If
                            End If
                        Next
                    Catch ex As Exception
                        oQueuedPipelines.Clear()
                        Capturing = CaptureStateEnum.Stopping
                        RaiseEvent SendMessage(New Message(Name, Colors.Red, NTPTime.GetDateTimeNow, ex.Message))
                    End Try

                    If oQueuedPipelines.Count > 0 Then
                        ' close existing pipelines
                        For Each oCurrentDeviceSerial In m_Pipelines.Values
                            For Each oPipeLine In oCurrentDeviceSerial.Values
                                oPipeLine.Item2.Stop()
                                oPipeLine.Item2.Release()
                                oPipeLine.Item2.Dispose()
                            Next
                        Next
                        m_Pipelines.Clear()

                        ' add to the pipeline
                        For Each oCurrentDeviceName In oQueuedPipelines
                            For Each oCurrentDeviceSerial In oCurrentDeviceName.Value
                                For Each oCurrentStream In oCurrentDeviceSerial.Value
                                    ' start pipeline
                                    oCurrentStream.Item2.Start(oCurrentStream.Item3)

                                    ' add to dictionary
                                    If Not m_Pipelines.ContainsKey(oCurrentDeviceSerial.Key) Then
                                        m_Pipelines.Add(oCurrentDeviceSerial.Key, New Dictionary(Of RS.Stream, Tuple(Of Single, RS.Pipeline, RS.VideoStreamProfile)))
                                    End If

                                    m_Pipelines(oCurrentDeviceSerial.Key).Add(oCurrentStream.Item1, New Tuple(Of Single, RS.Pipeline, RS.VideoStreamProfile)(oCurrentStream.Item5, oCurrentStream.Item2, oCurrentStream.Item4))
                                Next
                            Next
                        Next

                        Using oProcessing As New Processing(CaptureLoopActive)
                            ' sends a full frame to signal the start of capture
                            Dim oBGRStreamProfile As RS.VideoStreamProfile = m_ColourVideoStreamProfile
                            OnFrameAvailable(Frame.StartTransmission(m_ColourVideoStreamProfile, m_DepthVideoStreamProfile, Nothing, NTPTime.GetDateTimeNow))

                            Do
                                ' check for timing
                                Dim oCurrentDate As Date = NTPTime.GetDateTimeNow
                                If m_Duration >= 0 Then
                                    Dim iTime As Integer = Math.Floor((oCurrentDate - m_CaptureStart).TotalSeconds)
                                    If iTime >= m_Duration Then
                                        Task.Run(Sub()
                                                     RaiseEvent RingAlarm()
                                                 End Sub)

                                        Threading.Interlocked.Exchange(Capturing, CaptureStateEnum.Stopping)
                                        Exit Do
                                    End If
                                    If m_Alarms.Count > 0 Then
                                        Dim iFirstAlarm As Integer = m_Alarms.Min
                                        If iTime >= iFirstAlarm Then
                                            m_Alarms.Remove(iFirstAlarm)
                                            Task.Run(Sub()
                                                         RaiseEvent RingAlarm()
                                                     End Sub)
                                        End If
                                    End If
                                End If

                                Dim oFrameBag As New ConcurrentBag(Of Tuple(Of String, RS.Stream, Single, Date, RS.FrameSet))
                                Dim bFramePresent As Boolean = False

                                Dim oActionList As New List(Of Tuple(Of Action(Of Object), Object))
                                For Each oCurrentDeviceSerial In m_Pipelines
                                    For Each oCurrentStream In oCurrentDeviceSerial.Value
                                        Dim oAction As Action(Of Object) = Sub(oParam As Tuple(Of String, RS.Stream, Single, RS.Pipeline))
                                                                               ' poll for frames
                                                                               Dim oFrameSet As RS.FrameSet = Nothing

                                                                               Try
                                                                                   If oParam.Item4.PollForFrames(oFrameSet) Then
                                                                                       oFrameBag.Add(New Tuple(Of String, RS.Stream, Single, Date, RS.FrameSet)(oParam.Item1, oParam.Item2, oParam.Item3, oCurrentDate, oFrameSet))
                                                                                       Threading.Interlocked.Exchange(bFramePresent, True)
                                                                                   End If
                                                                               Catch ex As Exception
                                                                               End Try
                                                                           End Sub
                                        oActionList.Add(New Tuple(Of Action(Of Object), Object)(oAction, New Tuple(Of String, RS.Stream, Single, RS.Pipeline)(oCurrentDeviceSerial.Key, oCurrentStream.Key, oCurrentStream.Value.Item1, oCurrentStream.Value.Item2)))
                                    Next
                                Next

                                Do
                                    CommonFunctions.ProtectedRunTasks(oActionList)

                                    If Not bFramePresent Then
                                        Threading.Thread.Sleep(10)

                                        ' exit loop if capture stopped
                                        If Capturing = CaptureStateEnum.Stopping Then
                                            Exit Do
                                        End If
                                    End If
                                Loop Until bFramePresent

                                ' process frames
                                For Each oFrameSet In oFrameBag
                                    Using oReleaser As New RS.FramesReleaser
                                        Dim oColourFrame As RS.VideoFrame = Nothing
                                        Dim oDepthFrame As RS.VideoFrame = Nothing
                                        Dim oColourCreateDate As Date = Date.MinValue
                                        Dim oDepthCreateDate As Date = Date.MinValue
                                        Dim fDepthScale As Single = 0

                                        If Not IsNothing(oFrameSet.Item5.DepthFrame) Then
                                            oDepthFrame = RS.FramesReleaser.ScopedReturn(oReleaser, oFrameSet.Item5.DepthFrame)
                                            oDepthCreateDate = oFrameSet.Item4
                                            fDepthScale = oFrameSet.Item3
                                        End If
                                        If Not IsNothing(oFrameSet.Item5.ColorFrame) Then
                                            oColourFrame = RS.FramesReleaser.ScopedReturn(oReleaser, oFrameSet.Item5.ColorFrame)
                                            oColourCreateDate = oFrameSet.Item4
                                        End If

                                        Dim oColourMatrix As Matrix(Of Byte) = If(IsNothing(oColourFrame), Nothing, ColourFrameToMatrix(oColourFrame))
                                        Dim oDepthMatrix As Matrix(Of UShort) = If(IsNothing(oDepthFrame), Nothing, DepthFrameToMatrix(oDepthFrame))
                                        Dim fColourTimestamp As Double = If(IsNothing(oColourFrame), Double.NaN, oColourFrame.Timestamp)
                                        Dim fDepthTimestamp As Double = If(IsNothing(oDepthFrame), Double.NaN, oDepthFrame.Timestamp)
                                        Dim oColourTimestampDomain As RS.TimestampDomain = If(IsNothing(oColourFrame), RS.TimestampDomain.HardwareClock, oColourFrame.TimestampDomain)
                                        Dim oDepthTimestampDomain As RS.TimestampDomain = If(IsNothing(oDepthFrame), RS.TimestampDomain.HardwareClock, oDepthFrame.TimestampDomain)

                                        Dim oFrame As New Frame(Frame.FrameActionEnum.Frame, oColourCreateDate, oDepthCreateDate, oColourMatrix, oDepthMatrix, Nothing, fColourTimestamp, fDepthTimestamp, oColourTimestampDomain, oDepthTimestampDomain, String.Empty, String.Empty, fDepthScale)
                                        OnFrameAvailable(oFrame)

                                        ' clean up
                                        oFrameSet.Item5.Dispose()
                                        If Not IsNothing(oColourFrame) Then
                                            oColourFrame.Dispose()
                                        End If
                                        If Not IsNothing(oDepthFrame) Then
                                            oDepthFrame.Dispose()
                                        End If
                                    End Using
                                Next
                            Loop Until Capturing = CaptureStateEnum.Stopping

                            ' sends an empty frame to signal the end of capture
                            OnFrameAvailable(Frame.StopTransmission)
                        End Using
                    End If
                End If
            End If

            ' clean up
            For Each oCurrentDeviceSerial In m_Pipelines.Values
                For Each oPipeLine In oCurrentDeviceSerial.Values
                    oPipeLine.Item2.Stop()
                    oPipeLine.Item2.Release()
                    oPipeLine.Item2.Dispose()
                Next
            Next
            m_Pipelines.Clear()

            Capturing = CaptureStateEnum.Idle
        End Sub
        Protected Sub OnFrameAvailable(ByVal oFrame As Frame)
            RaiseEvent FrameAvailable(oFrame)
        End Sub
        Protected Sub OnSoftwareFrameAvailable(ByVal oFrame As Frame)
            RaiseEvent SoftwareFrameAvailable(oFrame)
        End Sub
        Private Function ColourFrameToMatrix(ByVal oFrame As RS.VideoFrame) As Matrix(Of Byte)
            ' converts frame to matrix
            If oFrame.Width > 0 AndAlso oFrame.Height > 0 Then
                Dim oBuffer((oFrame.Stride * oFrame.Height) - 1) As Byte
                oFrame.CopyTo(oBuffer)

                Using oSourceMatrix As Matrix(Of Byte) = New Matrix(Of Byte)(oFrame.Height, oFrame.Width, CInt(oFrame.BitsPerPixel / 8))
                    oSourceMatrix.Bytes = oBuffer
                    Return ConvertToBGR(oSourceMatrix, oFrame.Profile.Format)
                End Using
            Else
                Return Nothing
            End If
        End Function
        Private Function DepthFrameToMatrix(ByVal oFrame As RS.VideoFrame) As Matrix(Of UShort)
            ' converts frame to matrix
            Dim oKey As New Tuple(Of RS.Stream, Integer, Integer, RS.Format, Integer)(oFrame.Profile.Stream, oFrame.Width, oFrame.Height, oFrame.Profile.Format, oFrame.Profile.Framerate)
            If oFrame.Width > 0 And oFrame.Height > 0 Then
                Dim oBuffer((oFrame.Stride * oFrame.Height) - 1) As Byte
                oFrame.CopyTo(oBuffer)

                Dim oDepthMatrix As Matrix(Of UShort) = New Matrix(Of UShort)(oFrame.Height, oFrame.Width, CInt(oFrame.BitsPerPixel / 16))
                oDepthMatrix.Bytes = oBuffer
                Return oDepthMatrix
            Else
                Return Nothing
            End If
        End Function
        Private Function ConvertToBGR(ByVal oSourceMatrix As Matrix(Of Byte), ByVal oFormat As RS.Format) As Matrix(Of Byte)
            ' converts a raw frame matrix to bgr
            Dim oReturnMatrix As New Matrix(Of Byte)(oSourceMatrix.Height, oSourceMatrix.Width, 3)
            Select Case oFormat
                Case RS.Format.Bgr8
                    oSourceMatrix.CopyTo(oReturnMatrix)
                Case RS.Format.Bgra8
                    CvInvoke.CvtColor(oSourceMatrix, oReturnMatrix, CvEnum.ColorConversion.Bgra2Bgr)
                Case RS.Format.Rgb8
                    CvInvoke.CvtColor(oSourceMatrix, oReturnMatrix, CvEnum.ColorConversion.Rgb2Bgr)
                Case RS.Format.Rgba8
                    CvInvoke.CvtColor(oSourceMatrix, oReturnMatrix, CvEnum.ColorConversion.Rgba2Bgr)
                Case RS.Format.Yuyv
                    CvInvoke.CvtColor(oSourceMatrix, oReturnMatrix, CvEnum.ColorConversion.Yuv2BgrYuy2)
            End Select
            Return oReturnMatrix
        End Function
        Private Function AlignDepthToOther(ByVal oDepthMatrix As Matrix(Of UShort), ByVal oDepthVideoStreamProfile As RS.VideoStreamProfile, ByVal oOtherVideoStreamProfile As RS.VideoStreamProfile, ByVal oDepthToOtherExtrinsics As RS.Extrinsics, ByVal fDepthScale As Single, Optional ByVal oDepthBitsPerPixel As Integer = 16) As Matrix(Of UShort)
            ' aligns the depth matrix to another stream given the video stream profiles
            Dim oDepthIntrinsics As RS.Intrinsics = oDepthVideoStreamProfile.GetIntrinsics
            Dim oOtherIntrinsics As RS.Intrinsics = oOtherVideoStreamProfile.GetIntrinsics
            If IsNothing(oDepthToOtherExtrinsics) Then
                oDepthToOtherExtrinsics = oDepthVideoStreamProfile.GetExtrinsicsTo(oOtherVideoStreamProfile)
            End If
            Dim oDepthBpp As Integer = oDepthBitsPerPixel / 8
            Dim oError As Object = Nothing

            ' create the source and destination depth buffers
            Dim oDepthBuffer((oDepthMatrix.Width * oDepthMatrix.Height) - 1) As UShort
            Buffer.BlockCopy(oDepthMatrix.Bytes, 0, oDepthBuffer, 0, oDepthMatrix.Bytes.Length)
            Dim oDepthOutBuffer((oOtherVideoStreamProfile.Width * oOtherVideoStreamProfile.Height) - 1) As UShort

            ' run librealsense align function
            RS.NativeMethods.rs2_align_frame(oDepthOutBuffer, oDepthBuffer, oOtherIntrinsics, oDepthIntrinsics, oDepthToOtherExtrinsics, oDepthBpp, fDepthScale, oError)

            ' create a new depth matrix using the buffer data
            Dim oDepthOutMatrix As New Matrix(Of UShort)(oDepthOutBuffer)
            Dim oReshapedMatrix As Matrix(Of UShort) = oDepthOutMatrix.Reshape(1, oOtherVideoStreamProfile.Height)

            Return oReshapedMatrix
        End Function
        Private Function FormatToStreamType(ByVal oFormat As RS.Format) As StreamTypeEnum
            ' converts intel realsense format to streamtype
            Dim oStreamType As StreamTypeEnum = StreamTypeEnum.None
            Select Case oFormat
                Case RS.Format.Bgr8, RS.Format.Bgra8, RS.Format.Rgb8, RS.Format.Rgba8, RS.Format.Yuyv
                    oStreamType = StreamTypeEnum.Colour
                Case RS.Format.Z16
                    oStreamType = StreamTypeEnum.Depth
            End Select
            Return oStreamType
        End Function
        Private Function FormatNameToFormat(ByVal sFormatName As String) As RS.Format
            ' converts intel realsense format to streamtype
            Dim oFormat As RS.Format = RS.Format.Any
            If [Enum].IsDefined(GetType(RS.Format), sFormatName) Then
                oFormat = [Enum].Parse(GetType(RS.Format), sFormatName)
            End If
            Return oFormat
        End Function
        Public Enum CaptureStateEnum As Integer
            ' state should transition from idle --> capturing --> stopping --> idle
            Idle = 0
            Capturing
            Stopping
        End Enum
        Public Enum StreamTypeEnum As Integer
            None = 0
            Colour
            Depth
            DepthSingle
            DepthMask
        End Enum
        Public Structure CommonDevice
            Public ReadOnly Property Devices As Dictionary(Of Guid, String)
            Public ReadOnly Property Name As String
            Public ReadOnly Property Profiles As Dictionary(Of String, List(Of CommonProfile))

            Sub New(ByVal oDevices As Dictionary(Of Guid, String), ByVal sName As String, ByVal oProfiles As List(Of CommonProfile))
                Devices = oDevices
                Name = sName

                Profiles = New Dictionary(Of String, List(Of CommonProfile))
                For Each oProfile In oProfiles
                    If Not Profiles.ContainsKey(oProfile.Name) Then
                        Profiles.Add(oProfile.Name, New List(Of CommonProfile))
                    End If
                    Profiles(oProfile.Name).Add(oProfile)
                Next
            End Sub
            Public ReadOnly Property DeviceDisplay As HCBDisplay
                Get
                    Dim sColourName As String = "DarkSlateBlue"
                    Return New HCBDisplay(Name, True, sColourName, Devices.GetHashCode)
                End Get
            End Property
            Public ReadOnly Property ColourProfiles As Dictionary(Of String, List(Of CommonProfile))
                Get
                    Return (From oProfile In Profiles Where oProfile.Value.First.StreamType = StreamTypeEnum.Colour Select oProfile).ToDictionary(Function(x) x.Key, Function(x) x.Value)
                End Get
            End Property
        End Structure
        Public Structure CommonProfile
            Public ReadOnly Property DeviceName As String
            Public ReadOnly Property Name As String
            Public ReadOnly Property Width As Integer
            Public ReadOnly Property Height As Integer
            Public ReadOnly Property StreamType As StreamTypeEnum
            Public ReadOnly Property Tag As Object

            Sub New(ByVal sDeviceName As String, ByVal sName As String, ByVal iWidth As Integer, ByVal iHeight As Integer, ByVal oStreamType As StreamTypeEnum, ByVal oTag As Object)
                DeviceName = sDeviceName
                Name = sName
                Width = iWidth
                Height = iHeight
                StreamType = oStreamType
                Tag = oTag
            End Sub
            Public ReadOnly Property ProfileDisplay As HCBDisplay
                Get
                    Dim sStreamName As String = [Enum].GetName(GetType(StreamTypeEnum), StreamType)
                    Return New HCBDisplay(Name.ToUpper + " " + sStreamName, False,, DeviceName.GetHashCode)
                End Get
            End Property
            Public ReadOnly Property ResolutionDisplay As HCBDisplay
                Get
                    Return New HCBDisplay(Width.ToString + "x" + Height.ToString, False,, DeviceName.GetHashCode Xor Name.GetHashCode)
                End Get
            End Property
        End Structure
        Private Class Device
            Private m_Profiles As List(Of RS.VideoStreamProfile)
            Public ReadOnly Property Device As RS.Device
            Public ReadOnly Property GUID As Guid

            Sub New(ByVal oDevice As RS.Device)
                Device = oDevice
                GUID = Guid.NewGuid

                ' get depth scale
                Dim oSensors As RS.SensorList = oDevice.Sensors
                Dim oDepthSensorScaleList As List(Of Single) = (From oSensor In oSensors From oOption In oSensor.Options Where oOption.Key = RS.Option.DepthUnits Select oOption.Value).ToList
                If oDepthSensorScaleList.Count > 0 Then
                    DepthScale = oDepthSensorScaleList.First
                Else
                    DepthScale = -1
                End If

                ' get profiles with the highest framerate
                m_Profiles = New List(Of RS.VideoStreamProfile)
                For Each oSensor In oSensors
                    Dim oProfileTypeList As List(Of Tuple(Of Integer, Integer, RS.Format)) = (From oVideoStreamProfile In oSensor.VideoStreamProfiles Select New Tuple(Of Integer, Integer, RS.Format)(oVideoStreamProfile.Width, oVideoStreamProfile.Height, oVideoStreamProfile.Format) Distinct).ToList
                    For Each oProfileType In oProfileTypeList
                        Dim oCurrentProfileType As Tuple(Of Integer, Integer, RS.Format) = oProfileType
                        Dim oProfileList As List(Of RS.VideoStreamProfile) = (From oVideoStreamProfile In oSensor.VideoStreamProfiles Where oVideoStreamProfile.Width = oCurrentProfileType.Item1 AndAlso oVideoStreamProfile.Height = oCurrentProfileType.Item2 AndAlso oVideoStreamProfile.Format = oCurrentProfileType.Item3 AndAlso oVideoStreamProfile.Framerate >= MinFrameRate Order By oVideoStreamProfile.Framerate Descending Select oVideoStreamProfile).ToList
                        If oProfileList.Count > 0 Then
                            If oProfileList.First.Stream = RS.Stream.Depth Then
                                ' add fastest profile for depth stream
                                m_Profiles.Add(oProfileList.First)
                            Else
                                ' add slowest profile for all other streams
                                m_Profiles.Add(oProfileList.Last)
                            End If
                        End If
                    Next
                Next
                m_Profiles = m_Profiles.OrderByDescending(Function(oProfile) oProfile.Stream).ThenByDescending(Function(oProfile) oProfile.Format).ThenByDescending(Function(oProfile) oProfile.Width).ThenByDescending(Function(oProfile) oProfile.Height).ToList
            End Sub
            Public ReadOnly Property Name As String
                Get
                    Return Device.Info(RS.CameraInfo.Name)
                End Get
            End Property
            Public ReadOnly Property Serial As String
                Get
                    Return Device.Info(RS.CameraInfo.SerialNumber)
                End Get
            End Property
            Public ReadOnly Property ProductId As String
                Get
                    Return Device.Info(RS.CameraInfo.ProductId)
                End Get
            End Property
            Public ReadOnly Property DepthScale As Single
            Public Function Profiles(ByVal oFormat As RS.Format) As List(Of RS.VideoStreamProfile)
                ' get list of profiles of that format
                Return (From oProfile In m_Profiles Where oFormat = oProfile.Format Select oProfile).ToList
            End Function
            Public Function Profiles() As List(Of RS.VideoStreamProfile)
                ' get list of all profiles
                Return m_Profiles
            End Function
            Public Overrides Function Equals(obj As [Object]) As Boolean
                ' check for null and compare run-time types.
                If (obj Is Nothing) OrElse Not Me.[GetType]().Equals(obj.[GetType]()) Then
                    Return False
                Else
                    Dim oDevice As Device = DirectCast(obj, Device)
                    Return (oDevice.Name = Name) AndAlso (oDevice.Serial = Serial) AndAlso (oDevice.ProductId = ProductId)
                End If
            End Function
            Public Overrides Function GetHashCode() As Integer
                Return Name.GetHashCode Xor Serial.GetHashCode Xor ProductId.GetHashCode
            End Function
        End Class
#Region "IDisposable Support"
        Private disposedValue As Boolean
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    ' TODO: dispose managed state (managed objects).
                    CaptureStop()

                    ' dispose of unmanaged resources
                    If Not IsNothing(m_Context) Then
                        m_Context.Dispose()
                        m_Context = Nothing
                    End If
                    If Not IsNothing(m_Align) Then
                        m_Align.Dispose()
                        m_Align = Nothing
                    End If
                    If Not IsNothing(m_Decimate) Then
                        m_Decimate.Dispose()
                        m_Decimate = Nothing
                    End If
                    If Not IsNothing(m_DisparityTransform) Then
                        m_DisparityTransform.Dispose()
                        m_DisparityTransform = Nothing
                    End If
                    If Not IsNothing(m_DepthTransform) Then
                        m_DepthTransform.Dispose()
                        m_DepthTransform = Nothing
                    End If
                    If Not IsNothing(m_Spatial) Then
                        m_Spatial.Dispose()
                        m_Spatial = Nothing
                    End If
                    If Not IsNothing(m_Temporal) Then
                        m_Temporal.Dispose()
                        m_Temporal = Nothing
                    End If
                    If Not IsNothing(m_HoleFilling) Then
                        m_HoleFilling.Dispose()
                        m_HoleFilling = Nothing
                    End If
                    If Not IsNothing(m_Block) Then
                        m_Block.Dispose()
                        m_Block = Nothing
                    End If
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
        Public Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
            ' TODO: uncomment the following line if Finalize() is overridden above.
            ' GC.SuppressFinalize(Me)
        End Sub
#End Region
    End Class
    Public Class Frame
        Implements IDisposable, ICloneable

        Public FrameAction As FrameActionEnum
        Public SentDate As Date
        Public ColourCreateDate As Date
        Public DepthCreateDate As Date
        Public ColourFrame As Matrix(Of Byte)
        Public DepthFrame As Matrix(Of UShort)
        Public DepthFrameSingle As Matrix(Of Single)
        Public ColourTimestamp As Double
        Public DepthTimestamp As Double
        Public ColourTimestampDomain As Integer
        Public DepthTimestampDomain As Integer
        Public ColourFileName As String
        Public DepthFileName As String
        Public DepthScale As Single
        Public ColourDepthProfile As StreamProfiles
        Public Number As Integer
        Public OriginalNumber As Integer

        Sub New(ByVal oFrame As Frame)
            With oFrame
                FrameAction = .FrameAction
                SentDate = .SentDate
                ColourCreateDate = .ColourCreateDate
                DepthCreateDate = .DepthCreateDate
                ColourFrame = If(IsNothing(.ColourFrame), Nothing, .ColourFrame.Clone)
                DepthFrame = If(IsNothing(.DepthFrame), Nothing, .DepthFrame.Clone)
                DepthFrameSingle = If(IsNothing(.DepthFrameSingle), Nothing, .DepthFrameSingle.Clone)
                ColourTimestamp = .ColourTimestamp
                DepthTimestamp = .DepthTimestamp
                ColourTimestampDomain = .ColourTimestampDomain
                DepthTimestampDomain = .DepthTimestampDomain
                DepthScale = .DepthScale
                ColourDepthProfile = .ColourDepthProfile
                Number = .Number
                OriginalNumber = .OriginalNumber
            End With
        End Sub
        Sub New(ByVal oFrameAction As FrameActionEnum, ByVal oColourCreateDate As Date, ByVal oDepthCreateDate As Date, ByVal oColourFrame As Matrix(Of Byte), ByVal oDepthFrame As Matrix(Of UShort), ByVal oDepthFrameSingle As Matrix(Of Single), ByVal fColourTimestamp As Double, ByVal fDepthTimestamp As Double, ByVal oColourTimestampDomain As RS.TimestampDomain, ByVal oDepthTimestampDomain As RS.TimestampDomain, ByVal sColourFileName As String, ByVal sDepthFileName As String, ByVal fDepthScale As Single)
            FrameAction = oFrameAction
            SentDate = Date.MinValue
            ColourCreateDate = oColourCreateDate
            DepthCreateDate = oDepthCreateDate
            ColourFrame = oColourFrame
            DepthFrame = oDepthFrame
            DepthFrameSingle = oDepthFrameSingle
            ColourTimestamp = fColourTimestamp
            DepthTimestamp = fDepthTimestamp
            ColourTimestampDomain = oColourTimestampDomain
            DepthTimestampDomain = oDepthTimestampDomain
            ColourFileName = sColourFileName
            DepthFileName = sDepthFileName
            DepthScale = fDepthScale
            ColourDepthProfile = New StreamProfiles(Nothing, Nothing, Nothing)
            Number = -1
            OriginalNumber = -1
        End Sub
        Sub New(ByVal oFrameAction As FrameActionEnum, ByVal oSentDate As Date, ByVal oColourCreateDate As Date, ByVal oDepthCreateDate As Date, ByVal oColourFrame As Matrix(Of Byte), ByVal oDepthFrame As Matrix(Of UShort), ByVal oDepthFrameSingle As Matrix(Of Single), ByVal fColourTimestamp As Double, ByVal fDepthTimestamp As Double, ByVal oColourTimestampDomain As RS.TimestampDomain, ByVal oDepthTimestampDomain As RS.TimestampDomain, ByVal sColourFileName As String, ByVal sDepthFileName As String, ByVal fDepthScale As Single)
            FrameAction = oFrameAction
            SentDate = oSentDate
            ColourCreateDate = oColourCreateDate
            DepthCreateDate = oDepthCreateDate
            ColourFrame = oColourFrame
            DepthFrame = oDepthFrame
            DepthFrameSingle = oDepthFrameSingle
            ColourTimestamp = fColourTimestamp
            DepthTimestamp = fDepthTimestamp
            ColourTimestampDomain = oColourTimestampDomain
            DepthTimestampDomain = oDepthTimestampDomain
            ColourFileName = sColourFileName
            DepthFileName = sDepthFileName
            DepthScale = fDepthScale
            ColourDepthProfile = New StreamProfiles(Nothing, Nothing, Nothing)
            Number = -1
            OriginalNumber = -1
        End Sub
        Public Function Clone() As Object Implements ICloneable.Clone
            Return New Frame(Me)
        End Function
        Public Function CloneEmpty() As Object
            Return New Frame(FrameAction, SentDate, ColourCreateDate, DepthCreateDate, Nothing, Nothing, Nothing, Double.NaN, Double.NaN, RS.TimestampDomain.HardwareClock, RS.TimestampDomain.HardwareClock, String.Empty, String.Empty, DepthScale)
        End Function
        Public Shared Function None() As Frame
            ' returns empty frame
            Return New Frame(FrameActionEnum.None, Date.MinValue, Date.MinValue, Nothing, Nothing, Nothing, Double.NaN, Double.NaN, RS.TimestampDomain.HardwareClock, RS.TimestampDomain.HardwareClock, String.Empty, String.Empty, 0)
        End Function
        Public Shared Function StartTransmission(ByVal oColourStreamProfile As RS.VideoStreamProfile, ByVal oDepthStreamProfile As RS.VideoStreamProfile, ByVal oDepthToColour As RS.Extrinsics, ByVal oCreateDate As Date) As Frame
            ' returns start transmission frame
            Dim oFrame As New Frame(FrameActionEnum.StartTransmission, oCreateDate, oCreateDate, Nothing, Nothing, Nothing, Double.NaN, Double.NaN, RS.TimestampDomain.HardwareClock, RS.TimestampDomain.HardwareClock, String.Empty, String.Empty, 0)
            oFrame.ColourDepthProfile = New StreamProfiles(oColourStreamProfile, oDepthStreamProfile, oDepthToColour)
            Return oFrame
        End Function
        Public Shared Function StopTransmission() As Frame
            ' returns stop transmission frame
            Return New Frame(FrameActionEnum.StopTransmission, Date.MinValue, Date.MinValue, Nothing, Nothing, Nothing, Double.NaN, Double.NaN, RS.TimestampDomain.HardwareClock, RS.TimestampDomain.HardwareClock, String.Empty, String.Empty, 0)
        End Function
        Public Enum FrameActionEnum As Integer
            None
            Frame
            EmptyFrame
            StartTransmission
            StopTransmission
        End Enum
        <DataContract()> Public Class StreamProfiles
            <DataMember> Public ColourVideoStreamProfile As VideoStreamProfile
            <DataMember> Public DepthVideoStreamProfile As VideoStreamProfile
            <DataMember> Public DepthToColour As Extrinsics

            Sub New(ByVal oColourStreamProfile As RS.VideoStreamProfile, ByVal oDepthStreamProfile As RS.VideoStreamProfile, ByVal oDepthToColour As RS.Extrinsics)
                If IsNothing(oColourStreamProfile) Then
                    ColourVideoStreamProfile = Nothing
                Else
                    ColourVideoStreamProfile = New VideoStreamProfile(oColourStreamProfile)
                    ColourVideoStreamProfile.format = RS.Format.Bgr8
                    ColourVideoStreamProfile.bpp = VideoStreamProfile.FormatBPP(RS.Format.Bgr8)
                End If
                If IsNothing(oDepthStreamProfile) Then
                    DepthVideoStreamProfile = Nothing
                Else
                    DepthVideoStreamProfile = New VideoStreamProfile(oDepthStreamProfile)
                End If
                If IsNothing(oColourStreamProfile) OrElse IsNothing(oDepthStreamProfile) Then
                    DepthToColour = Nothing
                ElseIf (Not IsNothing(oDepthToColour.rotation)) AndAlso (Not IsNothing(oDepthToColour.translation)) Then
                    DepthToColour = New Extrinsics(oDepthToColour)
                Else
                    Dim oDepthToColourExtrinsics As RS.Extrinsics = oDepthStreamProfile.GetExtrinsicsTo(oColourStreamProfile)
                    DepthToColour = New Extrinsics(oDepthToColourExtrinsics)
                End If
            End Sub
            Public Shared Function GetKnownTypes() As List(Of Type)
                ' returns the list of additonal types
                Return New List(Of Type) From {GetType(VideoStreamProfile), GetType(Extrinsics)}
            End Function
        End Class
        <DataContract()> Public Class VideoStreamProfile
            <DataMember> Public type As Integer
            <DataMember> Public index As Integer
            <DataMember> Public uid As Integer
            <DataMember> Public width As Integer
            <DataMember> Public height As Integer
            <DataMember> Public fps As Integer
            <DataMember> Public bpp As Integer
            <DataMember> Public format As Integer
            <DataMember> Public intppx As Single
            <DataMember> Public intppy As Single
            <DataMember> Public intfx As Single
            <DataMember> Public intfy As Single
            <DataMember> Public intmodel As Integer
            <DataMember> Public intcoeffs As Single()

            Sub New(ByVal oStreamProfile As RS.VideoStreamProfile)
                type = oStreamProfile.Stream
                index = oStreamProfile.Index
                uid = oStreamProfile.UniqueID
                width = oStreamProfile.Width
                height = oStreamProfile.Height
                fps = oStreamProfile.Framerate
                bpp = FormatBPP(oStreamProfile.Format)
                format = oStreamProfile.Format

                Dim oIntrinsics As RS.Intrinsics = oStreamProfile.GetIntrinsics()
                With oIntrinsics
                    width = .width
                    height = .height
                    intppx = .ppx
                    intppy = .ppy
                    intfx = .fx
                    intfy = .fy
                    intmodel = .model
                    intcoeffs = .coeffs
                End With
            End Sub
            Public Function GetVideoStream(Optional ByVal oIntrinsics As RS.Intrinsics = Nothing) As RS.VideoStream
                ' gets videostream
                If IsNothing(oIntrinsics.coeffs) Then
                    oIntrinsics = New RS.Intrinsics
                    With oIntrinsics
                        .width = width
                        .height = height
                        .ppx = intppx
                        .ppy = intppy
                        .fx = intfx
                        .fy = intfy
                        .model = intmodel
                        .coeffs = intcoeffs
                    End With
                End If

                Dim oVideoStream As New RS.VideoStream
                With oVideoStream
                    .type = CType(type, RS.Stream)
                    .index = index
                    .uid = uid
                    .width = width
                    .height = height
                    .fps = fps
                    .bpp = bpp
                    .fmt = CType(format, RS.Format)
                    .intrinsics = oIntrinsics
                End With

                Return oVideoStream
            End Function
            Public Shared Function FormatBPP(ByVal oFormat As RS.Format) As Integer
                ' gets bpp for intel realsense format
                Select Case oFormat
                    Case RS.Format.Bgra8, RS.Format.Rgba8
                        Return 32
                    Case RS.Format.Bgr8, RS.Format.Rgb8
                        Return 24
                    Case RS.Format.Yuyv, RS.Format.Z16
                        Return 16
                    Case Else
                        Return 0
                End Select
            End Function
            Public Shared Function GetKnownTypes() As List(Of Type)
                ' returns the list of additonal types
                Return New List(Of Type) From {GetType(Single())}
            End Function
        End Class
        <DataContract()> Public Class Extrinsics
            <DataMember> Public rotation As Single()
            <DataMember> Public translation As Single()

            Sub New(ByVal oExtrinsics As RS.Extrinsics)
                rotation = oExtrinsics.rotation
                translation = oExtrinsics.translation
            End Sub
            Public Function GetExtrinsics() As RS.Extrinsics
                Dim oExtrinsics As New RS.Extrinsics
                oExtrinsics.rotation = rotation
                oExtrinsics.translation = translation
                Return oExtrinsics
            End Function
            Public Shared Function GetKnownTypes() As List(Of Type)
                ' returns the list of additonal types
                Return New List(Of Type) From {GetType(Single())}
            End Function
        End Class
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
                    ' dispose of unmanaged resources
                    If CommonFunctions.MatrixNotNothing(ColourFrame) Then
                        ColourFrame.Dispose()
                        ColourFrame = Nothing
                    End If
                    If CommonFunctions.MatrixNotNothing(DepthFrame) Then
                        DepthFrame.Dispose()
                        DepthFrame = Nothing
                    End If
                    If CommonFunctions.MatrixNotNothing(DepthFrameSingle) Then
                        DepthFrameSingle.Dispose()
                        DepthFrameSingle = Nothing
                    End If
                End If
            End SyncLock
        End Sub
#End Region
    End Class
    Public Structure MatrixData(Of T As Structure)
        Public Param As MatrixDataParam
        Public Frame As Byte()

        Sub New(ByVal oMatrix As Matrix(Of T))
            With Me
                If IsNothing(oMatrix) Then
                    Param = Nothing
                Else
                    Param = New MatrixDataParam(oMatrix.Width, oMatrix.Height, oMatrix.NumberOfChannels)
                    Using oCloneMatrix As Matrix(Of T) = oMatrix.Clone
                        .Frame = oCloneMatrix.Bytes
                    End Using
                End If
            End With
        End Sub
        Public Function GetMatrix() As Matrix(Of T)
            ' creates matrix from data
            If Frame.Length <> 0 AndAlso (Not IsNothing(Param)) Then
                Dim oMatrix As Matrix(Of T) = Nothing
                With Me
                    oMatrix = New Matrix(Of T)(.Param.Height, .Param.Width, .Param.Channels)
                    oMatrix.Bytes = .Frame
                End With
                Return oMatrix
            Else
                Return Nothing
            End If
        End Function
        Public Function GetBytes() As Byte()
            ' gets bytes from data
            If Frame.Length <> 0 AndAlso (Not IsNothing(Param)) Then
                Dim oParamBytes As Byte() = Param.GetBytes
                Dim oBytes(oParamBytes.Length + Frame.Length - 1) As Byte
                Buffer.BlockCopy(oParamBytes, 0, oBytes, 0, oParamBytes.Length)
                Buffer.BlockCopy(Frame, 0, oBytes, oParamBytes.Length, Frame.Length)

                ' use snappy compression
                Dim oCompressedBytes As Byte() = Snappy.SnappyCodec.Compress(oBytes)
                Return oCompressedBytes
            Else
                Return Nothing
            End If
        End Function
        Public Shared Function GetMatrixData(ByVal oCompressedBytes As Byte()) As MatrixData(Of T)
            ' gets data from bytes
            Dim oBytes As Byte() = Snappy.SnappyCodec.Uncompress(oCompressedBytes)
            Dim oMatrixData As New MatrixData(Of T)
            Dim iParamSize As Integer = Marshal.SizeOf(GetType(MatrixDataParam))
            Dim oParamBytes(iParamSize - 1) As Byte
            Buffer.BlockCopy(oBytes, 0, oParamBytes, 0, oParamBytes.Length)
            oMatrixData.Param = MatrixDataParam.GetParam(oParamBytes)
            ReDim oMatrixData.Frame((oBytes.Length - iParamSize) - 1)
            Buffer.BlockCopy(oBytes, oParamBytes.Length, oMatrixData.Frame, 0, oMatrixData.Frame.Length)
            Return oMatrixData
        End Function
    End Structure
    Public Structure MatrixDataParam
        Public Width As Integer
        Public Height As Integer
        Public Channels As Integer

        Sub New(ByVal iWidth As Integer, ByVal iHeight As Integer, ByVal iChannels As Integer)
            With Me
                .Width = iWidth
                .Height = iHeight
                .Channels = iChannels
            End With
        End Sub
        Public Function GetBytes() As Byte()
            ' gets bytes from data
            Dim iSize As Integer = Marshal.SizeOf(Me)
            Dim oPtr As IntPtr = Marshal.AllocHGlobal(iSize)
            Dim oBytes(iSize - 1) As Byte
            Marshal.StructureToPtr(Me, oPtr, False)
            Marshal.Copy(oPtr, oBytes, 0, iSize)
            Marshal.FreeHGlobal(oPtr)
            Return oBytes
        End Function
        Public Shared Function GetParam(ByVal oBytes As Byte()) As MatrixDataParam
            ' gets data from bytes
            Dim oHandle As GCHandle = GCHandle.Alloc(oBytes, GCHandleType.Pinned)
            Dim oParam As MatrixDataParam = Marshal.PtrToStructure(oHandle.AddrOfPinnedObject, GetType(MatrixDataParam))
            oHandle.Free()
            Return oParam
        End Function
    End Structure
    Public Class Message
        ' represents a single message with a source provider, a colour representing the source, a time of creation, and a message content
        Implements INotifyPropertyChanged

        Private m_Source As String
        Private m_Colour As Color
        Private m_DateTime As Date
        Private m_Message As String

        Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged
        Protected Sub OnPropertyChanged(ByVal sName As String)
            RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(sName))
        End Sub
        Public Sub New(ByVal sSource As String, oColour As Color, ByVal oDateTime As Date, ByVal sMessage As String)
            m_Source = sSource
            m_Colour = oColour
            m_DateTime = oDateTime
            m_Message = sMessage
            OnPropertyChanged(String.Empty)
        End Sub
        Public Property Source As String
            Get
                Return m_Source
            End Get
            Set(value As String)
                m_Source = value
                OnPropertyChanged("Source")
            End Set
        End Property
        Public Property Colour As Color
            Get
                Return m_Colour
            End Get
            Set(value As Color)
                m_Colour = value
                OnPropertyChanged("Colour")
                OnPropertyChanged("ColourText")
            End Set
        End Property
        Public ReadOnly Property ColourText As String
            Get
                Return m_Colour.ToString()
            End Get
        End Property
        Public Property DateTime As Date
            Get
                Return m_DateTime
            End Get
            Set(value As Date)
                m_DateTime = value
                OnPropertyChanged("DateTime")
                OnPropertyChanged("DateTimeText")
            End Set
        End Property
        Public ReadOnly Property DateTimeText As String
            Get
                Return m_DateTime.ToShortDateString + " " + m_DateTime.ToLongTimeString
            End Get
        End Property
        Public Property Message As String
            Get
                Return m_Message
            End Get
            Set(value As String)
                m_Message = value
                OnPropertyChanged("Message")
            End Set
        End Property
    End Class
    Public Class Processing
        ' blocks until processing is complete
        Implements IDisposable

        Private m_Processing As BooleanObject
        Private m_ExitDelegate As Action

        Sub New(ByRef oBooleanObject As BooleanObject, Optional ByVal oEntryDelegate As Action = Nothing, Optional ByVal oExitDelegate As Action = Nothing)
            ' the delegate is invoked on disposing the object
            m_Processing = oBooleanObject
            If m_Processing Is Nothing Then
                m_Processing = New BooleanObject(True)
            Else
                m_Processing.Value = True
            End If

            m_ExitDelegate = oExitDelegate

            ' invoke entry delegate
            If Not oEntryDelegate Is Nothing Then
                oEntryDelegate.Invoke
            End If
        End Sub
        Public ReadOnly Property IsProcessing As Boolean
            Get
                Return m_Processing.Value
            End Get
        End Property
        Public Class BooleanObject
            Private m_Boolean As Boolean

            Sub New(ByVal bBoolean As Boolean)
                m_Boolean = bBoolean
            End Sub
            Public Property Value As Boolean
                Get
                    Return m_Boolean
                End Get
                Set(value As Boolean)
                    m_Boolean = value
                End Set
            End Property
        End Class
        Public Shared Sub CheckProcessing(ByVal oIsProcessing As BooleanObject)
            ' if processing is ongoing, pause until processing stops
            If Not oIsProcessing Is Nothing Then
                Do While oIsProcessing.Value
                    Task.Delay(100).Wait()
                Loop
            End If
        End Sub
#Region "IDisposable Support"
        Private disposedValue As Boolean
        Protected Shadows Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    ' TODO: dispose managed state (managed objects).
                    m_Processing.Value = False

                    ' invoke exit delegate
                    If Not m_ExitDelegate Is Nothing Then
                        m_ExitDelegate.Invoke
                    End If
                End If
            End If
            disposedValue = True
        End Sub
        Public Shadows Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
        End Sub
#End Region
    End Class
    Public Structure HCBDisplay
        Sub New(ByVal sName As String, ByVal bHighlight As Boolean, Optional ByVal sColour As String = "Red", Optional oTag As Object = Nothing)
            Name = sName
            Highlight = bHighlight
            Colour = sColour
            Tag = oTag
        End Sub
        Public Property Name As String
        Public Property Highlight As Boolean
        Public Property Colour As String
        Public Property Tag As Object
        Public Overloads Overrides Function Equals(obj As Object) As Boolean
            If obj Is Nothing OrElse Not Me.GetType() Is obj.GetType() Then
                Return False
            End If

            Dim oHCBDisplay As HCBDisplay = CType(obj, HCBDisplay)
            Return Name = oHCBDisplay.Name AndAlso Highlight = oHCBDisplay.Highlight AndAlso Colour = oHCBDisplay.Colour AndAlso
                    ((Tag Is Nothing And oHCBDisplay.Tag Is Nothing) OrElse
                    (((Not Tag Is Nothing) And (Not oHCBDisplay.Tag Is Nothing)) AndAlso (Tag.GetHashCode = oHCBDisplay.Tag.GetHashCode)))
        End Function
        Public Overrides Function GetHashCode() As Integer
            If Tag Is Nothing Then
                Return Name.GetHashCode Xor Highlight.GetHashCode Xor Colour.GetHashCode
            Else
                Return Name.GetHashCode Xor Highlight.GetHashCode Xor Colour.GetHashCode Xor Tag.GetHashCode
            End If
        End Function
    End Structure
    Public Class FourCC
        Public Shared UserSelect As Integer = -1
        Public Shared _1978 As Integer = 943143217
        Public Shared _2VUY As Integer = 1498764850
        Public Shared _3IV0 As Integer = 810961203
        Public Shared _3IV1 As Integer = 827738419
        Public Shared _3IV2 As Integer = 844515635
        Public Shared _3IVD As Integer = 1146505523
        Public Shared _3IVX As Integer = 1482049843
        Public Shared _8BPS As Integer = 1397768760
        Public Shared AAS4 As Integer = 877871425
        Public Shared AASC As Integer = 1129529665
        Public Shared ABYR As Integer = 1381581377
        Public Shared ACTL As Integer = 1280590657
        Public Shared ADV1 As Integer = 827737153
        Public Shared ADVJ As Integer = 1247167553
        Public Shared AEIK As Integer = 1263093057
        Public Shared AEMI As Integer = 1229800769
        Public Shared AFLC As Integer = 1129072193
        Public Shared AFLI As Integer = 1229735489
        Public Shared AHDV As Integer = 1447315521
        Public Shared AJPG As Integer = 1196444225
        Public Shared AMPG As Integer = 1196444993
        Public Shared ANIM As Integer = 1296649793
        Public Shared AP41 As Integer = 825512001
        Public Shared AP42 As Integer = 842289217
        Public Shared ASLC As Integer = 1129075521
        Public Shared ASV1 As Integer = 827740993
        Public Shared ASV2 As Integer = 844518209
        Public Shared ASVX As Integer = 1482052417
        Public Shared ATM4 As Integer = 877483073
        Public Shared AUR2 As Integer = 844256577
        Public Shared AURA As Integer = 1095914817
        Public Shared AVC1 As Integer = 826496577
        Public Shared AVRN As Integer = 1314018881
        Public Shared BA81 As Integer = 825770306
        Public Shared BINK As Integer = 1263421762
        Public Shared BLZ0 As Integer = 811224130
        Public Shared BT20 As Integer = 808604738
        Public Shared BTCV As Integer = 1447253058
        Public Shared BW10 As Integer = 808539970
        Public Shared BYR1 As Integer = 827480386
        Public Shared BYR2 As Integer = 844257602
        Public Shared CC12 As Integer = 842089283
        Public Shared CDVC As Integer = 1129727043
        Public Shared CFCC As Integer = 1128482371
        Public Shared CGDI As Integer = 1229211459
        Public Shared CHAM As Integer = 1296123971
        Public Shared CJPG As Integer = 1196444227
        Public Shared CMYK As Integer = 1264143683
        Public Shared CPLA As Integer = 1095520323
        Public Shared CRAM As Integer = 1296126531
        Public Shared CSCD As Integer = 1145262915
        Public Shared CTRX As Integer = 1481790531
        Public Shared CVID As Integer = 1145656899
        Public Shared CWLT As Integer = 1414289219
        Public Shared CXY1 As Integer = 827938883
        Public Shared CXY2 As Integer = 844716099
        Public Shared CYUV As Integer = 1448433987
        Public Shared CYUY As Integer = 1498765635
        Public Shared D261 As Integer = 825635396
        Public Shared D263 As Integer = 859189828
        Public Shared DAVC As Integer = 1129726276
        Public Shared DCL1 As Integer = 827081540
        Public Shared DCL2 As Integer = 843858756
        Public Shared DCL3 As Integer = 860635972
        Public Shared DCL4 As Integer = 877413188
        Public Shared DCL5 As Integer = 894190404
        Public Shared DIV3 As Integer = 861292868
        Public Shared DIV4 As Integer = 878070084
        Public Shared DIV5 As Integer = 894847300
        Public Shared DIVX As Integer = 1482049860
        Public Shared DM4V As Integer = 1446268228
        Public Shared DMB1 As Integer = 826428740
        Public Shared DMB2 As Integer = 843205956
        Public Shared DMK2 As Integer = 843795780
        Public Shared DSVD As Integer = 1146508100
        Public Shared DUCK As Integer = 1262703940
        Public Shared DV25 As Integer = 892491332
        Public Shared DV50 As Integer = 808801860
        Public Shared DVAN As Integer = 1312904772
        Public Shared DVCS As Integer = 1396921924
        Public Shared DVE2 As Integer = 843404868
        Public Shared DVH1 As Integer = 826824260
        Public Shared DVHD As Integer = 1145591364
        Public Shared DVSD As Integer = 1146312260
        Public Shared DVSL As Integer = 1280529988
        Public Shared DVX1 As Integer = 827872836
        Public Shared DVX2 As Integer = 844650052
        Public Shared DVX3 As Integer = 861427268
        Public Shared DX50 As Integer = 808802372
        Public Shared DXGM As Integer = 1296521284
        Public Shared DXTC As Integer = 1129601092
        Public Shared DXTN As Integer = 1314150468
        Public Shared EKQ0 As Integer = 810634053
        Public Shared ELK0 As Integer = 810241093
        Public Shared EM2V As Integer = 1446137157
        Public Shared ES07 As Integer = 925913925
        Public Shared ESCP As Integer = 1346589509
        Public Shared ETV1 As Integer = 827741253
        Public Shared ETV2 As Integer = 844518469
        Public Shared ETVC As Integer = 1129731141
        Public Shared FFV1 As Integer = 827737670
        Public Shared FLJP As Integer = 1347046470
        Public Shared FMP4 As Integer = 877677894
        Public Shared FMVC As Integer = 1129729350
        Public Shared FPS1 As Integer = 827543622
        Public Shared FRWA As Integer = 1096241734
        Public Shared FRWD As Integer = 1146573382
        Public Shared FVF1 As Integer = 826693190
        Public Shared GEOX As Integer = 1481590087
        Public Shared GJPG As Integer = 1196444231
        Public Shared GLZW As Integer = 1465535559
        Public Shared GPEG As Integer = 1195724871
        Public Shared GWLT As Integer = 1414289223
        Public Shared H260 As Integer = 808858184
        Public Shared H261 As Integer = 825635400
        Public Shared H262 As Integer = 842412616
        Public Shared H263 As Integer = 859189832
        Public Shared H264 As Integer = 875967048
        Public Shared H265 As Integer = 892744264
        Public Shared H266 As Integer = 909521480
        Public Shared H267 As Integer = 926298696
        Public Shared H268 As Integer = 943075912
        Public Shared H269 As Integer = 959853128
        Public Shared HDYC As Integer = 1129923656
        Public Shared HEVC As Integer = 1129727304
        Public Shared HFYU As Integer = 1431914056
        Public Shared HMCR As Integer = 1380142408
        Public Shared HMRR As Integer = 1381125448
        Public Shared I263 As Integer = 859189833
        Public Shared ICLB As Integer = 1112294217
        Public Shared IGOR As Integer = 1380927305
        Public Shared IJPG As Integer = 1196444233
        Public Shared ILVC As Integer = 1129729097
        Public Shared ILVR As Integer = 1381387337
        Public Shared IPDV As Integer = 1447317577
        Public Shared IR21 As Integer = 825381449
        Public Shared IRAW As Integer = 1463898697
        Public Shared ISME As Integer = 1162695497
        Public Shared IV30 As Integer = 808670793
        Public Shared IV31 As Integer = 825448009
        Public Shared IV32 As Integer = 842225225
        Public Shared IV33 As Integer = 859002441
        Public Shared IV34 As Integer = 875779657
        Public Shared IV35 As Integer = 892556873
        Public Shared IV36 As Integer = 909334089
        Public Shared IV37 As Integer = 926111305
        Public Shared IV38 As Integer = 942888521
        Public Shared IV39 As Integer = 959665737
        Public Shared IV40 As Integer = 808736329
        Public Shared IV41 As Integer = 825513545
        Public Shared IV42 As Integer = 842290761
        Public Shared IV43 As Integer = 859067977
        Public Shared IV44 As Integer = 875845193
        Public Shared IV45 As Integer = 892622409
        Public Shared IV46 As Integer = 909399625
        Public Shared IV47 As Integer = 926176841
        Public Shared IV48 As Integer = 942954057
        Public Shared IV49 As Integer = 959731273
        Public Shared IV50 As Integer = 808801865
        Public Shared IYUV As Integer = 1448433993
        Public Shared JBYR As Integer = 1381581386
        Public Shared JPEG As Integer = 1195724874
        Public Shared JPGL As Integer = 1279742026
        Public Shared KMVC As Integer = 1129729355
        Public Shared L261 As Integer = 825635404
        Public Shared L263 As Integer = 859189836
        Public Shared LBYR As Integer = 1381581388
        Public Shared LCMW As Integer = 1464681292
        Public Shared LCW2 As Integer = 844579660
        Public Shared LEAD As Integer = 1145128268
        Public Shared LGRY As Integer = 1498564428
        Public Shared LJ11 As Integer = 825313868
        Public Shared LJ22 As Integer = 842156620
        Public Shared LJ2K As Integer = 1261587020
        Public Shared LJ44 As Integer = 875842124
        Public Shared LJPG As Integer = 1196444236
        Public Shared LMP2 As Integer = 844123468
        Public Shared LMP4 As Integer = 877677900
        Public Shared LSVC As Integer = 1129730892
        Public Shared LSVM As Integer = 1297503052
        Public Shared LSVX As Integer = 1482052428
        Public Shared LZO1 As Integer = 827284044
        Public Shared M261 As Integer = 825635405
        Public Shared M263 As Integer = 859189837
        Public Shared M4CC As Integer = 1128477773
        Public Shared M4S2 As Integer = 844313677
        Public Shared MC12 As Integer = 842089293
        Public Shared MCAM As Integer = 1296122701
        Public Shared MJ2C As Integer = 1127369293
        Public Shared MJPG As Integer = 1196444237
        Public Shared MMES As Integer = 1397050701
        Public Shared MP2A As Integer = 1093816397
        Public Shared MP2T As Integer = 1412583501
        Public Shared MP2V As Integer = 1446137933
        Public Shared MP42 As Integer = 842289229
        Public Shared MP43 As Integer = 859066445
        Public Shared MP4A As Integer = 1093947469
        Public Shared MP4S As Integer = 1395937357
        Public Shared MP4T As Integer = 1412714573
        Public Shared MP4V As Integer = 1446269005
        Public Shared MPEG As Integer = 1195724877
        Public Shared MPG4 As Integer = 877088845
        Public Shared MPGI As Integer = 1229410381
        Public Shared MR16 As Integer = 909201997
        Public Shared MRCA As Integer = 1094931021
        Public Shared MRLE As Integer = 1162629709
        Public Shared MSVC As Integer = 1129730893
        Public Shared MSZH As Integer = 1213879117
        Public Shared MTX1 As Integer = 827872333
        Public Shared MTX2 As Integer = 844649549
        Public Shared MTX3 As Integer = 861426765
        Public Shared MTX4 As Integer = 878203981
        Public Shared MTX5 As Integer = 894981197
        Public Shared MTX6 As Integer = 911758413
        Public Shared MTX7 As Integer = 928535629
        Public Shared MTX8 As Integer = 945312845
        Public Shared MTX9 As Integer = 962090061
        Public Shared MVI1 As Integer = 826889805
        Public Shared MVI2 As Integer = 843667021
        Public Shared MWV1 As Integer = 827742029
        Public Shared NAVI As Integer = 1230389582
        Public Shared NDSC As Integer = 1129530446
        Public Shared NDSM As Integer = 1297302606
        Public Shared NDSP As Integer = 1347634254
        Public Shared NDSS As Integer = 1397965902
        Public Shared NDXC As Integer = 1129858126
        Public Shared NDXH As Integer = 1213744206
        Public Shared NDXP As Integer = 1347961934
        Public Shared NDXS As Integer = 1398293582
        Public Shared NHVU As Integer = 1431717966
        Public Shared NTN1 As Integer = 827216974
        Public Shared NTN2 As Integer = 843994190
        Public Shared NVDS As Integer = 1396987470
        Public Shared NVHS As Integer = 1397249614
        Public Shared NVS0 As Integer = 810767950
        Public Shared NVS1 As Integer = 827545166
        Public Shared NVS2 As Integer = 844322382
        Public Shared NVS3 As Integer = 861099598
        Public Shared NVS4 As Integer = 877876814
        Public Shared NVS5 As Integer = 894654030
        Public Shared NVT0 As Integer = 810833486
        Public Shared NVT1 As Integer = 827610702
        Public Shared NVT2 As Integer = 844387918
        Public Shared NVT3 As Integer = 861165134
        Public Shared NVT4 As Integer = 877942350
        Public Shared NVT5 As Integer = 894719566
        Public Shared PDVC As Integer = 1129727056
        Public Shared PGVV As Integer = 1448494928
        Public Shared PHMO As Integer = 1330464848
        Public Shared PIM1 As Integer = 827148624
        Public Shared PIM2 As Integer = 843925840
        Public Shared PIMJ As Integer = 1246579024
        Public Shared PIXL As Integer = 1280854352
        Public Shared PJPG As Integer = 1196444240
        Public Shared PVEZ As Integer = 1514493520
        Public Shared PVMM As Integer = 1296914000
        Public Shared PVW2 As Integer = 844584528
        Public Shared QPEG As Integer = 1195724881
        Public Shared QPEQ As Integer = 1363497041
        Public Shared RGBT As Integer = 1413629778
        Public Shared RLE As Integer = 1162629727
        Public Shared RLE4 As Integer = 876956754
        Public Shared RLE8 As Integer = 944065618
        Public Shared RMP4 As Integer = 877677906
        Public Shared RPZA As Integer = 1096437842
        Public Shared RT21 As Integer = 825381970
        Public Shared RV20 As Integer = 808605266
        Public Shared RV30 As Integer = 808670802
        Public Shared RV40 As Integer = 808736338
        Public Shared S422 As Integer = 842150995
        Public Shared SAN3 As Integer = 860766547
        Public Shared SDCC As Integer = 1128481875
        Public Shared SEDG As Integer = 1195656531
        Public Shared SFMC As Integer = 1129137747
        Public Shared SMP4 As Integer = 877677907
        Public Shared SMSC As Integer = 1129532755
        Public Shared SMSD As Integer = 1146309971
        Public Shared SMSV As Integer = 1448299859
        Public Shared SP40 As Integer = 808734803
        Public Shared SP44 As Integer = 875843667
        Public Shared SP54 As Integer = 875909203
        Public Shared SPIG As Integer = 1195987027
        Public Shared SQZ2 As Integer = 844779859
        Public Shared STVA As Integer = 1096176723
        Public Shared STVB As Integer = 1112953939
        Public Shared STVC As Integer = 1129731155
        Public Shared STVX As Integer = 1482052691
        Public Shared STVY As Integer = 1498829907
        Public Shared SV10 As Integer = 808539731
        Public Shared SVQ1 As Integer = 827414099
        Public Shared SVQ3 As Integer = 860968531
        Public Shared TLMS As Integer = 1397574740
        Public Shared TLST As Integer = 1414745172
        Public Shared TM20 As Integer = 808602964
        Public Shared TM2X As Integer = 1479691604
        Public Shared TMIC As Integer = 1128877396
        Public Shared TMOT As Integer = 1414483284
        Public Shared TR20 As Integer = 808604244
        Public Shared TSCC As Integer = 1128485716
        Public Shared TV10 As Integer = 808539732
        Public Shared TVJP As Integer = 1347049044
        Public Shared TVMJ As Integer = 1246582356
        Public Shared TY0N As Integer = 1311791444
        Public Shared TY2C As Integer = 1127373140
        Public Shared TY2N As Integer = 1311922516
        Public Shared UCOD As Integer = 1146045269
        Public Shared ULTI As Integer = 1230261333
        Public Shared V210 As Integer = 808530518
        Public Shared V261 As Integer = 825635414
        Public Shared V655 As Integer = 892679766
        Public Shared VCR1 As Integer = 827474774
        Public Shared VCR2 As Integer = 844251990
        Public Shared VCR3 As Integer = 861029206
        Public Shared VCR4 As Integer = 877806422
        Public Shared VCR5 As Integer = 894583638
        Public Shared VCR6 As Integer = 911360854
        Public Shared VCR7 As Integer = 928138070
        Public Shared VCR8 As Integer = 944915286
        Public Shared VCR9 As Integer = 961692502
        Public Shared VDCT As Integer = 1413694550
        Public Shared VDOM As Integer = 1297040470
        Public Shared VDOW As Integer = 1464812630
        Public Shared VDTZ As Integer = 1515471958
        Public Shared VGPX As Integer = 1481656150
        Public Shared VIDS As Integer = 1396984150
        Public Shared VIFP As Integer = 1346783574
        Public Shared VIVO As Integer = 1331054934
        Public Shared VIXL As Integer = 1280854358
        Public Shared VLV1 As Integer = 827739222
        Public Shared VP30 As Integer = 808669270
        Public Shared VP31 As Integer = 825446486
        Public Shared VP40 As Integer = 808734806
        Public Shared VP50 As Integer = 808800342
        Public Shared VP60 As Integer = 808865878
        Public Shared VP61 As Integer = 825643094
        Public Shared VP62 As Integer = 842420310
        Public Shared VP70 As Integer = 808931414
        Public Shared VP80 As Integer = 808996950
        Public Shared VQC1 As Integer = 826495318
        Public Shared VQC2 As Integer = 843272534
        Public Shared VQJC As Integer = 1128943958
        Public Shared VSSV As Integer = 1448301398
        Public Shared VUUU As Integer = 1431655766
        Public Shared VX1K As Integer = 1261525078
        Public Shared VX2K As Integer = 1261590614
        Public Shared VXSP As Integer = 1347639382
        Public Shared VYU9 As Integer = 961894742
        Public Shared VYUY As Integer = 1498765654
        Public Shared WBVC As Integer = 1129726551
        Public Shared WHAM As Integer = 1296123991
        Public Shared WINX As Integer = 1481525591
        Public Shared WJPG As Integer = 1196444247
        Public Shared WMV1 As Integer = 827739479
        Public Shared WMV2 As Integer = 844516695
        Public Shared WMV3 As Integer = 861293911
        Public Shared WMVA As Integer = 1096174935
        Public Shared WNV1 As Integer = 827739735
        Public Shared WVC1 As Integer = 826496599
        Public Shared X263 As Integer = 859189848
        Public Shared X264 As Integer = 875967064
        Public Shared XLV0 As Integer = 810962008
        Public Shared XMPG As Integer = 1196445016
        Public Shared XVID As Integer = 1145656920
        Public Shared XWV0 As Integer = 810964824
        Public Shared XWV1 As Integer = 827742040
        Public Shared XWV2 As Integer = 844519256
        Public Shared XWV3 As Integer = 861296472
        Public Shared XWV4 As Integer = 878073688
        Public Shared XWV5 As Integer = 894850904
        Public Shared XWV6 As Integer = 911628120
        Public Shared XWV7 As Integer = 928405336
        Public Shared XWV8 As Integer = 945182552
        Public Shared XWV9 As Integer = 961959768
        Public Shared XXAN As Integer = 1312905304
        Public Shared Y16 As Integer = 909203807
        Public Shared Y411 As Integer = 825308249
        Public Shared Y41P As Integer = 1345401945
        Public Shared Y444 As Integer = 875836505
        Public Shared Y8 As Integer = 945381215
        Public Shared YC12 As Integer = 842089305
        Public Shared YUV8 As Integer = 945182041
        Public Shared YUV9 As Integer = 961959257
        Public Shared YUVP As Integer = 1347835225
        Public Shared YUY2 As Integer = 844715353
        Public Shared YUYV As Integer = 1448695129
        Public Shared YV12 As Integer = 842094169
        Public Shared YV16 As Integer = 909203033
        Public Shared YV92 As Integer = 842618457
        Public Shared ZLIB As Integer = 1112099930
        Public Shared ZMBV As Integer = 1447185754
        Public Shared ZPEG As Integer = 1195724890
        Public Shared ZYGO As Integer = 1330075994
        Public Shared ZYYY As Integer = 1499027802
    End Class
    <DataContract()> Public Class ClipData
        Implements INotifyPropertyChanged, IEditableObject

        <DataMember> Public m_Description As String
        <DataMember> Public m_Name As String
        <DataMember> Public m_ColourFrames As Integer
        <DataMember> Public m_DepthFrames As Integer
        <DataMember> Public m_ProcessingFrames As Integer
        <DataMember> Public m_ClipEntrance As Integer
        <DataMember> Public m_ClipSeated As Integer
        <DataMember> Public m_PreStart As PreStartEnum
        <DataMember> Public m_IgnoreDepth As Boolean
        <DataMember> Public m_AltMotion As Boolean
        <DataMember> Public m_ProcessedVideo As Boolean
        <DataMember> Public m_ProcessedGroundTruth As Boolean
        <DataMember> Public m_ProcessedCut As Boolean
        <DataMember> Public m_ProcessedSegmented As Boolean
        <DataMember> Public m_Accuracy As Dictionary(Of SegTypeEnum, SplitResults)
        Private inTxn As Boolean
        Private m_DescriptionB As String
        Private m_NameB As String

        Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged
        Protected Sub OnPropertyChanged(ByVal sName As String)
            RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(sName))
        End Sub
        Sub New()
            m_Description = String.Empty
            m_Name = String.Empty
            m_ColourFrames = 0
            m_DepthFrames = 0
            m_ProcessingFrames = 0
            m_ClipEntrance = 0
            m_ClipSeated = 0
            m_PreStart = PreStartEnum.Ten
            m_IgnoreDepth = False
            m_AltMotion = False
            m_ProcessedVideo = False
            m_ProcessedGroundTruth = False
            m_ProcessedCut = False
            m_ProcessedSegmented = False
            SetAccuracy()
            inTxn = False
            m_DescriptionB = String.Empty
            m_NameB = String.Empty
        End Sub
        Sub BeginEdit() Implements IEditableObject.BeginEdit
            If Not inTxn Then
                m_DescriptionB = m_Description
                m_NameB = m_Name
                inTxn = True
            End If
        End Sub
        Sub CancelEdit() Implements IEditableObject.CancelEdit
            If inTxn Then
                m_Description = m_DescriptionB
                m_Name = m_NameB
                m_DescriptionB = String.Empty
                m_NameB = String.Empty
                inTxn = False
            End If
        End Sub
        Sub EndEdit() Implements IEditableObject.EndEdit
            If inTxn Then
                m_DescriptionB = String.Empty
                m_NameB = String.Empty
                inTxn = False
            End If
        End Sub
        Public Property Description As String
            Get
                Return m_Description
            End Get
            Set(value As String)
                m_Description = value
                OnPropertyChanged("Description")
            End Set
        End Property
        Public Property Name As String
            Get
                Return m_Name
            End Get
            Set(value As String)
                m_Name = value
                OnPropertyChanged("Name")
            End Set
        End Property
        Public Property ColourFrames As Integer
            Get
                Return m_ColourFrames
            End Get
            Set(value As Integer)
                m_ColourFrames = value
                OnPropertyChanged("ColourFrames")
            End Set
        End Property
        Public Property DepthFrames As Integer
            Get
                Return m_DepthFrames
            End Get
            Set(value As Integer)
                m_DepthFrames = value
                OnPropertyChanged("DepthFrames")
            End Set
        End Property
        Public Property ProcessingFrames As Integer
            Get
                Return m_ProcessingFrames
            End Get
            Set(value As Integer)
                m_ProcessingFrames = value
                OnPropertyChanged("ProcessingFrames")
            End Set
        End Property
        Public Property ClipEntrance As Integer
            Get
                Return m_ClipEntrance
            End Get
            Set(value As Integer)
                m_ClipEntrance = value
                OnPropertyChanged("ClipEntrance")
            End Set
        End Property
        Public Property ClipSeated As Integer
            Get
                Return m_ClipSeated
            End Get
            Set(value As Integer)
                m_ClipSeated = value
                OnPropertyChanged("ClipSeated")
            End Set
        End Property
        Public Property PreStart As PreStartEnum
            Get
                Return m_PreStart
            End Get
            Set(value As PreStartEnum)
                m_PreStart = value
                OnPropertyChanged("PreStart")
            End Set
        End Property
        Public Property IgnoreDepth As Boolean
            Get
                Return m_IgnoreDepth
            End Get
            Set(value As Boolean)
                m_IgnoreDepth = value
                OnPropertyChanged("IgnoreDepth")
            End Set
        End Property
        Public Property AltMotion As Boolean
            Get
                Return m_AltMotion
            End Get
            Set(value As Boolean)
                m_AltMotion = value
                OnPropertyChanged("AltMotion")
            End Set
        End Property
        Public Property ProcessedVideo As Boolean
            Get
                Return m_ProcessedVideo
            End Get
            Set(value As Boolean)
                m_ProcessedVideo = value
                OnPropertyChanged("ProcessedVideo")
            End Set
        End Property
        Public Property ProcessedGroundTruth As Boolean
            Get
                Return m_ProcessedGroundTruth
            End Get
            Set(value As Boolean)
                m_ProcessedGroundTruth = value
                OnPropertyChanged("ProcessedGroundTruth")
            End Set
        End Property
        Public Property ProcessedCut As Boolean
            Get
                Return m_ProcessedCut
            End Get
            Set(value As Boolean)
                m_ProcessedCut = value
                OnPropertyChanged("ProcessedCut")
            End Set
        End Property
        Public Property ProcessedSegmented As Boolean
            Get
                Return m_ProcessedSegmented
            End Get
            Set(value As Boolean)
                m_ProcessedSegmented = value
                OnPropertyChanged("ProcessedSegmented")
            End Set
        End Property
        Public ReadOnly Property ProcessedAccuracy As Boolean
            Get
                Dim iEmptyAlgorithms As Integer = Aggregate oAccuracy In m_Accuracy Where Not ExcludedSegTypes.Contains(oAccuracy.Key) Into Count(oAccuracy.Value.Empty)
                Dim bAllAlgorithms As Boolean = (m_Accuracy.Count = [Enum].GetNames(GetType(SegTypeEnum)).Count)
                Return (iEmptyAlgorithms = 0) AndAlso bAllAlgorithms
            End Get
        End Property
        Public Property Accuracy As Dictionary(Of SegTypeEnum, SplitResults)
            Get
                Return m_Accuracy
            End Get
            Set(value As Dictionary(Of SegTypeEnum, SplitResults))
                m_Accuracy = value
                OnPropertyChanged("Accuracy")
                OnPropertyChanged("ProcessedAccuracy")
            End Set
        End Property
        Public Sub SetAccuracy()
            ' sets the dictionary
            If IsNothing(m_Accuracy) Then
                m_Accuracy = New Dictionary(Of SegTypeEnum, SplitResults)
            End If
            Dim oSegEnumList As List(Of SegTypeEnum) = [Enum].GetValues(GetType(SegTypeEnum)).Cast(Of SegTypeEnum).ToList
            For Each oSegType In oSegEnumList
                If Not m_Accuracy.ContainsKey(oSegType) Then
                    m_Accuracy.Add(oSegType, SplitResults.GetEmpty)
                End If
            Next
        End Sub
        <DataContract()> Public Class SplitResults
            <DataMember> Public Results As List(Of SplitResult)
            <DataMember> Public ProcessTime As TimeSpan

            Sub New(ByVal oProcessTime As TimeSpan)
                Results = New List(Of SplitResult)
                ProcessTime = oProcessTime
            End Sub
            Public ReadOnly Property TP As Integer
                Get
                    Return Results.Sum(Function(x) x.TP)
                End Get
            End Property
            Public ReadOnly Property TN As Integer
                Get
                    Return Results.Sum(Function(x) x.TN)
                End Get
            End Property
            Public ReadOnly Property FP As Integer
                Get
                    Return Results.Sum(Function(x) x.FP)
                End Get
            End Property
            Public ReadOnly Property FN As Integer
                Get
                    Return Results.Sum(Function(x) x.FN)
                End Get
            End Property
            Public ReadOnly Property ProcessFrames As Integer
                Get
                    Return Results.Count
                End Get
            End Property
            Public ReadOnly Property Empty
                Get
                    Return Results.Count = 0
                End Get
            End Property
            Public Shared Function GetEmpty()
                Return New SplitResults(TimeSpan.MinValue)
            End Function
            <DataContract()> Public Structure SplitResult
                <DataMember> Public TP As Integer
                <DataMember> Public TN As Integer
                <DataMember> Public FP As Integer
                <DataMember> Public FN As Integer

                Sub New(ByVal iTP As Integer, ByVal iTN As Integer, ByVal iFP As Integer, ByVal iFN As Integer)
                    TP = iTP
                    TN = iTN
                    FP = iFP
                    FN = iFN
                End Sub
                Public Shared Function GetKnownTypes() As List(Of Type)
                    ' returns the list of additonal types
                    Return New List(Of Type) From {}
                End Function
            End Structure
            Public Shared Function GetKnownTypes() As List(Of Type)
                ' returns the list of additonal types
                Return New List(Of Type) From {GetType(TimeSpan), GetType(SplitResult)}
            End Function
        End Class
#Region "Enums"
        Public Enum SegTypeEnum As Int32
            <Description("None")> None = 0
            <Description("Frame Difference")> FrameDifference = 1
            <Description("Static Frame Difference")> StaticFrameDifference = 2
            <Description("Weighted Moving Mean")> WeightedMovingMean = 3
            <Description("Weighted Moving Variance")> WeightedMovingVariance = 4
            <Description("GMM KaewTraKulPong")> MixtureOfGaussianV1 = 5
            <Description("GMM Zivkovic 1")> MixtureOfGaussianV2 = 6
            <Description("Adaptive Background Learning")> AdaptiveBackgroundLearning = 7
            <Description("Adaptive-Selective Background Learning")> AdaptiveSelectiveBackgroundLearning = 8
            <Description("KNN Background Subtractor")> KNN = 9
            <Description("GMG")> GMG = 10
            <Description("Adaptive Median")> DPAdaptiveMedian = 11
            <Description("GMM Stauffer")> DPGrimsonGMM = 12
            <Description("GMM Zivkovic 2")> DPZivkovicAGMM = 13
            <Description("Temporal Mean")> DPMean = 14
            <Description("Gaussian Average")> DPWrenGA = 15
            <Description("Temporal Median")> DPPratiMediod = 16
            <Description("Eigenbackground SL-PCA")> DPEigenbackground = 17
            <Description("Texture BGS")> DPTexture = 18
            <Description("Type-2 Fuzzy GMM-UM")> T2FGMM_UM = 19
            <Description("Type-2 Fuzzy GMM-UV")> T2FGMM_UV = 20
            <Description("Type-2 Fuzzy GMM-UM with MRF")> T2FMRF_UM = 21
            <Description("Type-2 Fuzzy GMM-UV with MRF")> T2FMRF_UV = 22
            <Description("Fuzzy Sugeno Integral")> FuzzySugenoIntegral = 23
            <Description("Fuzzy Choquet Integral")> FuzzyChoquetIntegral = 24
            <Description("Simple Gaussian")> LBSimpleGaussian = 25
            <Description("Fuzzy Gaussian")> LBFuzzyGaussian = 26
            <Description("GMM Bender")> LBMixtureOfGaussians = 27
            <Description("Adaptive SOM")> LBAdaptiveSOM = 28
            <Description("Fuzzy Adaptive SOM")> LBFuzzyAdaptiveSOM = 29
            <Description("Texture-Based Foreground Detection with MRF")> LBP_MRF = 30
            <Description("Multi-Layer BGS")> MultiLayer = 31
            <Description("Pixel-Based Adaptive Segmenter")> PixelBasedAdaptiveSegmenter = 32
            <Description("VuMeter")> VuMeter = 33
            <Description("KDE")> KDE = 34
            <Description("IMBS")> IndependentMultimodal = 35
            <Description("MultiCue BGS")> MultiCue = 36
            <Description("Sigma Delta")> SigmaDelta = 37
            <Description("SuBSENSE")> SuBSENSE = 38
            <Description("LOBSTER")> LOBSTER = 39
            <Description("PAWCS")> PAWCS = 40
            <Description("TwoPoints")> TwoPoints = 41
            <Description("ViBe")> ViBe = 42
            <Description("Codebook")> CodeBook = 43
        End Enum
#End Region
        Public Shared Function GetKnownTypes() As List(Of Type)
            ' returns the list of additonal types
            Return New List(Of Type) From {GetType(PreStartEnum), GetType(SegTypeEnum), GetType(SplitResults), GetType(Dictionary(Of SegTypeEnum, SplitResults))}
        End Function
    End Class
    <CollectionDataContract> Public Class TrueObservableCollection(Of T)
        Inherits ObservableCollection(Of T)

        Private m_BoundElement As FrameworkElement
        Private m_Update As Boolean
        Public AsyncInvoke As Boolean

        Public Sub New(Optional ByVal bAsyncInvoke As Boolean = False)
            AsyncInvoke = bAsyncInvoke
            m_Update = True
        End Sub
        Public Sub New(ByVal oCollection As IEnumerable(Of T), Optional ByVal bAsyncInvoke As Boolean = False)
            MyBase.New(oCollection)
            AsyncInvoke = bAsyncInvoke
            m_Update = True
        End Sub
        Public Sub New(ByVal oList As List(Of T), Optional ByVal bAsyncInvoke As Boolean = False)
            MyBase.New(oList)
            AsyncInvoke = bAsyncInvoke
            m_Update = True
        End Sub
        Public Sub SetBoundElement(ByRef oBoundElement As FrameworkElement)
            m_BoundElement = oBoundElement
            If IsNothing(m_BoundElement.BindingGroup) Then
                m_BoundElement.BindingGroup = New BindingGroup
            End If
        End Sub
        Public Sub AddRange(ByVal oRange As IEnumerable(Of T))
            For Each oItem In oRange
                Items.Add(oItem)
            Next
            If m_Update Then
                OnCollectionChanged(New NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset))
            End If
        End Sub
        Public Sub Reset(ByVal range As IEnumerable(Of T))
            Items.Clear()
            AddRange(range)
        End Sub
        Public Overloads Sub Clear()
            ClearItems()
        End Sub
        Protected Overrides Sub ClearItems()
            If m_Update Then
                MyBase.ClearItems()
            Else
                Items.Clear()
            End If
        End Sub
        Protected Overrides Sub SetItem(index As Integer, item As T)
            If m_Update Then
                MyBase.SetItem(index, item)
            Else
                Items(index) = item
            End If
        End Sub
        Protected Overrides Sub OnPropertyChanged(e As PropertyChangedEventArgs)
            If IsNothing(m_BoundElement) OrElse m_BoundElement.Dispatcher.CheckAccess Then
                MyBase.OnPropertyChanged(e)
            Else
                CommonFunctions.SafeInvoke(Sub() MyBase.OnPropertyChanged(e), , AsyncInvoke)
            End If
        End Sub
        Protected Overrides Sub OnCollectionChanged(e As NotifyCollectionChangedEventArgs)
            If IsNothing(m_BoundElement) OrElse m_BoundElement.Dispatcher.CheckAccess Then
                MyBase.OnCollectionChanged(e)
            Else
                CommonFunctions.SafeInvoke(Sub() MyBase.OnCollectionChanged(e), , AsyncInvoke)
            End If
        End Sub
        Public Class SuspendUpdates
            Implements IDisposable

            Private m_Collection As TrueObservableCollection(Of T)
            Private m_OldUpdate As Boolean
            Sub New(ByRef oCollection As TrueObservableCollection(Of T))
                m_Collection = oCollection
                m_OldUpdate = m_Collection.m_Update
                m_Collection.m_Update = False
            End Sub
#Region "IDisposable Support"
            Private disposedValue As Boolean
            Protected Overridable Sub Dispose(disposing As Boolean)
                If Not disposedValue Then
                    If disposing Then
                        m_Collection.m_Update = m_OldUpdate
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
            Public Sub Dispose() Implements IDisposable.Dispose
                ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
                Dispose(True)
                ' TODO: uncomment the following line if Finalize() is overridden above.
                ' GC.SuppressFinalize(Me)
            End Sub
#End Region
        End Class
    End Class
#End Region
    Public Class CommonFunctions
#Region "Serialisation"
        Public Shared Function GetKnownTypes(Optional ByVal oTypes As List(Of Type) = Nothing) As List(Of Type)
            ' gets list of known types
            Dim oKnownTypes As New List(Of Type)

            If Not oTypes Is Nothing Then
                oKnownTypes.AddRange(oTypes)
            End If
            Return oKnownTypes.Distinct.ToList
        End Function
        Public Shared Sub SerializeDataContractStream(Of T)(ByRef oStream As IO.Stream, ByVal data As T, Optional ByVal oAdditionalTypes As List(Of Type) = Nothing, Optional ByVal bUseKnownTypes As Boolean = True)
            ' serialise to stream
            Dim oKnownTypes As New List(Of Type)
            If bUseKnownTypes Then
                oKnownTypes.AddRange(GetKnownTypes)
            End If
            If Not oAdditionalTypes Is Nothing Then
                oKnownTypes.AddRange(oAdditionalTypes)
            End If

            Dim oDataContractSerializer As New DataContractSerializer(GetType(T), oKnownTypes)
            oDataContractSerializer.WriteObject(oStream, data)
        End Sub
        Public Shared Function DeserializeDataContractStream(Of T)(ByRef oStream As IO.Stream, Optional ByVal oAdditionalTypes As List(Of Type) = Nothing, Optional ByVal bUseKnownTypes As Boolean = True) As T
            ' deserialise from stream
            Dim oXmlDictionaryReaderQuotas As New Xml.XmlDictionaryReaderQuotas With {.MaxArrayLength = 100000000, .MaxStringContentLength = 100000000}
            Dim oXmlDictionaryReader As Xml.XmlDictionaryReader = Xml.XmlDictionaryReader.CreateTextReader(oStream, oXmlDictionaryReaderQuotas)

            Dim theObject As T = Nothing
            Try
                Dim oKnownTypes As New List(Of Type)
                If bUseKnownTypes Then
                    oKnownTypes.AddRange(GetKnownTypes)
                End If
                If Not oAdditionalTypes Is Nothing Then
                    oKnownTypes.AddRange(oAdditionalTypes)
                End If

                Dim oDataContractSerializer As New DataContractSerializer(GetType(T), oKnownTypes)
                theObject = oDataContractSerializer.ReadObject(oXmlDictionaryReader, True)
            Catch ex As SerializationException
            End Try

            oXmlDictionaryReader.Close()
            Return theObject
        End Function
        Public Shared Sub SerializeDataContractFile(Of T)(ByVal sFilePath As String, ByVal data As T, Optional ByVal oAdditionalTypes As List(Of Type) = Nothing, Optional ByVal bUseKnownTypes As Boolean = True, Optional ByVal sExtension As String = "", Optional ByVal bCompress As Boolean = True)
            ' serialise using data contract serialiser
            ' compress using gzip
            ' create directory if necessary
            Dim oFileInfo As New IO.FileInfo(sFilePath)
            Dim oDirectoryInfo As IO.DirectoryInfo = oFileInfo.Directory
            If Not oDirectoryInfo.Exists Then
                oDirectoryInfo.Create()
            End If

            If bCompress Then
                Using oFileStream As IO.FileStream = IO.File.Create(If(sExtension = String.Empty, sFilePath, ReplaceExtension(sFilePath, If(sExtension = String.Empty, "gz", sExtension))))
                    Using oGZipStream As New IO.Compression.GZipStream(oFileStream, IO.Compression.CompressionMode.Compress)
                        SerializeDataContractStream(oGZipStream, data, oAdditionalTypes, bUseKnownTypes)
                    End Using
                End Using
            Else
                Using oFileStream As IO.FileStream = IO.File.Create(If(sExtension = String.Empty, sFilePath, ReplaceExtension(sFilePath, If(sExtension = String.Empty, "xml", sExtension))))
                    SerializeDataContractStream(oFileStream, data, oAdditionalTypes, bUseKnownTypes)
                End Using
            End If
        End Sub
        Public Shared Function DeserializeDataContractFile(Of T)(ByVal sFilePath As String, Optional ByVal oAdditionalTypes As List(Of Type) = Nothing, Optional ByVal bUseKnownTypes As Boolean = True, Optional ByVal sExtension As String = "", Optional ByVal bDecompress As Boolean = True) As T
            ' deserialise using data contract serialiser
            If bDecompress Then
                Using oFileStream As IO.FileStream = IO.File.OpenRead(If(sExtension = String.Empty, sFilePath, ReplaceExtension(sFilePath, If(sExtension = String.Empty, "gz", sExtension))))
                    Using oGZipStream As New IO.Compression.GZipStream(oFileStream, IO.Compression.CompressionMode.Decompress)
                        Return DeserializeDataContractStream(Of T)(oGZipStream, oAdditionalTypes, bUseKnownTypes)
                    End Using
                End Using
            Else
                Using oFileStream As IO.FileStream = IO.File.OpenRead(If(sExtension = String.Empty, sFilePath, ReplaceExtension(sFilePath, If(sExtension = String.Empty, "xml", sExtension))))
                    Return DeserializeDataContractStream(Of T)(oFileStream, oAdditionalTypes, bUseKnownTypes)
                End Using
            End If
        End Function
#End Region
#Region "Utility"
        Public Shared Sub ProtectedRunTasks(ByVal oActions As List(Of Tuple(Of Action(Of Object), Object)), Optional ByVal bSTA As Boolean = False)
            ' protects tasks from memory exceptions by clearing memory if an error occurs
            Dim oActionDictionary As New Dictionary(Of Guid, Tuple(Of Action(Of Object), Object))
            For Each oAction As Tuple(Of Action(Of Object), Object) In oActions
                oActionDictionary.Add(Guid.NewGuid, oAction)
            Next

            Do Until oActionDictionary.Count = 0
                ' create task dictionary from actions
                Dim oTaskDictionary As New ConcurrentDictionary(Of Guid, Task)
                For Each oGUID As Guid In oActionDictionary.Keys
                    If bSTA Then
                        oTaskDictionary.TryAdd(oGUID, StartSTATask(oActionDictionary(oGUID).Item1, oActionDictionary(oGUID).Item2))
                    Else
                        oTaskDictionary.TryAdd(oGUID, New Task(oActionDictionary(oGUID).Item1, oActionDictionary(oGUID).Item2))
                    End If
                Next

                Try
                    If Not bSTA Then
                        For Each oTask In oTaskDictionary.Values
                            oTask.Start()
                        Next
                    End If
                    Task.WaitAll(oTaskDictionary.Values.ToArray)
                Catch ae As AggregateException
                    Throw New ArgumentException(ae.Message)
                End Try

                For Each oGUID In oTaskDictionary.Keys
                    If oTaskDictionary(oGUID).IsCompleted Then
                        oActionDictionary.Remove(oGUID)
                    ElseIf oTaskDictionary(oGUID).IsFaulted Then
                        For Each ex In oTaskDictionary(oGUID).Exception.Flatten.InnerExceptions
                            If TypeOf ex Is OutOfMemoryException Then
                                ClearMemory()
                            Else
                                Throw ex
                            End If
                        Next
                    End If
                Next

                oTaskDictionary.Clear()
            Loop
        End Sub
        Public Shared Function StartSTATask(ByVal oAction As Action(Of Object), state As Object) As Task
            Dim oTCS As New TaskCompletionSource(Of Object)()
            Dim oThread = New Threading.Thread(Sub()
                                                   Try
                                                       oAction.Invoke(state)
                                                       oTCS.SetResult(Nothing)
                                                   Catch e As Exception
                                                       oTCS.SetException(e)
                                                   End Try
                                               End Sub)
            oThread.SetApartmentState(Threading.ApartmentState.STA)
            oThread.Start()
            Return oTCS.Task
        End Function
        Public Shared Sub ClearMemory()
            ' clear up memory
            Runtime.GCSettings.LargeObjectHeapCompactionMode = Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
            GC.Collect(2, GCCollectionMode.Forced, True, True)
            GC.WaitForPendingFinalizers()
        End Sub
        Public Shared Async Sub SafeInvoke(ByVal TaskDelegate As Action, Optional ByVal oDispatcher As Threading.Dispatcher = Nothing, Optional ByVal bAsyncInvoke As Boolean = False)
            Try
                If oDispatcher Is Nothing Then
                    Dim oApplication As Windows.Application = Windows.Application.Current
                    If Not IsNothing(oApplication) Then
                        If bAsyncInvoke Then
                            Await oApplication.Dispatcher.BeginInvoke(TaskDelegate, Threading.DispatcherPriority.Render)
                        Else
                            oApplication.Dispatcher.Invoke(TaskDelegate, Threading.DispatcherPriority.Render)
                        End If
                    End If
                Else
                    If bAsyncInvoke Then
                        Await oDispatcher.BeginInvoke(TaskDelegate, Threading.DispatcherPriority.Render)
                    Else
                        oDispatcher.Invoke(TaskDelegate, Threading.DispatcherPriority.Render)
                    End If
                End If
            Catch ex As TaskCanceledException
                ' tasks may be cancelled when the module is exited
            End Try
        End Sub
        Public Shared Function MatrixIsNothing(Of T As Structure)(ByVal oMatrix As Matrix(Of T)) As Boolean
            ' checks if the matrix is nothing or disposed
            Return IsNothing(oMatrix) OrElse oMatrix.Ptr.Equals(IntPtr.Zero)
        End Function
        Public Shared Function MatrixNotNothing(Of T As Structure)(ByVal oMatrix As Matrix(Of T)) As Boolean
            ' checks if the matrix is not nothing and not disposed
            Return (Not IsNothing(oMatrix)) AndAlso (Not oMatrix.Ptr.Equals(IntPtr.Zero))
        End Function
#End Region
#Region "I/O"
        Public Shared Function ReplaceExtension(ByVal sFilePath As String, ByVal sExtension As String) As String
            Try
                Return IO.Path.ChangeExtension(sFilePath, sExtension)
            Catch ex As Exception
                Return String.Empty
            End Try
        End Function
        Public Shared Function SafeFileName(ByVal sFileName As String, Optional ByVal sReplaceChar As String = "", Optional ByVal sRemoveChar As String = "") As String
            ' removes unsafe characters from a file name
            Dim sReturnFileName As String = sFileName
            sReturnFileName = System.Text.RegularExpressions.Regex.Replace(sReturnFileName, "[ \\/:*?""<>|\r\n]", String.Empty)

            If sRemoveChar <> String.Empty Then
                Dim sRemoveChars As Char() = sRemoveChar.ToCharArray
                For Each sChar In sRemoveChars
                    sReturnFileName = sReturnFileName.Replace(sChar.ToString, String.Empty)
                Next
            End If

            Dim sInvalidFileNameChars As Char() = IO.Path.GetInvalidFileNameChars
            For Each sChar In sInvalidFileNameChars
                sReturnFileName = sReturnFileName.Replace(sChar.ToString, sReplaceChar)
            Next
            Return sReturnFileName
        End Function
        Public Shared Sub SaveMatrix(ByVal sFileName As String, ByVal oMatrix As Matrix(Of Byte), Optional DPI As Single = Resolution096)
            ' save matrix to TIFF file
            If Not IsNothing(oMatrix) Then
                Dim oBitmapSource As BitmapSource = Converter.MatrixToBitmapSource(oMatrix, DPI)
                Try
                    Using oFileStream As New IO.FileStream(sFileName, IO.FileMode.Create)
                        Dim oTiffBitmapEncoder As New TiffBitmapEncoder()
                        oTiffBitmapEncoder.Compression = TiffCompressOption.Zip
                        oTiffBitmapEncoder.Frames.Add(BitmapFrame.Create(oBitmapSource))
                        oTiffBitmapEncoder.Save(oFileStream)
                        oFileStream.Flush()
                    End Using
                Catch ex As IO.IOException
                End Try
            End If
        End Sub
#End Region
#Region "UI"
        Public Shared Function GetIcon(ByVal sIconName As String) As ImageSource
            If CommonIcons.ContainsKey(sIconName) Then
                Return CommonIcons(sIconName)
            Else
                Return Nothing
            End If
        End Function
        Public Shared Sub ClickCheck(sender As Object, e As RoutedEventArgs, ByVal oAction As Action)
            ' ignore stylus and touch events
            If (Not e.RoutedEvent.Equals(Stylus.StylusDownEvent)) AndAlso (Not e.RoutedEvent.Equals(UIElement.TouchDownEvent)) Then
                oAction.Invoke()
            End If
        End Sub
#End Region
#Region "Drawing"
        Public Shared Function GetRotationMatrix(ByVal fAngle As Double, ByVal oSize As System.Drawing.SizeF, ByVal bDisplace As Boolean, ByVal bKeepOriginalSize As Boolean, ByRef oDestSize As System.Drawing.Size, Optional ByVal oCenter As System.Drawing.PointF = Nothing) As Matrix(Of Double)
            ' get the rotation matrix
            Dim fHeight As Double = oSize.Height
            Dim fWidth As Double = oSize.Width
            If oCenter.IsEmpty Then
                oCenter = New System.Drawing.PointF(fWidth / 2, fHeight / 2)
            End If

            Dim oRotationMatrix As New Matrix(Of Double)(2, 3)
            CvInvoke.GetRotationMatrix2D(oCenter, fAngle, 1, oRotationMatrix)

            Dim fCos As Double = Math.Abs(oRotationMatrix(0, 0))
            Dim fSin As Double = Math.Abs(oRotationMatrix(0, 1))

            If bKeepOriginalSize Then
                oDestSize = New System.Drawing.Size(oSize.Width, oSize.Height)
            Else
                oDestSize = New System.Drawing.Size(Math.Ceiling((fHeight * fSin) + (fWidth * fCos)), Math.Ceiling((fHeight * fCos) + (fWidth * fSin)))
            End If

            If bDisplace Then
                oRotationMatrix(0, 2) += (oDestSize.Width / 2) - oCenter.X
                oRotationMatrix(1, 2) += (oDestSize.Height / 2) - oCenter.Y
            End If

            Return oRotationMatrix
        End Function
        Public Shared Sub TransformPoints(ByRef oPoints As System.Drawing.PointF(), ByVal oRotationMatrix As Matrix(Of Double))
            ' transforms points using an affine transform
            Using oVector As New Util.VectorOfPointF(oPoints)
                CvInvoke.Transform(oVector, oVector, oRotationMatrix)
                oPoints = oVector.ToArray
            End Using
        End Sub
        Public Shared Function RotateMatrix(Of T As Structure)(ByVal oMatrix As Matrix(Of T), ByVal oFillValue As [Structure].MCvScalar, ByVal oDestSize As System.Drawing.Size, ByVal oRotationMatrix As Matrix(Of Double)) As Matrix(Of T)
            ' rotates a matrix
            If (Not IsNothing(oMatrix)) AndAlso (Not oMatrix.Ptr.Equals(IntPtr.Zero)) Then
                Dim oMatrixRotated As New Matrix(Of T)(oDestSize.Height, oDestSize.Width, oMatrix.NumberOfChannels)
                CvInvoke.WarpAffine(oMatrix, oMatrixRotated, oRotationMatrix, oMatrixRotated.Size, CvEnum.Inter.Cubic, CvEnum.Warp.FillOutliers, CvEnum.BorderType.Constant, oFillValue)
                Return oMatrixRotated
            Else
                Return Nothing
            End If
        End Function
#End Region
    End Class
    Public Class Converter
#Region "Bitmaps"
        Public Shared Function MatrixToBitmapSource(ByVal oMatrix As Matrix(Of Byte), Optional DPI As Single = Resolution096) As BitmapSource
            ' converts matrix data to an array which takes into account the bitmap stride
            If IsNothing(oMatrix) Then
                Return Nothing
            Else
                Using oColourMatrix As New Matrix(Of Byte)(oMatrix.Height, oMatrix.Width, 4)
                    Select Case oMatrix.NumberOfChannels
                        Case 4
                            oMatrix.CopyTo(oColourMatrix)
                        Case 3
                            CvInvoke.CvtColor(oMatrix, oColourMatrix, CvEnum.ColorConversion.Bgr2Bgra)
                        Case 1
                            CvInvoke.CvtColor(oMatrix, oColourMatrix, CvEnum.ColorConversion.Gray2Bgra)
                        Case Else
                            Throw New ArgumentException("Converter:MatrixToBitmapSource: Number of channels must be 1, 3, or 4")
                    End Select

                    Dim width As Integer = oColourMatrix.Cols
                    Dim height As Integer = oColourMatrix.Rows
                    Dim iBytesPerPixel As Integer = 4
                    Dim iStride As Integer = width * iBytesPerPixel

                    Dim oWriteableBitmap As New WriteableBitmap(width, height, DPI, DPI, PixelFormats.Bgra32, Nothing)
                    oWriteableBitmap.WritePixels(New Int32Rect(0, 0, width, height), oColourMatrix.Bytes, iStride, 0, 0)
                    Return oWriteableBitmap
                End Using
            End If
        End Function
        Public Shared Function MatrixToJpgBytes(ByVal oMatrix As Matrix(Of Byte), ByVal iQuality As Integer, ByVal oCompressor As TurboJpegWrapper.TJCompressor) As Byte()
            ' converts a matrix to a jpeg encoded in bytes
            Return oCompressor.Compress(oMatrix.Bytes, oMatrix.Mat.Step, oMatrix.Width, oMatrix.Height, TurboJpegWrapper.TJPixelFormats.TJPF_BGR, TurboJpegWrapper.TJSubsamplingOptions.TJSAMP_420, iQuality, TurboJpegWrapper.TJFlags.FASTDCT)
        End Function
        Public Shared Function MatToBitmap(ByRef oMat As Mat, ByVal iResolution As Integer, Optional iResolution2 As Integer = -1) As System.Drawing.Bitmap
            ' convert mat to bitmap
            If IsNothing(oMat) OrElse ((oMat.Depth <> CvEnum.DepthType.Cv8U And oMat.Depth <> CvEnum.DepthType.Cv32F) Or (oMat.NumberOfChannels <> 1 And oMat.NumberOfChannels <> 3 And oMat.NumberOfChannels <> 4)) Then
                Return Nothing
            Else
                Dim oReturnBitmap As System.Drawing.Bitmap = Nothing
                If oMat.Depth = CvEnum.DepthType.Cv8U Then
                    If oMat.NumberOfChannels = 1 Then
                        Dim oImage = oMat.ToImage(Of [Structure].Gray, Byte)
                        oReturnBitmap = oImage.ToBitmap
                    ElseIf oMat.NumberOfChannels = 3 Then
                        oReturnBitmap = oMat.ToImage(Of [Structure].Bgr, Byte).ToBitmap
                    ElseIf oMat.NumberOfChannels = 4 Then
                        oReturnBitmap = oMat.ToImage(Of [Structure].Bgra, Byte).ToBitmap
                    End If
                ElseIf oMat.Depth = CvEnum.DepthType.Cv32F Then
                    If oMat.NumberOfChannels = 1 Then
                        oReturnBitmap = BitmapConvertGrayscale(oMat.ToImage(Of [Structure].Gray, Single).ToBitmap)
                    ElseIf oMat.NumberOfChannels = 3 Then
                        oReturnBitmap = oMat.ToImage(Of [Structure].Bgr, Byte).ToBitmap
                    ElseIf oMat.NumberOfChannels = 4 Then
                        oReturnBitmap = oMat.ToImage(Of [Structure].Bgra, Byte).ToBitmap
                    End If
                End If

                If Not IsNothing(oReturnBitmap) Then
                    oReturnBitmap.SetResolution(iResolution, If(iResolution2 = -1, iResolution, iResolution2))
                End If
                Return oReturnBitmap
            End If
        End Function
        Public Shared Function MatrixToArray(ByVal oMatrix As Matrix(Of Byte)) As Byte()
            ' convert a matrix to a byte array
            Return oMatrix.Bytes
        End Function
        Public Shared Function ArrayToMatrix(Of T As Structure)(ByVal oArray As Byte(), ByVal width As Integer, ByVal height As Integer, ByVal channels As Integer) As Matrix(Of T)
            ' convert a two dimensional array to a one dimensional array equivalent
            Dim oMatrix As New Matrix(Of T)(height, width, channels)
            oMatrix.Bytes = oArray
            Return oMatrix
        End Function
        Public Shared Function BitmapToMatrix(ByVal oBitmap As System.Drawing.Bitmap) As Matrix(Of Byte)
            ' convert bitmap to matrix
            If IsNothing(oBitmap) Then
                Return Nothing
            Else
                Dim oReturnMatrix As Matrix(Of Byte) = Nothing
                Dim oRectangle As New System.Drawing.Rectangle(0, 0, oBitmap.Width, oBitmap.Height)
                Dim oBitmapData As System.Drawing.Imaging.BitmapData = oBitmap.LockBits(oRectangle, System.Drawing.Imaging.ImageLockMode.ReadOnly, oBitmap.PixelFormat)

                Select Case oBitmap.PixelFormat
                    Case System.Drawing.Imaging.PixelFormat.Format8bppIndexed
                        Using oMat As New Mat(oBitmap.Height, oBitmap.Width, CvEnum.DepthType.Cv8U, 1, oBitmapData.Scan0, oBitmapData.Stride)
                            oReturnMatrix = New Matrix(Of Byte)(oBitmap.Height, oBitmap.Width, 1)
                            oMat.CopyTo(oReturnMatrix)
                        End Using
                    Case System.Drawing.Imaging.PixelFormat.Format24bppRgb
                        Using oMat As New Mat(oBitmap.Height, oBitmap.Width, CvEnum.DepthType.Cv8U, 3, oBitmapData.Scan0, oBitmapData.Stride)
                            oReturnMatrix = New Matrix(Of Byte)(oBitmap.Height, oBitmap.Width, 3)
                            oMat.CopyTo(oReturnMatrix)
                        End Using
                    Case System.Drawing.Imaging.PixelFormat.Format32bppArgb
                        Using oMat As New Mat(oBitmap.Height, oBitmap.Width, CvEnum.DepthType.Cv8U, 4, oBitmapData.Scan0, oBitmapData.Stride)
                            oReturnMatrix = New Matrix(Of Byte)(oBitmap.Height, oBitmap.Width, 3)
                            CvInvoke.CvtColor(oMat, oReturnMatrix, CvEnum.ColorConversion.Bgra2Bgr)
                        End Using
                    Case Else
                        Return Nothing
                End Select

                oBitmap.UnlockBits(oBitmapData)

                Return oReturnMatrix
            End If
        End Function
        Public Shared Function BitmapConvertGrayscale(ByVal oImage As System.Drawing.Bitmap) As System.Drawing.Bitmap
            If IsNothing(oImage) Then
                Return Nothing
            ElseIf oImage.PixelFormat = System.Drawing.Imaging.PixelFormat.Format8bppIndexed Then
                Return oImage.Clone
            ElseIf oImage.PixelFormat = System.Drawing.Imaging.PixelFormat.Format32bppArgb Or oImage.PixelFormat = System.Drawing.Imaging.PixelFormat.Format24bppRgb Then
                Dim oReturnBitmap As System.Drawing.Bitmap = Accord.Imaging.Filters.Grayscale.CommonAlgorithms.BT709.Apply(oImage)
                oReturnBitmap.SetResolution(oImage.HorizontalResolution, oImage.VerticalResolution)
                Return oReturnBitmap
            Else
                Return Nothing
            End If
        End Function
        Public Shared Sub CreateDepthMap(ByRef oColourMatrix As Matrix(Of Byte), ByRef oDepthMatrix As Matrix(Of Single), ByVal oMapType As CvEnum.ColorMapType)
            ' creates a depth map
            ' disposes the colour matrix, if necessary
            If CommonFunctions.MatrixNotNothing(oColourMatrix) Then
                oColourMatrix.Dispose()
                oColourMatrix = Nothing
            End If

            If CommonFunctions.MatrixNotNothing(oDepthMatrix) Then
                Using oSingleMatrix As New Matrix(Of Single)(oDepthMatrix.Size)
                    Using oByteImage As New Image(Of [Structure].Gray, Byte)(oDepthMatrix.Size)
                        CvInvoke.Threshold(oDepthMatrix, oSingleMatrix, 2, 2, CvEnum.ThresholdType.Trunc)
                        CvInvoke.cvConvertScale(oSingleMatrix, oByteImage, Byte.MaxValue / 2, 0)

                        ' create a new colour matrix
                        oColourMatrix = New Matrix(Of Byte)(oDepthMatrix.Height, oDepthMatrix.Width, 3)

                        CvInvoke.ApplyColorMap(oByteImage, oColourMatrix, oMapType)
                    End Using
                End Using
            End If
        End Sub
#End Region
#Region "Xaml"
        Public Shared Function XamlToDrawingImage(ByVal sXaml As String) As DrawingImage
            ' converts text xaml image resource to DrawingImage
            Dim oDrawingImage As DrawingImage = Nothing
            Using oStringReader As New IO.StringReader(sXaml)
                Using oXmlReader As Xml.XmlReader = Xml.XmlReader.Create(oStringReader)
                    Dim oResourceDictionary As ResourceDictionary = Nothing
                    Try
                        oResourceDictionary = CType(Markup.XamlReader.Load(oXmlReader), ResourceDictionary)
                    Catch ex As Markup.XamlParseException
                        Return Nothing
                    End Try

                    For Each sKey As String In oResourceDictionary.Keys
                        Dim oCurrentResource As Object = oResourceDictionary(sKey)
                        If oCurrentResource.GetType.Equals(GetType(DrawingBrush)) Then
                            Dim oDrawingBrush As DrawingBrush = CType(oCurrentResource, DrawingBrush)
                            oDrawingImage = New DrawingImage(oDrawingBrush.Drawing)
                            Exit For
                        End If
                    Next
                End Using
            End Using

            Return oDrawingImage
        End Function
#End Region
#Region "Structures"
        Public Const DoubleNaNString As String = "FFFFFFFFFFFFFFFF"
        Public Shared Function DoubleToByteString(ByVal fDouble As Double) As String
            ' converts a double number to a string representation of 16 chars (8 bytes)
            If Double.IsNaN(fDouble) Then
                Return DoubleNaNString
            Else
                Dim oDoubleBytes As Byte() = BitConverter.GetBytes(BitConverter.DoubleToInt64Bits(fDouble))
                Dim sByteString As String = String.Empty
                For i = 0 To oDoubleBytes.Length - 1
                    sByteString += Hex(oDoubleBytes(i)).PadLeft(2, "0")
                Next
                Return sByteString
            End If
        End Function
        Public Shared Function ByteStringToDouble(ByVal sString As String) As Double
            ' converts a string representation of 16 chars (8 bytes) to a double
            ' returns Double.NaN if no valid conversion is found
            If sString = DoubleNaNString Then
                Return Double.NaN
            Else
                Dim fReturn As Double = Double.NaN
                Dim oCulture As New CultureInfo("en-US")
                If sString.Length = 16 Then
                    Dim oDoubleBytes(7) As Byte
                    For i = 0 To 7
                        Dim sCurrentByte As String = sString(i * 2) + sString((i * 2) + 1)
                        If Not Byte.TryParse(sCurrentByte, NumberStyles.AllowHexSpecifier, oCulture, oDoubleBytes(i)) Then
                            Return Double.NaN
                        End If
                    Next

                    Return BitConverter.Int64BitsToDouble(BitConverter.ToInt64(oDoubleBytes, 0))
                Else
                    Return Double.NaN
                End If
            End If
        End Function
#End Region
    End Class
End Class
#Region "WPFConverters"
<ValueConversion(GetType(Double), GetType(Double))> Public Class MultiplierConverter
    Implements IValueConverter

    Public Function Convert(value As Object, targetType As Type, parameter As Object, culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim fReturn As Double = CDbl(value) * CDbl(parameter)
        Return If(fReturn = 0, 1, fReturn)
    End Function
    Public Function ConvertBack(value As Object, targetType As Type, parameter As Object, culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Return If(CDbl(parameter) = 0, CDbl(1), CDbl(value) / CDbl(parameter))
    End Function
End Class
Public Class IndexOfConverter
    Implements IMultiValueConverter

    Public Function Convert(ByVal values As Object(), ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IMultiValueConverter.Convert
        If Not values(0).GetType.Equals(GetType(DataGrid)) Then
            Return Binding.DoNothing
        ElseIf DesignerProperties.GetIsInDesignMode(values(0)) Then
            Return False
        Else
            Dim oDataGrid As DataGrid = TryCast(values(0), DataGrid)
            If IsNothing(oDataGrid) Then
                Return Binding.DoNothing
            Else
                Dim oItem As Object = values(1)
                Dim iStart As Int32 = values(2)
                Dim iIndex As Integer = oDataGrid.Items.IndexOf(oItem)
                Return (iIndex + iStart).ToString
            End If
        End If
    End Function
    Public Function ConvertBack(ByVal value As Object, ByVal targetTypes As Type(), ByVal parameter As Object, ByVal culture As CultureInfo) As Object() Implements IMultiValueConverter.ConvertBack
        Return targetTypes.[Select](Function(t) Binding.DoNothing).ToArray()
    End Function
End Class
#End Region
Public Enum PreStartEnum As Integer
    None = 0
    Two = 2
    Ten = 10
End Enum
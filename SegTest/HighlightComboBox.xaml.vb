Imports SegTest.Common
Imports System.Collections.ObjectModel

Public Class HighlightComboBox
    Private Const InnerMarginScale As Double = 0.15
    Private Const InnerHeightScale As Double = 0.5
    Private Const InnerFontSizeScale As Double = 0.25
    Private m_WidthMultiplier As Double = 1
    Private m_FontMultiplier As Double = 1
    Public Event SelectionChanged(sender As Object, e As SelectionChangedEventArgs)
    Public Shared ReadOnly HCBReferenceHeightProperty As DependencyProperty = DependencyProperty.Register("HCBReferenceHeight", GetType(Double), GetType(HighlightComboBox), New FrameworkPropertyMetadata(CDbl(0), FrameworkPropertyMetadataOptions.AffectsMeasure, New PropertyChangedCallback(AddressOf OnHCBReferenceHeightChanged)))
    Public Shared ReadOnly HCBReferenceWidthProperty As DependencyProperty = DependencyProperty.Register("HCBReferenceWidth", GetType(Double), GetType(HighlightComboBox), New FrameworkPropertyMetadata(CDbl(0), FrameworkPropertyMetadataOptions.AffectsMeasure, New PropertyChangedCallback(AddressOf OnHCBReferenceWidthChanged)))
    Public Shared ReadOnly HCBTextProperty As DependencyProperty = DependencyProperty.Register("HCBText", GetType(HCBDisplay), GetType(HighlightComboBox), New FrameworkPropertyMetadata(New HCBDisplay, FrameworkPropertyMetadataOptions.AffectsRender, New PropertyChangedCallback(AddressOf OnHCBTextChanged)))
    Public Shared ReadOnly HCBContentProperty As DependencyProperty = DependencyProperty.Register("HCBContent", GetType(ObservableCollection(Of HCBDisplay)), GetType(HighlightComboBox), New FrameworkPropertyMetadata(New ObservableCollection(Of HCBDisplay), FrameworkPropertyMetadataOptions.AffectsRender, New PropertyChangedCallback(AddressOf OnHCBContentChanged)))

    Public Property HCBReferenceHeight As Double
        Get
            Return CDbl(GetValue(HCBReferenceHeightProperty))
        End Get
        Set(ByVal value As Double)
            SetValue(HCBReferenceHeightProperty, value)
        End Set
    End Property
    Public Property HCBReferenceWidth As Double
        Get
            Return CDbl(GetValue(HCBReferenceWidthProperty))
        End Get
        Set(ByVal value As Double)
            SetValue(HCBReferenceWidthProperty, value)
        End Set
    End Property
    Public WriteOnly Property HCBText As HCBDisplay
        Set(ByVal value As HCBDisplay)
            If IsNothing(ComboBoxMain.ItemsSource) Then
                ComboBoxMain.SelectedIndex = -1
            Else
                Dim oHCBDisplayCollection As ObservableCollection(Of HCBDisplay) = ComboBoxMain.ItemsSource
                Dim oIndexList As List(Of Integer) = (From iIndex As Integer In Enumerable.Range(0, oHCBDisplayCollection.Count) Where oHCBDisplayCollection(iIndex).Equals(value) Select iIndex).ToList
                If oIndexList.Count > 0 Then
                    ComboBoxMain.SelectedIndex = oIndexList.First
                Else
                    ComboBoxMain.SelectedIndex = -1
                End If
            End If
        End Set
    End Property
    Public WriteOnly Property HCBContent As ObservableCollection(Of HCBDisplay)
        Set(value As ObservableCollection(Of HCBDisplay))
            ComboBoxMain.ItemsSource = value
        End Set
    End Property
    Public Property WidthMultiplier As Double
        Get
            Return m_WidthMultiplier
        End Get
        Set(value As Double)
            m_WidthMultiplier = value
        End Set
    End Property
    Public Property FontMultiplier As Double
        Get
            Return m_FontMultiplier
        End Get
        Set(value As Double)
            m_FontMultiplier = value
        End Set
    End Property
    Public Property IsEditable As Boolean
        Get
            Return ComboBoxMain.IsEditable
        End Get
        Set(value As Boolean)
            ComboBoxMain.IsEditable = value
        End Set
    End Property
    Public Property IsReadOnly As Boolean
        Get
            Return ComboBoxMain.IsReadOnly
        End Get
        Set(value As Boolean)
            ComboBoxMain.IsReadOnly = value
        End Set
    End Property
    Public WriteOnly Property InnerToolTip As String
        Set(value As String)
            ComboBoxMain.ToolTip = value
        End Set
    End Property
    Private Shared Sub OnHCBReferenceHeightChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        Dim oHighlightComboBox As HighlightComboBox = CType(d, HighlightComboBox)
        Dim oGrid As Grid = CType(oHighlightComboBox.Content, Grid)
        Dim oRectangleBackground As Rectangle = CType(oGrid.FindName("RectangleBackground"), Rectangle)
        oRectangleBackground.Margin = New Thickness(e.NewValue * InnerMarginScale)

        Dim oComboBoxMain As ComboBox = CType(oGrid.FindName("ComboBoxMain"), ComboBox)
        oComboBoxMain.Margin = New Thickness(e.NewValue * InnerMarginScale)
        If e.NewValue > 0 Then
            oComboBoxMain.FontSize = e.NewValue * oHighlightComboBox.FontMultiplier * InnerFontSizeScale
        End If
        oComboBoxMain.MinHeight = e.NewValue * InnerHeightScale
        oComboBoxMain.MinWidth = Math.Max(e.NewValue, oHighlightComboBox.HCBReferenceWidth) * InnerHeightScale * oHighlightComboBox.WidthMultiplier
    End Sub
    Private Shared Sub OnHCBReferenceWidthChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        Dim oHighlightComboBox As HighlightComboBox = CType(d, HighlightComboBox)
        Dim oGrid As Grid = CType(oHighlightComboBox.Content, Grid)
        Dim oComboBoxMain As ComboBox = CType(oGrid.FindName("ComboBoxMain"), ComboBox)
        oComboBoxMain.MinWidth = Math.Max(oHighlightComboBox.HCBReferenceHeight, e.NewValue) * InnerHeightScale * oHighlightComboBox.WidthMultiplier
    End Sub
    Private Shared Sub OnHCBTextChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        If Not IsNothing(e.NewValue) Then
            Dim oHCBDisplay As HCBDisplay = e.NewValue
            Dim oHighlightComboBox As HighlightComboBox = CType(d, HighlightComboBox)
            Dim oGrid As Grid = CType(oHighlightComboBox.Content, Grid)
            Dim oComboBoxMain As ComboBox = CType(oGrid.FindName("ComboBoxMain"), ComboBox)
            If IsNothing(oComboBoxMain.ItemsSource) Then
                oComboBoxMain.SelectedIndex = -1
            Else
                Dim oHCBDisplayCollection As ObservableCollection(Of HCBDisplay) = oComboBoxMain.ItemsSource
                Dim oIndexList As List(Of Integer) = (From iIndex As Integer In Enumerable.Range(0, oHCBDisplayCollection.Count) Where oHCBDisplayCollection(iIndex).Equals(oHCBDisplay) Select iIndex).ToList
                If oIndexList.Count > 0 Then
                    oComboBoxMain.SelectedIndex = oIndexList.First
                Else
                    oComboBoxMain.SelectedIndex = -1
                End If
            End If
        End If
    End Sub
    Private Shared Sub OnHCBContentChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        Dim oHighlightComboBox As HighlightComboBox = CType(d, HighlightComboBox)
        Dim oGrid As Grid = CType(oHighlightComboBox.Content, Grid)
        Dim oComboBoxMain As ComboBox = CType(oGrid.FindName("ComboBoxMain"), ComboBox)
        oComboBoxMain.ItemsSource = e.NewValue
    End Sub
    Private Sub OnHCBSelectionChanged(sender As Object, e As SelectionChangedEventArgs) Handles ComboBoxMain.SelectionChanged
        If (e.AddedItems.Count <> e.RemovedItems.Count) OrElse (Not e.AddedItems.Equals(e.RemovedItems)) Then
            RaiseEvent SelectionChanged(sender, e)
        End If
    End Sub
    Private Sub ComboBoxMainMouseMoveHandler(sender As Object, e As MouseEventArgs) Handles ComboBoxMain.MouseMove
        If Not e.LeftButton = MouseButtonState.Pressed Then
            ComboBoxMain.Background = New SolidColorBrush(Color.FromArgb(&H33, &H0, &HFF, &H0))
        End If
    End Sub
    Private Sub ComboBoxMainMouseLeaveHandler(sender As Object, e As EventArgs) Handles ComboBoxMain.MouseLeave, ComboBoxMain.TouchLeave
        ComboBoxMain.Background = New SolidColorBrush(Colors.Transparent)
    End Sub
End Class
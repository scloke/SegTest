Imports Microsoft.VisualBasic

Public Class Scrap
    Private Function GenerateFrameDictionaryKey(ByVal sSerial As String, ByVal oStream As RS.Stream, ByVal iFrameNumber As ULong) As String
        ' generates key for frame dictionary
        Dim iSerialHash As Integer = sSerial.GetHashCode
        Dim iStreamHash As Integer = oStream.GetHashCode

        ' get individual byte arrays
        Dim oSerialBytes As Byte() = BitConverter.GetBytes(iSerialHash)
        Dim oStreamBytes As Byte() = BitConverter.GetBytes(iStreamHash)
        Dim oFrameBytes As Byte() = BitConverter.GetBytes(iFrameNumber)

        ' generate combined byte array
        Dim iArrayLength As Integer = oSerialBytes.Length + oStreamBytes.Length + oFrameBytes.Length
        Dim oKeyArray(iArrayLength - 1) As Byte
        Array.Copy(oSerialBytes, 0, oKeyArray, 0, oSerialBytes.Length)
        Array.Copy(oStreamBytes, 0, oKeyArray, oSerialBytes.Length, oStreamBytes.Length)
        Array.Copy(oFrameBytes, 0, oKeyArray, oSerialBytes.Length + oStreamBytes.Length, oFrameBytes.Length)

        Dim sKey As String = System.Text.Encoding.UTF8.GetString(oKeyArray)
        Return sKey
    End Function
    Private Class FrameSink
        Implements IDisposable

        Private m_Frames As ConcurrentQueue(Of RS.Frame)

        Sub New()
            m_Frames = New ConcurrentQueue(Of RS.Frame)
        End Sub
        Public ReadOnly Property Count As Integer
            Get
                Return m_Frames.Count
            End Get
        End Property
        Public Sub Enqueue(ByVal frame As RS.Frame)
            If Not IsNothing(frame) Then
                m_Frames.Enqueue(frame)
            End If
        End Sub
        Public Function GetFrame() As RS.Frame
            If m_Frames.Count = 0 Then
                Return Nothing
            Else
                Dim oFrame As RS.Frame = Nothing
                If m_Frames.TryDequeue(oFrame) Then
                    Return oFrame
                Else
                    Return Nothing
                End If
            End If
        End Function
#Region "IDisposable Support"
        Private disposedValue As Boolean
        Protected Shadows Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    ' TODO: dispose managed state (managed objects).
                    Do Until m_Frames.Count = 0
                        Dim oFrame As RS.Frame = Nothing
                        m_Frames.TryDequeue(oFrame)
                        If Not IsNothing(oFrame) Then
                            oFrame.Dispose()
                        End If
                    Loop
                End If
            End If
            disposedValue = True
        End Sub
        Public Shadows Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
        End Sub
#End Region
    End Class
    Private Sub WarpFrame()
        Using oVectorFlow As New Util.VectorOfMat
            Using oFractionFlowX As New Matrix(Of Single)(OpticalFlowHeight, OpticalFlowWidth)
                Using oFractionFlowY As New Matrix(Of Single)(OpticalFlowHeight, OpticalFlowWidth)
                    CvInvoke.Split(oFractionFlow, oVectorFlow)
                    oVectorFlow(0).CopyTo(oFractionFlowX)
                    oVectorFlow(1).CopyTo(oFractionFlowY)

                    Dim iCountX As Integer = CvInvoke.CountNonZero(oFractionFlowX)
                    Dim iCountY As Integer = CvInvoke.CountNonZero(oFractionFlowY)
                    If iCountX = 0 And iCountY = 0 Then
                        Return oFrame1.Clone
                    Else
                        Dim oInterMatrix As New Matrix(Of T)(oFrame1.Height, oFrame1.Width, oFrame1.NumberOfChannels)
                        oFrame1.Mul(fFraction).CopyTo(oInterMatrix)
                        oFrame2.Mul(1 - fFraction).Add(oInterMatrix).CopyTo(oInterMatrix)

                        Using oMaskMatrix1 As New Matrix(Of Byte)(oFrame1.Size)
                            oMaskMatrix1.SetValue(255)
                            Using oMaskMatrix2 As New Matrix(Of Byte)(oFrame1.Size)
                                Using oWarpMatrix As New Matrix(Of T)(oFrame1.Height, oFrame1.Width, oFrame1.NumberOfChannels)
                                    For y = 0 To OpticalFlowHeight - 1
                                        For x = 0 To OpticalFlowWidth - 1
                                            Dim xFlow As Single = oFractionFlowX(y, x) * iScaleDown
                                            Dim yFlow As Single = oFractionFlowY(y, x) * iScaleDown
                                            If xFlow > FlowThreshold OrElse yFlow > FlowThreshold Then
                                                Dim oPoint1 As New System.Drawing.PointF(x * iScaleDown, y * iScaleDown)
                                                Dim oPoint2 As New System.Drawing.PointF(((x + 1) * iScaleDown) - 1, y * iScaleDown)
                                                Dim oPoint3 As New System.Drawing.PointF(((x + 1) * iScaleDown) - 1, ((y + 1) * iScaleDown) - 1)
                                                Dim oPoint4 As New System.Drawing.PointF(x * iScaleDown, ((y + 1) * iScaleDown) - 1)

                                                Dim oPoint1A As New System.Drawing.PointF((x * iScaleDown) + xFlow, (y * iScaleDown) + yFlow)
                                                Dim oPoint2A As New System.Drawing.PointF((((x + 1) * iScaleDown) - 1) + xFlow, (y * iScaleDown) + yFlow)
                                                Dim oPoint3A As New System.Drawing.PointF((((x + 1) * iScaleDown) - 1) + xFlow, (((y + 1) * iScaleDown) - 1) + yFlow)
                                                Dim oPoint4A As New System.Drawing.PointF((x * iScaleDown) + xFlow, (((y + 1) * iScaleDown) - 1) + yFlow)

                                                Dim iMinX As Integer = Integer.MaxValue
                                                Dim iMaxX As Integer = Integer.MinValue
                                                Dim iMinY As Integer = Integer.MaxValue
                                                Dim iMaxY As Integer = Integer.MinValue

                                                Dim oSourcePoints As System.Drawing.PointF() = {oPoint1, oPoint2, oPoint3, oPoint4}
                                                Dim oDestPoints As System.Drawing.PointF() = {oPoint1A, oPoint2A, oPoint3A, oPoint4A}

                                                For Each oPoint In oSourcePoints
                                                    iMinX = Math.Min(iMinX, oPoint.X)
                                                    iMinY = Math.Min(iMinY, oPoint.Y)
                                                    iMaxX = Math.Max(iMaxX, oPoint.X)
                                                    iMaxY = Math.Max(iMaxY, oPoint.Y)
                                                Next
                                                For Each oPoint In oDestPoints
                                                    iMinX = Math.Min(iMinX, oPoint.X)
                                                    iMinY = Math.Min(iMinY, oPoint.Y)
                                                    iMaxX = Math.Max(iMaxX, oPoint.X)
                                                    iMaxY = Math.Max(iMaxY, oPoint.Y)
                                                Next

                                                iMinX = Math.Max(0, iMinX)
                                                iMinY = Math.Max(0, iMinY)
                                                iMaxX = Math.Min(oFrame1.Width - 1, iMaxX)
                                                iMaxY = Math.Min(oFrame1.Height - 1, iMaxY)
                                                Dim oROI As New System.Drawing.Rectangle(iMinX, iMinY, iMaxX - iMinX + 1, iMaxY - iMinY + 1)

                                                Using oPerspectiveMat As Mat = CvInvoke.GetPerspectiveTransform(oSourcePoints, oDestPoints)
                                                    oMaskMatrix2.GetSubRect(oROI).SetZero()
                                                    oWarpMatrix.GetSubRect(oROI).SetZero()
                                                    CvInvoke.WarpPerspective(oFrame1.GetSubRect(oROI), oWarpMatrix.GetSubRect(oROI), oPerspectiveMat, oWarpMatrix.Size, CvEnum.Inter.Lanczos4, CvEnum.Warp.Default)
                                                    CvInvoke.WarpPerspective(oMaskMatrix1.GetSubRect(oROI), oMaskMatrix2.GetSubRect(oROI), oPerspectiveMat, oMaskMatrix2.Size, CvEnum.Inter.Lanczos4, CvEnum.Warp.Default)
                                                    CvInvoke.Threshold(oMaskMatrix2.GetSubRect(oROI), oMaskMatrix2.GetSubRect(oROI), 127, 255, CvEnum.ThresholdType.Binary)

                                                    oInterMatrix.GetSubRect(oROI).SetValue(New [Structure].MCvScalar(0, 0, 0), oMaskMatrix2.GetSubRect(oROI))
                                                    CvInvoke.BitwiseNot(oMaskMatrix2.GetSubRect(oROI), oMaskMatrix2.GetSubRect(oROI))
                                                    oWarpMatrix.GetSubRect(oROI).SetValue(New [Structure].MCvScalar(0, 0, 0), oMaskMatrix2.GetSubRect(oROI))
                                                    CvInvoke.Max(oInterMatrix.GetSubRect(oROI), oWarpMatrix.GetSubRect(oROI), oInterMatrix.GetSubRect(oROI))
                                                End Using
                                            End If
                                        Next
                                    Next
                                End Using
                            End Using
                        End Using

                        Using oOutput As New Matrix(Of Byte)(oFrame1.Height, oFrame1.Width, 3)
                            CommonFunctions.SaveMatrix("D:\Downloads\Test1.tif", oFrame1.Convert(Of Byte))
                            CommonFunctions.SaveMatrix("D:\Downloads\Test2.tif", oFrame2.Convert(Of Byte))
                            CommonFunctions.SaveMatrix("D:\Downloads\Test3.tif", oInterMatrix.Convert(Of Byte))
                        End Using


                        Return oInterMatrix
                    End If
                End Using
            End Using
        End Using
    End Sub
    Public Class Processor
        Implements IDisposable

        Public FaceIdentifierStore As FaceIdentifier
#Region "Constants"
        Private Const SmallWidth As Integer = 640

        ' number of face landmarks
        Private Const LandmarkCount As Integer = 68
        Public Const MaxFaces As Integer = 2
        ' alternate angles for face detection
        Public Shared FaceDetectionAngles As List(Of Integer) = {-90, -60, -30, 30, 60, 90}.ToList
#End Region

        Sub New()
            FaceIdentifierStore = New FaceIdentifier
        End Sub
        Public Sub Reset()
            FaceIdentifierStore.Dispose()
            FaceIdentifierStore = New FaceIdentifier
        End Sub
        Public Function DetectFaces(ByVal oColourMatrix As Matrix(Of Byte)) As List(Of FaceIdentifier.FaceResult)
            ' returns the detected face regions
            Dim oRotatedRegions As New List(Of FaceIdentifier.FaceResult)
            If Not IsNothing(oColourMatrix) Then
                Dim fSmallScale As Double = SmallWidth / oColourMatrix.Width
                Dim iSmallHeight As Integer = oColourMatrix.Height * fSmallScale

                Dim oRegions As New KeyValuePair(Of Integer, List(Of FaceIdentifier.FaceResult))(0, New List(Of FaceIdentifier.FaceResult))
                Dim oSmallRegions As New List(Of Rect)

                Using oMonoMatrix As New Matrix(Of Byte)(oColourMatrix.Size)
                    CvInvoke.CvtColor(oColourMatrix, oMonoMatrix, CvEnum.ColorConversion.Bgr2Gray)
                    Using oSmallMonoMatrix As New Matrix(Of Byte)(iSmallHeight, SmallWidth)
                        CvInvoke.Resize(oMonoMatrix, oSmallMonoMatrix, oSmallMonoMatrix.Size)

                        ' use CLAHE to improve contrast for face detection
                        CvInvoke.CLAHE(oSmallMonoMatrix, 2, New System.Drawing.Size(8, 8), oSmallMonoMatrix)

                        ' check on unrotated faces to see if detection possible
                        Dim oUnrotatedResizedFaces As List(Of FaceIdentifier.FaceResult) = FaceDetectProcessing(oSmallMonoMatrix, fSmallScale)

                        If oUnrotatedResizedFaces.Count > 0 Then
                            oRegions = New KeyValuePair(Of Integer, List(Of FaceIdentifier.FaceResult))(0, oUnrotatedResizedFaces)
                        Else
                            ' check rotated faces to see if detection possible
                            ' get the matrix dictionary for processing
                            Dim oMatrixDictionary As Dictionary(Of Integer, Tuple(Of Matrix(Of Byte), [Structure].RotatedRect)) = GetDetectionMatrices(oSmallMonoMatrix)
                            Dim oResizedFacesDictionary As New ConcurrentDictionary(Of Integer, List(Of FaceIdentifier.FaceResult))
                            Dim oActionList As New List(Of Tuple(Of Action(Of Object), Object))

                            Dim oAction As Action(Of Object) = Sub(oParam As KeyValuePair(Of Integer, Tuple(Of Matrix(Of Byte), [Structure].RotatedRect)))
                                                                   Dim oLocalResizedFaces As List(Of FaceIdentifier.FaceResult) = FaceDetectProcessing(oParam.Value.Item1, fSmallScale)

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
                                Dim oOrderedFacesDictionary As Dictionary(Of Integer, List(Of FaceIdentifier.FaceResult)) = oResizedFacesDictionary.ToList.OrderBy(Function(x) x.Key).ToDictionary(Function(x) x.Key, Function(x) x.Value)
                                Dim iMedianIndex As Integer = (oOrderedFacesDictionary.Count - 1) / 2
                                oRegions = New KeyValuePair(Of Integer, List(Of FaceIdentifier.FaceResult))(oOrderedFacesDictionary.Keys(iMedianIndex), oOrderedFacesDictionary.Values(iMedianIndex))
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

                                oRotatedRegions.Add(New FaceIdentifier.FaceResult(oRotatedRect, oTransformedLandmarks))
                            Next
                        End Using
                    End Using
                End If
            End If
            Return oRotatedRegions
        End Function
        Private Shared Function FaceDetectProcessing(ByVal oMatrix As Matrix(Of Byte), ByVal fSmallScale As Double) As List(Of FaceIdentifier.FaceResult)
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
            Dim oFaceResult As New List(Of FaceIdentifier.FaceResult)
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
                oFaceResult.Add(New FaceIdentifier.FaceResult(oFaceRect, oLandmarks))
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
        Public Class FaceIdentifier
            Inherits List(Of FaceIdentifierItem)
            Implements IDisposable

#Region "Constants"
            ' time period to remove old face dictionary entries (seconds)
            Private Const DictionaryRemovalTime As Integer = 5
            ' face hash size
            Private Const FaceAnalyserSize As Integer = 128
            ' maximum missing values
            Private Const MaxMissing As Integer = 3
            ' smoothing alpha and beta
            Private Const SmoothAlphaFaceLocation As Single = 0.5
            Private Const SmoothBetaFaceLocation As Single = 0.5
            ' capacity for the smoothing queue
            Private Const FaceQueueCapacity As Integer = 20
            ' distance threshold for face identification
            Private Const LBPHFaceThreshold As Double = 100
#End Region

            Sub New()
                MyBase.New
            End Sub
            Public Function GetFaceIndices(ByRef oFaces As Dictionary(Of Integer, FaceResult), ByVal oFrameSize As System.Drawing.Size, ByVal iMaxFaces As Integer) As ConcurrentDictionary(Of Integer, Tuple(Of Integer, Double))
                ' gets the index of the matching faces
                ' the faces are given as a dictionary containing the number as the key, and the value as a tuple of the face hash and the location
                ' the result is a dictionary with the same key, and the value as a tuple of the index and the distance
                If iMaxFaces = 0 Then
                    Return Nothing
                End If

                ' remove outdated face items
                Dim oCurrentDate As Date = NTPTime.GetDateTimeNow
                For i = Me.Count - 1 To 0 Step -1
                    If (oCurrentDate - Me(i).UpdateTime).TotalSeconds > DictionaryRemovalTime Then
                        Me.RemoveAt(i)
                    End If
                Next

                Dim oFinalResults As New ConcurrentDictionary(Of Integer, Tuple(Of Integer, Double))
                Dim oKeyList As List(Of Integer) = oFaces.Keys.ToList
                If Me.Count > 0 Then
                    Using oLBPHFaceRecognizer As New Face.LBPHFaceRecognizer()
                        Using oLabels As New Util.VectorOfInt
                            Using oTrainVector As New Util.VectorOfMat
                                For i = 0 To Me.Count - 1
                                    Dim iFacePartIndex As Integer = Me(i).Index
                                    For j = 0 To Me(i).Count - 1
                                        Dim oFaceMatrixTrain As Matrix(Of Byte) = GetMatrixFromBytes(Me(i)(j).Item1)
                                        oTrainVector.Push(oFaceMatrixTrain.Mat)
                                        oLabels.Push({iFacePartIndex})
                                    Next
                                Next

                                ' add a blank face at the end so that detection distance can be found even with a single face
                                If oTrainVector.Size = 1 Then
                                    Dim oBlankFace As New Matrix(Of Byte)(oTrainVector(0).Size)
                                    oTrainVector.Push(oBlankFace)
                                    oLabels.Push({-1})
                                End If

                                oLBPHFaceRecognizer.Train(oTrainVector, oLabels)

                                ' check the face dictionary
                                Dim oFaceResults As New Dictionary(Of Integer, Tuple(Of Integer, Double))
                                Dim oIndexList As List(Of Integer) = (From oItem In Me Select oItem.Index).ToList
                                For i = 0 To oFaces.Count - 1
                                    Dim iKey As Integer = oFaces.Keys(i)
                                    Dim oLocation As System.Drawing.PointF = oFaces(iKey).Location
                                    Using oFaceMatrix As Matrix(Of Byte) = GetMatrixFromBytes(oFaces(iKey).FaceBytes)
                                        Dim oResult As Face.FaceRecognizer.PredictionResult = oLBPHFaceRecognizer.Predict(oFaceMatrix.Mat)

                                        ' check if prediction is in the store
                                        If oIndexList.Contains(oResult.Label) Then
                                            Dim oFaceIdentifierItem As FaceIdentifierItem = Me(GetNumberFromIndex(oResult.Label))

                                            ' assume that a face location that is not in the process dictionary is 20% the image diagonal distance
                                            Dim fDiagonalDistance20 As Double = Math.Sqrt((oFrameSize.Width * oFrameSize.Width) + (oFrameSize.Height * oFrameSize.Height)) * 0.2
                                            Dim oSmoothedLocation As System.Drawing.PointF = oFaceIdentifierItem.GetSmoothedLocation
                                            Dim fActualDistance As Double = Math.Sqrt(((oLocation.X - oSmoothedLocation.X) * (oLocation.X - oSmoothedLocation.X)) + ((oLocation.Y - oSmoothedLocation.Y) * (oLocation.Y - oSmoothedLocation.Y)))

                                            ' if only cropped faces are present in the queue, then halve the distance to account for the decreased accuracy
                                            Dim fModifiedDistance As Double = oResult.Distance * Math.Max(fActualDistance / fDiagonalDistance20, 0.5) * If(oFaceIdentifierItem.ContainsNonCropped, 1, 0.5)

                                            oFaceResults.Add(iKey, New Tuple(Of Integer, Double)(oResult.Label, fModifiedDistance))
                                        Else
                                            oFaceResults.Add(iKey, New Tuple(Of Integer, Double)(GetFreeIndex(iMaxFaces), Double.MaxValue))
                                        End If
                                    End Using
                                Next

                                ' dispose of matrices
                                For i = 0 To oTrainVector.Size - 1
                                    oTrainVector(i).Dispose()
                                Next

                                ' finalise results
                                Dim oLabelList As List(Of Integer) = (From oItem In oFaceResults.Values Where oItem.Item1 <> -1 Select oItem.Item1 Distinct).ToList
                                For i = 0 To oLabelList.Count - 1
                                    Dim iLabel As Integer = oLabelList(i)

                                    ' get item with either the maximum distance (new addition) or the lowest distance less than the threshold
                                    Dim oMaxKeyList As List(Of Integer) = (From oItem In oFaceResults Where oItem.Value.Item1 = iLabel AndAlso (oItem.Value.Item2 = Double.MaxValue OrElse oItem.Value.Item2 < LBPHFaceThreshold) Order By oItem.Value.Item2 Ascending Select oItem.Key).ToList

                                    If oMaxKeyList.Count > 0 Then
                                        ' add to final results and remove it from the list of processed keys
                                        Dim iMaxKey As Integer = oMaxKeyList.First
                                        oFinalResults.TryAdd(iMaxKey, oFaceResults(iMaxKey))
                                        oKeyList.Remove(iMaxKey)
                                    End If
                                Next

                                ' add values for the remaining keys
                                For i = 0 To oKeyList.Count - 1
                                    Dim iLabel As Integer = GetFreeIndex(iMaxFaces)
                                    If iLabel = -1 Then
                                        ' no more free labels
                                        oFinalResults.TryAdd(oKeyList(i), Nothing)
                                    Else
                                        ' add new label to the list
                                        oFinalResults.TryAdd(oKeyList(i), New Tuple(Of Integer, Double)(iLabel, Double.MaxValue))
                                    End If
                                Next

                                ' add missing indices
                                oIndexList = (From oItem In Me Select oItem.Index).ToList
                                Dim oFinalIndexList As List(Of Integer) = (From oValue In oFinalResults.Values Where Not IsNothing(oValue) Select oValue.Item1).ToList
                                For Each iIndex In oFinalIndexList
                                    oIndexList.Remove(iIndex)
                                Next

                                For Each iIndex In oIndexList
                                    Dim iNumber As Integer = GetNumberFromIndex(iIndex)
                                    If Me(iNumber).MissingCount < MaxMissing Then
                                        Me(iNumber).MissingCount += 1
                                        Dim oProjectedLocation As System.Drawing.PointF = Me(iNumber).GetSmoothedLocation
                                        Dim oNewFaceResult As FaceResult = Me(iNumber).LastFaceResult.UpdateResult(oProjectedLocation)

                                        ' get first available key
                                        Dim iNewKey As Integer = -1
                                        Dim iCurrent As Integer = 0
                                        Do
                                            Dim iFound As Integer = Aggregate iCurrentKey In oFaces.Keys Where iCurrentKey = iCurrent Into Count
                                            If iFound = 0 Then
                                                iNewKey = iCurrent
                                                Exit Do
                                            End If
                                            iCurrent += 1
                                        Loop

                                        oFaces.Add(iNewKey, oNewFaceResult)
                                        oFinalResults.TryAdd(iNewKey, New Tuple(Of Integer, Double)(iNumber, Double.MaxValue))
                                    End If
                                Next
                            End Using
                        End Using
                    End Using
                Else
                    For i = 0 To oKeyList.Count - 1
                        Dim iLabel As Integer = GetFreeIndex(iMaxFaces)
                        If iLabel = -1 Then
                            ' no more free labels
                            oFinalResults.TryAdd(oKeyList(i), Nothing)
                        Else
                            ' add new label to the list
                            oFinalResults.TryAdd(oKeyList(i), New Tuple(Of Integer, Double)(iLabel, Double.MaxValue))
                        End If
                    Next
                End If

                ' run through the final results and enqueue all valid values
                For i = 0 To oFinalResults.Keys.Count - 1
                    Dim iKey As Integer = oFinalResults.Keys(i)
                    If (Not IsNothing(oFinalResults(iKey))) AndAlso (Not IsNothing(oFaces(iKey).FaceBytes)) Then
                        For j = 0 To Me.Count - 1
                            If Me(j).Index = oFinalResults(iKey).Item1 Then
                                Me(j).Enqueue(oFaces(iKey))
                                Exit For
                            End If
                        Next
                    End If
                Next

                Return oFinalResults
            End Function
            Public Function GetNumberFromIndex(ByVal iIndex As Integer) As Integer
                ' gets the number of the face identifier item from its index
                Dim oItemList As List(Of Integer) = (From iCurrentIndex In Enumerable.Range(0, Me.Count) Where Me(iCurrentIndex).Index = iIndex Select iCurrentIndex).ToList
                If oItemList.Count > 0 Then
                    Return oItemList.First
                Else
                    Return -1
                End If
            End Function
            Private Function GetFreeIndex(ByVal iMaxFaces As Integer) As Integer
                ' check if there is space for another item
                ' find first free index
                Dim iFacePartIndex As Integer = -1
                For i = 0 To iMaxFaces - 1
                    Dim iCurrent As Integer = i
                    Dim iFound As Integer = Aggregate oItem In Me Where oItem.Index = iCurrent Into Count
                    If iFound = 0 Then
                        iFacePartIndex = i
                        Exit For
                    End If
                Next

                If iFacePartIndex <> -1 Then
                    ' add item to list if not already there
                    Dim oIndexList As List(Of Integer) = (From oItem In Me Select oItem.Index).ToList
                    If Not oIndexList.Contains(iFacePartIndex) Then
                        Dim oFaceIdentifierItem As New FaceIdentifierItem(iFacePartIndex)
                        Me.Add(oFaceIdentifierItem)
                    End If
                End If

                Return iFacePartIndex
            End Function
            Private Shared Function GetMatrixFromBytes(ByVal oBytes As Byte()) As Matrix(Of Byte)
                ' re-creates a matrix from an array of bytes
                Dim iByteCount As Integer = FaceAnalyserSize * FaceAnalyserSize
                Dim iChannels As Integer = oBytes.Count / iByteCount
                Dim oMatrix As New Matrix(Of Byte)(FaceAnalyserSize, FaceAnalyserSize, iChannels)
                oMatrix.Bytes = oBytes
                Return oMatrix
            End Function
            Public Shared Function FaceBytes(ByVal oMatrix As Matrix(Of Byte)) As Byte()
                ' gets the face based on the landmarks and resizes it to a square matrix, and get the bytes
                If oMatrix.Width = FaceAnalyserSize AndAlso oMatrix.Height = FaceAnalyserSize Then
                    If oMatrix.NumberOfChannels = 1 Then
                        Return oMatrix.Bytes
                    ElseIf oMatrix.NumberOfChannels = 3 Then
                        Using oMonoMatrix As New Matrix(Of Byte)(FaceAnalyserSize, FaceAnalyserSize)
                            CvInvoke.CvtColor(oMatrix, oMonoMatrix, CvEnum.ColorConversion.Bgr2Gray)
                            Return oMonoMatrix.Bytes
                        End Using
                    Else
                        Return Nothing
                    End If
                Else
                    Using oResizedMatrix As New Matrix(Of Byte)(FaceAnalyserSize, FaceAnalyserSize, oMatrix.NumberOfChannels)
                        CvInvoke.Resize(oMatrix, oResizedMatrix, oResizedMatrix.Size)

                        If oMatrix.NumberOfChannels = 1 Then
                            Return oResizedMatrix.Bytes
                        ElseIf oMatrix.NumberOfChannels = 3 Then
                            Using oMonoMatrix As New Matrix(Of Byte)(FaceAnalyserSize, FaceAnalyserSize)
                                CvInvoke.CvtColor(oResizedMatrix, oMonoMatrix, CvEnum.ColorConversion.Bgr2Gray)
                                Return oMonoMatrix.Bytes
                            End Using
                        Else
                            Return Nothing
                        End If
                    End Using
                End If
            End Function
            Public Shadows Sub Clear()
                For i = 0 To Me.Count - 1
                    Me(i).Dispose()
                Next
                MyBase.Clear()
            End Sub
            Public Class FaceIdentifierItem
                Inherits Queue(Of Tuple(Of Byte(), Boolean))
                Implements IDisposable

                Public Index As Integer
                Public UpdateTime As Date
                Public LocationX As DoubleSmoothing
                Public LocationY As DoubleSmoothing
                Public MissingCount As Integer
                Public LastFaceResult As FaceResult
                Private Const m_Capacity As Integer = 8

                Sub New(ByVal iFacePartIndex As Integer)
                    MyBase.New(m_Capacity)
                    Index = iFacePartIndex
                    LocationX = New DoubleSmoothing(FaceQueueCapacity)
                    LocationY = New DoubleSmoothing(FaceQueueCapacity)
                    MissingCount = 0
                    LastFaceResult = Nothing
                End Sub
                Public ReadOnly Property ContainsNonCropped As Boolean
                    Get
                        ' returns true if at least one non-cropped item is present in the queue
                        Dim iNonCropped As Integer = Aggregate oItem In Me Where Not oItem.Item2 Into Count
                        Return iNonCropped > 0
                    End Get
                End Property
                Public Overloads Sub Enqueue(ByVal oFaceResult As FaceResult)
                    If Count >= m_Capacity Then
                        Do Until Count <= m_Capacity - 1
                            Dequeue()
                        Loop
                    End If
                    MyBase.Enqueue(New Tuple(Of Byte(), Boolean)(oFaceResult.FaceBytes, oFaceResult.Cropped))

                    ' updates the store
                    LocationX.Enqueue(oFaceResult.Location.X)
                    LocationY.Enqueue(oFaceResult.Location.Y)
                    LastFaceResult = oFaceResult.CopyResult
                    MissingCount = 0
                    UpdateTime = NTPTime.GetDateTimeNow
                End Sub
                Public Function GetSmoothedLocation() As System.Drawing.PointF
                    ' project location ahead according to the number of missing frames
                    Dim fLocationX As Double = LocationX.GetSmoothedQueueResult(SmoothAlphaFaceLocation, SmoothBetaFaceLocation, MissingCount)
                    Dim fLocationY As Double = LocationY.GetSmoothedQueueResult(SmoothAlphaFaceLocation, SmoothBetaFaceLocation, MissingCount)

                    If Double.IsNaN(fLocationX) Or Double.IsNaN(fLocationY) Then
                        Return Nothing
                    Else
                        Return New System.Drawing.PointF(fLocationX, fLocationY)
                    End If
                End Function
#Region "IDisposable Support"
                Private disposedValue As Boolean
                Protected Overridable Sub Dispose(disposing As Boolean)
                    If Not disposedValue Then
                        If disposing Then
                            Clear()
                        End If
                    End If
                    disposedValue = True
                End Sub
                Public Sub Dispose() Implements IDisposable.Dispose
                    Dispose(True)
                End Sub
#End Region
            End Class
            Public Class FaceResult
                Inherits List(Of System.Drawing.PointF)
                Implements IDisposable

                Public Region As [Structure].RotatedRect
                Public Location As System.Drawing.PointF
                Public FaceBytes As Byte()
                Public Cropped As Boolean

                Sub New()
                    Me.Clear()
                    Region = [Structure].RotatedRect.Empty
                    Location = Nothing
                    FaceBytes = Nothing
                    Cropped = False
                End Sub
                Sub New(ByVal oRegion As [Structure].RotatedRect, ByVal oPoints As List(Of System.Drawing.PointF))
                    Me.Clear()
                    Me.AddRange(oPoints)
                    Region = oRegion
                    Location = oRegion.Center
                    FaceBytes = Nothing
                    Cropped = False
                End Sub
                Public Function CopyResult() As FaceResult
                    ' copies the information aside from the distance and facebytes
                    Dim oNewFaceResult As New FaceResult
                    With oNewFaceResult
                        oNewFaceResult.AddRange(Me)
                        .Region = Region
                        .Location = Location
                        .Cropped = Cropped
                    End With
                    Return oNewFaceResult
                End Function
                Public Function UpdateResult(ByVal oNewLocation As System.Drawing.PointF) As FaceResult
                    ' updates with the new location displacement
                    Dim fDisplacementX As Double = oNewLocation.X - Location.X
                    Dim fDisplacementY As Double = oNewLocation.Y - Location.Y

                    Dim oNewFaceResult As New FaceResult
                    With oNewFaceResult
                        If Not IsNothing(Region) Then
                            .Region = New [Structure].RotatedRect(New System.Drawing.PointF(Region.Center.X + fDisplacementX, Region.Center.Y + fDisplacementY), Region.Size, Region.Angle)
                        End If

                        For Each oPoint In Me
                            oNewFaceResult.Add(New System.Drawing.PointF(oPoint.X + fDisplacementX, oPoint.Y + fDisplacementY))
                        Next

                        .Location = oNewLocation
                        .Cropped = Cropped
                    End With
                    Return oNewFaceResult
                End Function
#Region "IDisposable Support"
                Private disposedValue As Boolean ' To detect redundant calls

                ' IDisposable
                Protected Overridable Sub Dispose(disposing As Boolean)
                    If Not disposedValue Then
                        If disposing Then
                            FaceBytes = Nothing
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
            Protected Overridable Sub Dispose(disposing As Boolean)
                If Not disposedValue Then
                    If disposing Then
                        For Each oItem In Me
                            oItem.Dispose()
                        Next
                        Clear()
                    End If
                End If
                disposedValue = True
            End Sub
            Public Sub Dispose() Implements IDisposable.Dispose
                Dispose(True)
            End Sub
#End Region
        End Class
#Region "IDisposable Support"
        Private disposedValue As Boolean
        Protected Shadows Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    ' TODO: dispose managed state (managed objects).
                    FaceIdentifierStore.Dispose()
                End If
            End If
            disposedValue = True
        End Sub
        Public Shadows Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
        End Sub
#End Region
    End Class
    Public Class RotatedRectSmoothing
        Implements IDisposable, ICloneable

        Private m_x As NoSmoothing
        Private m_y As NoSmoothing
        Private m_angle As DoubleSmoothing
        Private m_width As MedianSmoothing
        Private m_height As MedianSmoothing

        Sub New(ByVal iCapacity As Integer)
            m_x = New NoSmoothing
            m_y = New NoSmoothing
            m_angle = New DoubleSmoothing(iCapacity)
            m_width = New MedianSmoothing(iCapacity)
            m_height = New MedianSmoothing(iCapacity)
        End Sub
        Sub New(ByVal oRotatedRectSmoothing As RotatedRectSmoothing)
            m_x = oRotatedRectSmoothing.m_x.Clone
            m_y = oRotatedRectSmoothing.m_y.Clone
            m_angle = oRotatedRectSmoothing.m_angle.Clone
            m_width = oRotatedRectSmoothing.m_width.Clone
            m_height = oRotatedRectSmoothing.m_height.Clone
        End Sub
        Public Function Clone() As Object Implements ICloneable.Clone
            Dim oRotatedRectSmoothing As New RotatedRectSmoothing(Me)
            Return oRotatedRectSmoothing
        End Function
        Public ReadOnly Property Valid As Boolean
            Get
                Return m_angle.Count > 0
            End Get
        End Property
        Public Overloads Sub Enqueue(ByVal oRotatedRect As [Structure].RotatedRect, ByVal fAlpha As Double, ByVal fBeta As Double)
            m_x.Enqueue(oRotatedRect.Center.X)
            m_y.Enqueue(oRotatedRect.Center.Y)

            Dim fSmoothedAngle As Single = m_angle.GetSmoothedQueueResult(fAlpha, fBeta)
            Dim fAngleDiff As Single = 0.0
            If Not Single.IsNaN(fSmoothedAngle) Then
                fAngleDiff = Math.Abs((((fSmoothedAngle - oRotatedRect.Angle) + 180.0 + 90.0) Mod 180.0) - 90.0)
            End If

            Dim fSmoothedWidth As Single = m_width.GetSmoothedQueueResult
            Dim fSmoothedHeight As Single = m_height.GetSmoothedQueueResult

            If fAngleDiff < 45.0 Then
                Dim fCorrectedAngle As Single = ((oRotatedRect.Angle + 180.0 + 90.0) Mod 180.0) - 90.0
                If Not Single.IsNaN(fSmoothedAngle) Then
                    Dim fCorrectedAngleDiff = Math.Abs((((fSmoothedAngle - fCorrectedAngle) + 180.0 + 90.0) Mod 180.0) - 90.0)
                    If fCorrectedAngleDiff <= 45.0 * SmoothingMaxFaceDifference Then
                        m_angle.Enqueue(fCorrectedAngle)
                    End If
                Else
                    m_angle.Enqueue(fCorrectedAngle)
                End If

                If (Not Single.IsNaN(fSmoothedWidth)) AndAlso (Not Single.IsNaN(fSmoothedHeight)) Then
                    Dim fMaxDiff As Single = Math.Max(Math.Max(oRotatedRect.Size.Width / fSmoothedWidth, fSmoothedWidth / oRotatedRect.Size.Width), Math.Max(oRotatedRect.Size.Height / fSmoothedHeight, fSmoothedHeight / oRotatedRect.Size.Height))
                    If fMaxDiff <= 1.0 + SmoothingMaxFaceDifference Then
                        m_width.Enqueue(oRotatedRect.Size.Width)
                        m_height.Enqueue(oRotatedRect.Size.Height)
                    End If
                Else
                    m_width.Enqueue(oRotatedRect.Size.Width)
                    m_height.Enqueue(oRotatedRect.Size.Height)
                End If
            Else
                Dim fCorrectedAngle As Single = ((oRotatedRect.Angle + 90.0F + 180.0 + 90.0) Mod 180.0) - 90.0
                If Not Single.IsNaN(fSmoothedAngle) Then
                    Dim fCorrectedAngleDiff = Math.Abs((((fSmoothedAngle - fCorrectedAngle) + 180.0 + 90.0) Mod 180.0) - 90.0)
                    If fCorrectedAngleDiff <= 45.0 * SmoothingMaxFaceDifference Then
                        m_angle.Enqueue(fCorrectedAngle)
                    End If
                Else
                    m_angle.Enqueue(fCorrectedAngle)
                End If

                If (Not Single.IsNaN(fSmoothedWidth)) AndAlso (Not Single.IsNaN(fSmoothedHeight)) Then
                    Dim fMaxDiff As Single = Math.Max(Math.Max(oRotatedRect.Size.Height / fSmoothedWidth, fSmoothedWidth / oRotatedRect.Size.Height), Math.Max(oRotatedRect.Size.Width / fSmoothedHeight, fSmoothedHeight / oRotatedRect.Size.Width))
                    If fMaxDiff <= 1.0 + SmoothingMaxFaceDifference Then
                        m_width.Enqueue(oRotatedRect.Size.Height)
                        m_height.Enqueue(oRotatedRect.Size.Width)
                    End If
                Else
                    m_width.Enqueue(oRotatedRect.Size.Height)
                    m_height.Enqueue(oRotatedRect.Size.Width)
                End If
            End If
        End Sub
        Public Function GetSmoothedQueueResult(ByVal fAlpha As Double, ByVal fBeta As Double) As [Structure].RotatedRect
            ' get the result of the smoothed value
            Return New [Structure].RotatedRect(New System.Drawing.PointF(m_x.GetSmoothedQueueResult, m_y.GetSmoothedQueueResult), New System.Drawing.SizeF(m_width.GetSmoothedQueueResult, m_height.GetSmoothedQueueResult), m_angle.GetSmoothedQueueResult(fAlpha, fBeta))
        End Function
#Region "IDisposable Support"
        Private disposedValue As Boolean
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    m_x.Dispose()
                    m_y.Dispose()
                    m_angle.Dispose()
                    m_width.Dispose()
                    m_height.Dispose()
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
    Public Class DoubleSmoothing
        Inherits Queue(Of Double)
        Implements IDisposable, ICloneable

        Private m_Capacity As Integer
        Private cacheLock As System.Threading.ReaderWriterLockSlim

        Sub New(ByVal iCapacity As Integer)
            MyBase.New(iCapacity)
            m_Capacity = iCapacity
            cacheLock = New System.Threading.ReaderWriterLockSlim
        End Sub
        Sub New(ByVal iCapacity As Integer, collection As IEnumerable(Of Double))
            MyBase.New(collection)
            m_Capacity = iCapacity
            cacheLock = New System.Threading.ReaderWriterLockSlim
        End Sub
        Public Function Clone() As Object Implements ICloneable.Clone
            Dim oDoubleSmoothing As New DoubleSmoothing(m_Capacity, Me)
            Return oDoubleSmoothing
        End Function
        Public Overloads Sub Enqueue(ByVal item As Double)
            cacheLock.EnterWriteLock()
            Try
                If Count >= m_Capacity Then
                    Do Until Count <= m_Capacity - 1
                        Dequeue()
                    Loop
                End If
                MyBase.Enqueue(item)
            Finally
                cacheLock.ExitWriteLock()
            End Try
        End Sub
        Public Function GetSmoothedQueue(ByVal fAlpha As Double, ByVal fBeta As Double, Optional ByVal iAhead As Integer = 0) As List(Of Double)
            ' runs the Holt-Winters double exponential smoothing method on the contained queue values
            Dim oQueueS As New Queue(Of Double)
            Dim oQueueB As New Queue(Of Double)

            cacheLock.EnterReadLock()
            Try
                For i = 0 To Count - 1
                    Select Case i
                        Case 0
                            oQueueS.Enqueue(Me(i))
                            oQueueB.Enqueue(Me(i))
                        Case 1
                            Dim fS1 As Double = 0
                            Dim fB1 As Double = 0
                            DoubleSmoothInit(Me(i - 1), Me(i), fS1, fB1)
                            oQueueS.Enqueue(fS1)
                            oQueueB.Enqueue(fB1)
                        Case Else
                            Dim fSt As Double = 0
                            Dim fBt As Double = 0
                            DoubleSmooth(oQueueS(i - 1), oQueueB(i - 1), fAlpha, fBeta, Me(i), fSt, fBt)
                            oQueueS.Enqueue(fSt)
                            oQueueB.Enqueue(fBt)
                    End Select
                Next

                ' forecast ahead
                If oQueueS.Count > 0 Then
                    For i = 0 To iAhead - 1
                        Dim fLast As Double = oQueueS(oQueueS.Count - 1)
                        Select Case oQueueS.Count
                            Case 1
                                oQueueS.Enqueue(fLast)
                            Case Else
                                Dim fNextLast As Double = oQueueS(oQueueS.Count - 2)
                                Dim fStep As Double = fLast - fNextLast
                                oQueueS.Enqueue(fLast + fStep)
                        End Select
                    Next
                End If
            Finally
                cacheLock.ExitReadLock()
            End Try

            Return oQueueS.ToList
        End Function
        Public Function GetSmoothedQueueResult(ByVal fAlpha As Double, ByVal fBeta As Double, Optional ByVal iAhead As Integer = 0) As Double
            ' get the result of the last smoothed value
            ' gets the predicted values iAhead steps after the last value, if supplied
            Dim oSmoothedList As List(Of Double) = GetSmoothedQueue(fAlpha, fBeta, iAhead)
            If oSmoothedList.Count > 0 Then
                Return oSmoothedList(oSmoothedList.Count - 1)
            Else
                Return Double.NaN
            End If
        End Function
        Private Shared Sub DoubleSmooth(ByVal fStp As Double, ByVal fBtp As Double, ByVal fAlpha As Double, ByVal fBeta As Double, ByVal fXt As Double, ByRef fSt As Double, ByRef fBt As Double)
            ' runs the Holt-Winters double exponential smoothing method
            fSt = (fAlpha * fXt) + ((1 - fAlpha) * (fStp + fBtp))
            fBt = (fBeta * (fSt - fStp)) + ((1 - fBeta) * fBtp)
        End Sub
        Private Shared Sub DoubleSmoothInit(ByVal fX0 As Double, ByVal fX1 As Double, ByRef fS1 As Double, ByRef fB1 As Double)
            ' initialises the Holt-Winters double exponential smoothing method
            fS1 = fX1
            fB1 = fX1 - fX0
        End Sub
#Region "IDisposable Support"
        Private disposedValue As Boolean
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    cacheLock.Dispose()
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
    Public Class MedianSmoothing
        Inherits Queue(Of Double)
        Implements IDisposable, ICloneable

        Private m_Capacity As Integer
        Private cacheLock As System.Threading.ReaderWriterLockSlim

        Sub New(ByVal iCapacity As Integer)
            MyBase.New(iCapacity)
            m_Capacity = iCapacity
            cacheLock = New System.Threading.ReaderWriterLockSlim
        End Sub
        Sub New(ByVal iCapacity As Integer, collection As IEnumerable(Of Double))
            MyBase.New(collection)
            m_Capacity = iCapacity
            cacheLock = New System.Threading.ReaderWriterLockSlim
        End Sub
        Public Function Clone() As Object Implements ICloneable.Clone
            Dim oMedianSmoothing As New MedianSmoothing(m_Capacity, Me)
            Return oMedianSmoothing
        End Function
        Public Overloads Sub Enqueue(ByVal item As Double)
            cacheLock.EnterWriteLock()
            Try
                If Count >= m_Capacity Then
                    Do Until Count <= m_Capacity - 1
                        Dequeue()
                    Loop
                End If
                MyBase.Enqueue(item)
            Finally
                cacheLock.ExitWriteLock()
            End Try
        End Sub
        Public Function GetSmoothedQueueResult() As Double
            ' get the result of the smoothed value
            If Me.Count = 0 Then
                Return Double.NaN
            Else
                Dim oSmoothedList As New List(Of Double)(Me)
                oSmoothedList.Sort()
                Dim iMedianIndex As Integer = (oSmoothedList.Count - 1) / 2
                Return oSmoothedList(iMedianIndex)
            End If
        End Function
#Region "IDisposable Support"
        Private disposedValue As Boolean
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    cacheLock.Dispose()
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
    Public Class NoSmoothing
        Implements IDisposable, ICloneable

        Private m_LastResult As Double
        Private cacheLock As System.Threading.ReaderWriterLockSlim

        Sub New()
            m_LastResult = Double.NaN
            cacheLock = New System.Threading.ReaderWriterLockSlim
        End Sub
        Sub New(ByVal item As Double)
            m_LastResult = item
            cacheLock = New System.Threading.ReaderWriterLockSlim
        End Sub
        Public Function Clone() As Object Implements ICloneable.Clone
            Dim oNoSmoothing As New NoSmoothing(m_LastResult)
            Return oNoSmoothing
        End Function
        Public Overloads Sub Enqueue(ByVal item As Double)
            cacheLock.EnterWriteLock()
            Try
                m_LastResult = item
            Finally
                cacheLock.ExitWriteLock()
            End Try
        End Sub
        Public Function GetSmoothedQueueResult() As Double
            ' get the result of the smoothed value
            Return m_LastResult
        End Function
#Region "IDisposable Support"
        Private disposedValue As Boolean
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    cacheLock.Dispose()
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
    Public Sub DepthCrop()
        Using oDepthCrop As Matrix(Of Single) = oDepthMatrix.GetSubRect(oBoundingRect)
            Dim fCenterDepth As Single = oDepthCrop((oDepthCrop.Height - 1) / 2, (oDepthCrop.Width - 1) / 2)


            Using oLowRange As New Matrix(Of Single)(oDepthCrop.Size)
                Using oHighRange As New Matrix(Of Single)(oDepthCrop.Size)
                    oLowRange.SetValue(fCenterDepth - DepthFillTolerance)
                    oHighRange.SetValue(fCenterDepth + DepthFillTolerance)
                    Using oRangeMatrix As New Matrix(Of Byte)(oDepthCrop.Size)
                        CvInvoke.InRange(oDepthCrop, oLowRange, oHighRange, oRangeMatrix)

                        CommonFunctions.SaveMatrix("D:\Downloads\Test1.tif", oRangeMatrix)

                        Using oStats As New Mat
                            Using oCentroids As New Mat
                                Dim iComponents As Integer = CvInvoke.ConnectedComponentsWithStats(oRangeMatrix, Nothing, oStats, oCentroids)

                                MsgBox("Babi")
                            End Using
                        End Using
                    End Using
                End Using
            End Using
        End Using
    End Sub
    Public Sub Test()
        CommonFunctions.SaveMatrix("D:\Downloads\Test1.tif", oMask)
        Dim oTest As New MatrixData(Of Byte)(oMask)
        Dim oTestMatrix = MatrixData(Of Byte).GetMatrixData(oTest.GetBytes).GetMatrix
        CommonFunctions.SaveMatrix("D:\Downloads\Test2.tif", oTestMatrix)
    End Sub
    Public Sub Streams()
        ComboBoxImageL.DataContext = Me
        ComboBoxImageR.DataContext = Me

        Dim oBindingStream1 As New Binding
        oBindingStream1.Path = New PropertyPath("Streams")
        oBindingStream1.Mode = BindingMode.OneWay
        oBindingStream1.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxImageL.SetBinding(HighlightComboBox.HCBContentProperty, oBindingStream1)

        Dim oBindingStream2 As New Binding
        oBindingStream2.Path = New PropertyPath("StreamL")
        oBindingStream2.Mode = BindingMode.OneWay
        oBindingStream2.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxImageL.SetBinding(HighlightComboBox.HCBTextProperty, oBindingStream2)

        Dim oBindingStream3 As New Binding
        oBindingStream3.Path = New PropertyPath("Streams")
        oBindingStream3.Mode = BindingMode.OneWay
        oBindingStream3.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxImageR.SetBinding(HighlightComboBox.HCBContentProperty, oBindingStream3)

        Dim oBindingStream4 As New Binding
        oBindingStream4.Path = New PropertyPath("StreamL")
        oBindingStream4.Mode = BindingMode.OneWay
        oBindingStream4.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged
        ComboBoxImageR.SetBinding(HighlightComboBox.HCBTextProperty, oBindingStream4)

        Private m_Streams As TrueObservableCollection(Of HCBDisplay)
        m_Streams = New TrueObservableCollection(Of HCBDisplay)(From sName In [Enum].GetNames(GetType(RealSenseCapture.StreamTypeEnum)) Select New HCBDisplay(sName, False, , [Enum].Parse(GetType(RealSenseCapture.StreamTypeEnum), sName)))
        m_StreamL = m_Streams.First
        m_StreamR = m_Streams.First
    End Sub
    Public ReadOnly Property Streams As TrueObservableCollection(Of HCBDisplay)
        Get
            Return m_Streams
        End Get
    End Property
    Public ReadOnly Property StreamL As HCBDisplay
        Get
            Return m_StreamL
        End Get
    End Property
    Public ReadOnly Property StreamR As HCBDisplay
        Get
            Return m_StreamR
        End Get
    End Property
    Private Sub ComboBoxImageL_SelectionChanged_Handler(sender As Object, e As SelectionChangedEventArgs) Handles ComboBoxImageL.SelectionChanged
        Dim oComboBoxMain As ComboBox = CType(sender, ComboBox)
        If Not IsNothing(oComboBoxMain.SelectedItem) Then
            Me.m_StreamL = oComboBoxMain.SelectedItem
        End If
    End Sub
    Private Sub ComboBoxImageR_SelectionChanged_Handler(sender As Object, e As SelectionChangedEventArgs) Handles ComboBoxImageR.SelectionChanged
        Dim oComboBoxMain As ComboBox = CType(sender, ComboBox)
        If Not IsNothing(oComboBoxMain.SelectedItem) Then
            Me.m_StreamR = oComboBoxMain.SelectedItem
        End If
    End Sub
    Private Sub RetimeVideo(ByVal sFolderName As String, ByVal sVideoName As String, ByVal fps As Integer)
        ' retimes the videos to the required fps
        Const ScaleDown As Integer = 4

        Dim oPlayDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single))) = GetPlayDictionary(sFolderName)

        Dim TaskDelegate1 As Action = Sub()
                                          Dim iColourCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Count()
                                          Dim iDepthCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Into Count()
                                          m_CaptureClipData.ColourFrames = iColourCount
                                          m_CaptureClipData.DepthFrames = iDepthCount
                                      End Sub

        CommonFunctions.SafeInvoke(TaskDelegate1, UIDispatcher, True)

        Dim iStartColour As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Min(iKey)
        Dim iStartDepth As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Into Min(iKey)
        Dim iEndColour As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Max(iKey)
        Dim iEndDepth As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Into Max(iKey)
        Dim iStart As Integer = Math.Max(iStartColour, iStartDepth)
        Dim iEnd As Integer = Math.Min(iEndColour, iEndDepth)

        Dim oColourList As List(Of Integer) = (From iKey In oPlayDictionary.Keys Where iKey >= iStart AndAlso iKey <= iEnd From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select iKey).ToList
        Dim oDepthList As List(Of Integer) = (From iKey In oPlayDictionary.Keys Where iKey >= iStart AndAlso iKey <= iEnd From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Select iKey).ToList

        Dim sColourFileName As String = (From oStream In oPlayDictionary(oColourList.First) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select oStream.Item2).First
        Dim oColourFileInfo As New IO.FileInfo(sColourFileName)
        Dim sColourName As String = Left(oColourFileInfo.Name, Len(oColourFileInfo.Name) - Len(oColourFileInfo.Extension))
        Dim sColourScale As String = Mid(sColourName, 22, 8)

        Dim sDepthFileName As String = (From oStream In oPlayDictionary(oDepthList.First) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Select oStream.Item2).First
        Dim oDepthFileInfo As New IO.FileInfo(sDepthFileName)
        Dim sDepthName As String = Left(oDepthFileInfo.Name, Len(oDepthFileInfo.Name) - Len(oDepthFileInfo.Extension))
        Dim sDepthScale As String = Mid(sDepthName, 22, 8)

        Dim sColour As String = [Enum].GetName(GetType(RealSenseCapture.StreamTypeEnum), RealSenseCapture.StreamTypeEnum.Colour)
        Dim sDepthSingle As String = [Enum].GetName(GetType(RealSenseCapture.StreamTypeEnum), RealSenseCapture.StreamTypeEnum.DepthSingle)

        Dim fInterval As Double = 1.0 / CDbl(fps)
        Dim oTargetList As New List(Of Integer)
        Dim fCurrent As Double = CDbl(iStart) / 1000.0
        Dim fEnd As Double = CDbl(Math.Min(oColourList.Max, oDepthList.Max)) / 1000.0
        Do
            oTargetList.Add(CInt(fCurrent * 1000.0))
            fCurrent += fInterval
        Loop Until fCurrent > fEnd

        Dim oColourInterpolations As Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)) = GetInterpolations(oColourList, oTargetList)
        Dim oDepthInterpolations As Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)) = GetInterpolations(oDepthList, oTargetList)

        m_CaptureClipData.ProcessingFrames = oColourInterpolations.Count + oDepthInterpolations.Count

        Dim TaskDelegate2 As Action = Sub()
                                          TextImageL.Text = "Colour"
                                      End Sub

        CommonFunctions.SafeInvoke(TaskDelegate2, UIDispatcher, True)

        Dim OpticalFlowHeight As Integer = 0
        Dim OpticalFlowWidth As Integer = 0
        Dim oFlowMatrix As Matrix(Of Single) = Nothing
        Dim oMapMatrix As Matrix(Of Single) = Nothing
        Dim bNewFlow As Boolean = False
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

                    Using oInterMatrix As Matrix(Of Byte) = GetFlowFrame(oMatrix1, oMatrix2, oFlowMatrix, oMapMatrix, bNewFlow, ScaleDown, fFraction)
                        Dim sFrameNumber As String = (i + 1).ToString("D6")
                        Dim sDuration As String = iKey.ToString("D10")
                        Dim sFilename As String = "[" + sFrameNumber + "][" + sDuration + "][" + sColourScale + "][" + sColour + "].bin"

                        Using oSmallMatrix As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth, 3)
                            CvInvoke.Resize(oInterMatrix, oSmallMatrix, oSmallMatrix.Size, 0, 0, CvEnum.Inter.Cubic)
                            m_ByteFileWriter.EnqueueByte(sRetimedName + "\" + sFilename, New MatrixData(Of Byte)(oSmallMatrix), m_CaptureClipData)
                        End Using

                        Dim sIndexFilename As String = "[" + sFrameNumber + "][" + sDuration + "][Index].jpg"
                        Dim fSmallScale As Double = SmallWidth / oInterMatrix.Width
                        Dim iSmallHeight As Integer = oInterMatrix.Height * fSmallScale
                        Using oIndexMatrix As New Matrix(Of Byte)(iSmallHeight, SmallWidth, 3)
                            CvInvoke.Resize(oInterMatrix, oIndexMatrix, oIndexMatrix.Size)
                            Dim bIndexBytes As Byte() = Converter.MatrixToJpgBytes(oIndexMatrix, 50, m_Compressor)
                            m_ByteFileWriter.EnqueueData(sRetimedName + "\" + sIndexFilename, bIndexBytes, Nothing)
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
        Next

        OpticalFlowHeight = 0
        OpticalFlowWidth = 0
        If CommonFunctions.MatrixNotNothing(oFlowMatrix) Then
            oFlowMatrix.Dispose()
            oFlowMatrix = Nothing
        End If
        If CommonFunctions.MatrixNotNothing(oMapMatrix) Then
            oMapMatrix.Dispose()
            oMapMatrix = Nothing
        End If

        Dim TaskDelegate3 As Action = Sub()
                                          TextImageL.Text = "Depth"
                                      End Sub

        CommonFunctions.SafeInvoke(TaskDelegate3, UIDispatcher, True)

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
                    Using oInterMatrix As Matrix(Of Single) = GetFlowFrame(oMatrix1, oMatrix2, oFlowMatrix, oMapMatrix, bNewFlow, ScaleDown, fFraction)
                        Dim sFrameNumber As String = (i + 1).ToString("D6")
                        Dim sDuration As String = iKey.ToString("D10")
                        Dim sFilename As String = "[" + sFrameNumber + "][" + sDuration + "][" + sDepthScale + "][" + sDepthSingle + "].bin"

                        Using oSmallMatrix As New Matrix(Of Single)(SaveVideoHeight, SaveVideoWidth)
                            CvInvoke.Resize(oInterMatrix, oSmallMatrix, oSmallMatrix.Size, 0, 0, CvEnum.Inter.Cubic)
                            m_ByteFileWriter.EnqueueData(sRetimedName + "\" + sFilename, (New MatrixData(Of Single)(oSmallMatrix)).GetBytes, m_CaptureClipData)
                        End Using

                        Dim oDepthFrameSingle As Matrix(Of Single) = oInterMatrix.Clone
                        Dim TaskDelegate As Action = Sub()
                                                         Dim oColourImage As Matrix(Of Byte) = Nothing
                                                         Converter.CreateDepthMap(oColourImage, oDepthFrameSingle, CvEnum.ColorMapType.Jet)
                                                         LImage = oColourImage

                                                         oColourImage.Dispose()
                                                         oDepthFrameSingle.Dispose()
                                                     End Sub

                        CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
                    End Using
                End Using
            End Using
        Next
        If CommonFunctions.MatrixNotNothing(oFlowMatrix) Then
            oFlowMatrix.Dispose()
            oFlowMatrix = Nothing
        End If
        If CommonFunctions.MatrixNotNothing(oMapMatrix) Then
            oMapMatrix.Dispose()
            oMapMatrix = Nothing
        End If
    End Sub
    Private Sub ConvertVideo(ByVal sFolderName As String)
        ' converts raw frames to video
        Dim oPlayDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single))) = GetPlayDictionary(sFolderName)

        Dim iColourCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Count()
        Dim iDepthCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Depth Into Count()
        m_CaptureClipData.ColourFrames = iColourCount
        m_CaptureClipData.DepthFrames = iDepthCount
        m_CaptureClipData.ProcessingFrames = iDepthCount

        m_RealSenseCapture.ConvertDepth(sFolderName, oPlayDictionary)
    End Sub
    Private Sub RetimeVideo(ByVal sFolderName As String, ByVal sRetimedName As String, ByVal fps As Integer)
        ' retimes the videos to the required fps
        Const ScaleDown As Integer = 4

        Dim oPlayDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, RS.TimestampDomain, Double, Single))) = GetPlayDictionary(sFolderName)

        Dim TaskDelegate1 As Action = Sub()
                                          Dim iColourCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Count()
                                          Dim iDepthCount As Integer = Aggregate oFrame In oPlayDictionary.Values From oStream In oFrame Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Into Count()
                                          m_CaptureClipData.ColourFrames = iColourCount
                                          m_CaptureClipData.DepthFrames = iDepthCount
                                      End Sub

        CommonFunctions.SafeInvoke(TaskDelegate1, UIDispatcher, True)

        Dim iStartColour As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Min(iKey)
        Dim iStartDepth As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Into Min(iKey)
        Dim iEndColour As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Into Max(iKey)
        Dim iEndDepth As Integer = Aggregate iKey In oPlayDictionary.Keys From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Into Max(iKey)
        Dim iStart As Integer = Math.Max(iStartColour, iStartDepth)
        Dim iEnd As Integer = Math.Min(iEndColour, iEndDepth)

        Dim oColourList As List(Of Integer) = (From iKey In oPlayDictionary.Keys Where iKey >= iStart AndAlso iKey <= iEnd From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select iKey).ToList
        Dim oDepthList As List(Of Integer) = (From iKey In oPlayDictionary.Keys Where iKey >= iStart AndAlso iKey <= iEnd From oStream In oPlayDictionary(iKey) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Select iKey).ToList

        Dim sColourFileName As String = (From oStream In oPlayDictionary(oColourList.First) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.Colour Select oStream.Item2).First
        Dim oColourFileInfo As New IO.FileInfo(sColourFileName)
        Dim sColourName As String = Left(oColourFileInfo.Name, Len(oColourFileInfo.Name) - Len(oColourFileInfo.Extension))
        Dim sColourScale As String = Mid(sColourName, 22, 8)

        Dim sDepthFileName As String = (From oStream In oPlayDictionary(oDepthList.First) Where oStream.Item3 = SegTest.Common.RealSenseCapture.StreamTypeEnum.DepthSingle Select oStream.Item2).First
        Dim oDepthFileInfo As New IO.FileInfo(sDepthFileName)
        Dim sDepthName As String = Left(oDepthFileInfo.Name, Len(oDepthFileInfo.Name) - Len(oDepthFileInfo.Extension))
        Dim sDepthScale As String = Mid(sDepthName, 22, 8)

        Dim sColour As String = [Enum].GetName(GetType(RealSenseCapture.StreamTypeEnum), RealSenseCapture.StreamTypeEnum.Colour)
        Dim sDepthSingle As String = [Enum].GetName(GetType(RealSenseCapture.StreamTypeEnum), RealSenseCapture.StreamTypeEnum.DepthSingle)

        Dim fInterval As Double = 1.0 / CDbl(fps)
        Dim oTargetList As New List(Of Integer)
        Dim fCurrent As Double = CDbl(iStart) / 1000.0
        Dim fEnd As Double = CDbl(Math.Min(oColourList.Max, oDepthList.Max)) / 1000.0
        Do
            oTargetList.Add(CInt(fCurrent * 1000.0))
            fCurrent += fInterval
        Loop Until fCurrent > fEnd

        Dim oColourInterpolations As Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)) = GetInterpolations(oColourList, oTargetList)
        Dim oDepthInterpolations As Dictionary(Of Integer, Tuple(Of Integer, Integer, Single)) = GetInterpolations(oDepthList, oTargetList)

        m_CaptureClipData.ProcessingFrames = oColourInterpolations.Count + oDepthInterpolations.Count

        Dim TaskDelegate2 As Action = Sub()
                                          TextImageL.Text = "Colour"
                                      End Sub

        CommonFunctions.SafeInvoke(TaskDelegate2, UIDispatcher, True)

        Dim OpticalFlowHeight As Integer = 0
        Dim OpticalFlowWidth As Integer = 0
        Dim oFlowMatrix As Matrix(Of Single) = Nothing
        Dim oMapMatrix As Matrix(Of Single) = Nothing
        Dim bNewFlow As Boolean = False
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

                    Using oInterMatrix As Matrix(Of Byte) = GetFlowFrame(oMatrix1, oMatrix2, oFlowMatrix, oMapMatrix, bNewFlow, ScaleDown, fFraction)
                        Dim sFrameNumber As String = (i + 1).ToString("D6")
                        Dim sDuration As String = iKey.ToString("D10")
                        Dim sFilename As String = "[" + sFrameNumber + "][" + sDuration + "][" + sColourScale + "][" + sColour + "].bin"

                        Using oSmallMatrix As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth, 3)
                            CvInvoke.Resize(oInterMatrix, oSmallMatrix, oSmallMatrix.Size, 0, 0, CvEnum.Inter.Cubic)
                            m_ByteFileWriter.EnqueueByte(sRetimedName + "\" + sFilename, New MatrixData(Of Byte)(oSmallMatrix), m_CaptureClipData)
                        End Using

                        Dim sIndexFilename As String = "[" + sFrameNumber + "][" + sDuration + "][Index].jpg"
                        Dim fSmallScale As Double = SmallWidth / oInterMatrix.Width
                        Dim iSmallHeight As Integer = oInterMatrix.Height * fSmallScale
                        Using oIndexMatrix As New Matrix(Of Byte)(iSmallHeight, SmallWidth, 3)
                            CvInvoke.Resize(oInterMatrix, oIndexMatrix, oIndexMatrix.Size)
                            Dim bIndexBytes As Byte() = Converter.MatrixToJpgBytes(oIndexMatrix, 50, m_Compressor)
                            m_ByteFileWriter.EnqueueData(sRetimedName + "\" + sIndexFilename, bIndexBytes, Nothing)
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
        Next

        OpticalFlowHeight = 0
        OpticalFlowWidth = 0
        If CommonFunctions.MatrixNotNothing(oFlowMatrix) Then
            oFlowMatrix.Dispose()
            oFlowMatrix = Nothing
        End If
        If CommonFunctions.MatrixNotNothing(oMapMatrix) Then
            oMapMatrix.Dispose()
            oMapMatrix = Nothing
        End If

        Dim TaskDelegate3 As Action = Sub()
                                          TextImageL.Text = "Depth"
                                      End Sub

        CommonFunctions.SafeInvoke(TaskDelegate3, UIDispatcher, True)

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
                    Using oInterMatrix As Matrix(Of Single) = GetFlowFrame(oMatrix1, oMatrix2, oFlowMatrix, oMapMatrix, bNewFlow, ScaleDown, fFraction)
                        Dim sFrameNumber As String = (i + 1).ToString("D6")
                        Dim sDuration As String = iKey.ToString("D10")
                        Dim sFilename As String = "[" + sFrameNumber + "][" + sDuration + "][" + sDepthScale + "][" + sDepthSingle + "].bin"

                        Using oSmallMatrix As New Matrix(Of Single)(SaveVideoHeight, SaveVideoWidth)
                            CvInvoke.Resize(oInterMatrix, oSmallMatrix, oSmallMatrix.Size, 0, 0, CvEnum.Inter.Cubic)
                            m_ByteFileWriter.EnqueueData(sRetimedName + "\" + sFilename, (New MatrixData(Of Single)(oSmallMatrix)).GetBytes, m_CaptureClipData)
                        End Using

                        Dim oDepthFrameSingle As Matrix(Of Single) = oInterMatrix.Clone
                        Dim TaskDelegate As Action = Sub()
                                                         Dim oColourImage As Matrix(Of Byte) = Nothing
                                                         Converter.CreateDepthMap(oColourImage, oDepthFrameSingle, CvEnum.ColorMapType.Jet)
                                                         LImage = oColourImage

                                                         oColourImage.Dispose()
                                                         oDepthFrameSingle.Dispose()
                                                     End Sub

                        CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
                    End Using
                End Using
            End Using
        Next
        If CommonFunctions.MatrixNotNothing(oFlowMatrix) Then
            oFlowMatrix.Dispose()
            oFlowMatrix = Nothing
        End If
        If CommonFunctions.MatrixNotNothing(oMapMatrix) Then
            oMapMatrix.Dispose()
            oMapMatrix = Nothing
        End If
    End Sub
    Private Sub SaveVideo(ByVal sFolderName As String, ByVal sRetimedName As String, ByVal oStreamType As RealSenseCapture.StreamTypeEnum, ByVal fps As Integer, ByVal bCompressed As Boolean)
        ' save retimed video to file
        Dim sStream As String = [Enum].GetName(GetType(RealSenseCapture.StreamTypeEnum), oStreamType)
        Dim sVideoFileName As String = sFolderName + "\" + VideoFolder + "\" + CommonFunctions.SafeFileName(m_CaptureClipData.Name) + "_" + sStream + ".mkv"
        Dim oRetimedDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, Single))) = GetRetimedDictionary(sRetimedName, {oStreamType}.ToList)
        If oRetimedDictionary.Count > 0 Then
            Dim oFileList As List(Of Tuple(Of Integer, String, Single)) = (From iKey In oRetimedDictionary.Keys From oStream In oRetimedDictionary(iKey) Where oStream.Item3 = oStreamType Order By iKey Ascending Select New Tuple(Of Integer, String, Single)(iKey, oStream.Item2, oStream.Item4)).ToList

            m_CaptureClipData.ProcessingFrames = oFileList.Count

            If bCompressed Then
                Using oVideoWriter As New Accord.Video.FFMPEG.VideoFileWriter()
                    oVideoWriter.Open(sVideoFileName, SaveVideoWidth, SaveVideoHeight, New Rational(fps), Accord.Video.FFMPEG.VideoCodec.H264, 4000000)
                    Using oResizedMatrix As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth, 3)
                        For Each oStream In oFileList
                            Dim oBytes As Byte() = IO.File.ReadAllBytes(oStream.Item2)
                            Using oSourceMatrix = MatrixData(Of Byte).GetMatrixData(oBytes).GetMatrix
                                Select Case oSourceMatrix.NumberOfChannels
                                    Case 3
                                        CvInvoke.Resize(oSourceMatrix, oResizedMatrix, oResizedMatrix.Size, 0, 0, CvEnum.Inter.Lanczos4)
                                    Case 1
                                        Using oResizedMonoMatrix As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth, 1)
                                            CvInvoke.Resize(oSourceMatrix, oResizedMonoMatrix, oResizedMonoMatrix.Size, 0, 0, CvEnum.Inter.Lanczos4)
                                            CvInvoke.Threshold(oResizedMonoMatrix, oResizedMonoMatrix, 127, 255, CvEnum.ThresholdType.Binary)
                                            CvInvoke.CvtColor(oResizedMonoMatrix, oResizedMatrix, CvEnum.ColorConversion.Gray2Bgr)
                                        End Using
                                End Select

                                Using oResizedBitmap As System.Drawing.Bitmap = Converter.MatToBitmap(oResizedMatrix.Mat, 300)
                                    oVideoWriter.WriteVideoFrame(oResizedBitmap)
                                End Using

                                m_CaptureClipData.ProcessingFrames -= 1
                            End Using
                        Next
                    End Using
                End Using
            Else
                Using oVideoWriter As New VideoWriter(sVideoFileName, FourCC.FFV1, fps, New System.Drawing.Size(SaveVideoWidth, SaveVideoHeight), True)
                    Using oResizedMatrix As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth, 3)
                        For Each oStream In oFileList
                            Dim oBytes As Byte() = IO.File.ReadAllBytes(oStream.Item2)
                            Using oSourceMatrix = MatrixData(Of Byte).GetMatrixData(oBytes).GetMatrix
                                Select Case oSourceMatrix.NumberOfChannels
                                    Case 3
                                        CvInvoke.Resize(oSourceMatrix, oResizedMatrix, oResizedMatrix.Size, 0, 0, CvEnum.Inter.Lanczos4)
                                    Case 1
                                        Using oResizedMonoMatrix As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth, 1)
                                            CvInvoke.Resize(oSourceMatrix, oResizedMonoMatrix, oResizedMonoMatrix.Size, 0, 0, CvEnum.Inter.Lanczos4)
                                            CvInvoke.Threshold(oResizedMonoMatrix, oResizedMonoMatrix, 127, 255, CvEnum.ThresholdType.Binary)
                                            CvInvoke.CvtColor(oResizedMonoMatrix, oResizedMatrix, CvEnum.ColorConversion.Gray2Bgr)
                                        End Using
                                End Select

                                oVideoWriter.Write(oResizedMatrix.Mat)

                                m_CaptureClipData.ProcessingFrames -= 1
                            End Using
                        Next
                    End Using
                End Using
            End If
        End If
    End Sub
    Private Sub GroundTruth(ByVal sFolderName As String, ByVal sRetimedName As String)
        ' gets the ground truth from matched colour and depth videos
        Dim oColorRed As [Structure].MCvScalar = New [Structure].Bgr(System.Drawing.Color.Red).MCvScalar

        Dim oRetimedDictionary As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, Single))) = GetRetimedDictionary(sRetimedName, {RealSenseCapture.StreamTypeEnum.Colour, RealSenseCapture.StreamTypeEnum.DepthSingle}.ToList)
        If oRetimedDictionary.Count > 0 Then
            Dim iProcessing As Integer = Aggregate iKey In oRetimedDictionary.Keys Into Count(oRetimedDictionary(iKey).Count = 2)

            m_CaptureClipData.ColourFrames = iProcessing
            m_CaptureClipData.DepthFrames = iProcessing
            m_CaptureClipData.ProcessingFrames = iProcessing * 2

            Dim TaskDelegate1 As Action = Sub()
                                              TextImageL.Text = "Colour"
                                          End Sub
            CommonFunctions.SafeInvoke(TaskDelegate1, UIDispatcher, True)

            Dim oBoundingDict As New Dictionary(Of Integer, [Structure].RotatedRect)
            For Each iKey In oRetimedDictionary.Keys
                Dim oColourFrame As Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, Single) = (From oStream In oRetimedDictionary(iKey) Where oStream.Item3 = RealSenseCapture.StreamTypeEnum.Colour Select oStream).First
                Dim oColourBytes As Byte() = IO.File.ReadAllBytes(oColourFrame.Item2)
                Dim oColourMatrix = MatrixData(Of Byte).GetMatrixData(oColourBytes).GetMatrix
                Dim oFaceResults As List(Of Processor.FaceResult) = m_Processor.DetectFaces(oColourMatrix)
                If oFaceResults.Count = 1 Then
                    Dim oRectPresentList As List(Of Integer) = (From iBoundingKey In oBoundingDict.Keys Where Not oBoundingDict(iBoundingKey).Size.IsEmpty Order By iBoundingKey Descending Select iBoundingKey).ToList
                    If oRectPresentList.Count > 0 Then
                        ' check to see if center of face falls within the bounds of the previous face
                        If oBoundingDict(oRectPresentList.First).MinAreaRect.Contains(New System.Drawing.Point(oFaceResults.First.Region.Center.X, oFaceResults.First.Region.Center.Y)) Then
                            oBoundingDict.Add(iKey, oFaceResults.First.Region)
                        Else
                            oBoundingDict.Add(iKey, New [Structure].RotatedRect)
                        End If
                    Else
                        oBoundingDict.Add(iKey, oFaceResults.First.Region)
                    End If
                Else
                    oBoundingDict.Add(iKey, New [Structure].RotatedRect)
                End If

                If Not m_Processing Then
                    m_Processing = True

                    Dim TaskDelegate As Action = Sub()
                                                     If Not oBoundingDict(iKey).Size.IsEmpty Then
                                                         Dim fLineScale As Single = Math.Max(Math.Sqrt((oColourMatrix.Width * oColourMatrix.Width) + (oColourMatrix.Height * oColourMatrix.Height)) / 1000, 2)
                                                         Dim oVertices As System.Drawing.Point() = oBoundingDict(iKey).GetVertices.Select(Function(oPoint) New System.Drawing.Point(oPoint.X, oPoint.Y)).ToArray
                                                         CvInvoke.Polylines(oColourMatrix, oVertices, True, oColorRed, 1 * fLineScale)
                                                     End If

                                                     LImage = oColourMatrix

                                                     oColourMatrix.Dispose()

                                                     m_CaptureClipData.ProcessingFrames -= 1

                                                     m_Processing = False
                                                 End Sub

                    CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
                Else
                    oColourMatrix.Dispose()
                End If
            Next

            ' fill in gaps
            Dim iLastFound As Integer = -1
            For i = 0 To oBoundingDict.Keys.Count - 2
                Dim iKey As Integer = oBoundingDict.Keys(i)

                ' search forward for next valid rect
                If iLastFound <> -1 AndAlso oBoundingDict(iKey).Size.IsEmpty Then
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

                If Not oBoundingDict(iKey).Size.IsEmpty Then
                    iLastFound = iKey
                End If
            Next

            ' process depth
            Dim TaskDelegate2 As Action = Sub()
                                              TextImageL.Text = "Depth Mask"
                                          End Sub
            CommonFunctions.SafeInvoke(TaskDelegate2, UIDispatcher, True)

            For Each iKey In oRetimedDictionary.Keys
                Dim oDepthFrame As Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, Single) = (From oStream In oRetimedDictionary(iKey) Where oStream.Item3 = RealSenseCapture.StreamTypeEnum.DepthSingle Select oStream).First
                Dim oDepthBytes As Byte() = IO.File.ReadAllBytes(oDepthFrame.Item2)
                Dim oDepthMatrix = MatrixData(Of Single).GetMatrixData(oDepthBytes).GetMatrix

                Using oMaskExpanded As New Matrix(Of Byte)(oDepthMatrix.Height + 2, oDepthMatrix.Width + 2)
                    If Not oBoundingDict(iKey).Size.IsEmpty Then
                        Dim oBounds As New System.Drawing.Rectangle
                        CvInvoke.FloodFill(oDepthMatrix, oMaskExpanded, New System.Drawing.Point(oBoundingDict(iKey).Center.X, oBoundingDict(iKey).Center.Y), New [Structure].MCvScalar(255), oBounds, New [Structure].MCvScalar(DepthFillTolerance), New [Structure].MCvScalar(DepthFillTolerance), CvEnum.Connectivity.FourConnected, CvEnum.FloodFillType.MaskOnly)

                        Using oMask As Matrix(Of Byte) = oMaskExpanded.GetSubRect(New System.Drawing.Rectangle(1, 1, oDepthMatrix.Width, oDepthMatrix.Height))
                            Dim fMax As Double = 0
                            Dim fMin As Double = 0
                            CvInvoke.MinMaxLoc(oMask, fMin, fMax, Nothing, Nothing)
                            If fMin = fMax Then
                                oMask.SetZero()
                            Else
                                CvInvoke.Normalize(oMask, oMask, 0, 255, CvEnum.NormType.MinMax)
                            End If

                            ' watershed processing
                            If m_CaptureClipData.Watershed Then
                                Using oOuterMask As Matrix(Of Byte) = oMask.Clone
                                    Using oInnerMask As Matrix(Of Byte) = oMask.Clone
                                        CvInvoke.MorphologyEx(oOuterMask, oOuterMask, CvEnum.MorphOp.Dilate, CvInvoke.GetStructuringElement(CvEnum.ElementShape.Rectangle, New System.Drawing.Size(3, 3), New System.Drawing.Point(-1, -1)), New System.Drawing.Point(-1, -1), WatershedMargin, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(Byte.MinValue))
                                        CvInvoke.MorphologyEx(oInnerMask, oInnerMask, CvEnum.MorphOp.Erode, CvInvoke.GetStructuringElement(CvEnum.ElementShape.Rectangle, New System.Drawing.Size(3, 3), New System.Drawing.Point(-1, -1)), New System.Drawing.Point(-1, -1), WatershedMargin, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(Byte.MinValue))
                                        CvInvoke.BitwiseNot(oOuterMask, oOuterMask)
                                        Using oMarkerMatrix As New Matrix(Of Integer)(oMask.Size)
                                            oMarkerMatrix.SetZero()
                                            oMarkerMatrix.SetValue(2, oInnerMask)
                                            oMarkerMatrix.SetValue(1, oOuterMask)

                                            Dim oColourFrame As Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, Single) = (From oStream In oRetimedDictionary(iKey) Where oStream.Item3 = RealSenseCapture.StreamTypeEnum.Colour Select oStream).First
                                            Dim oColourBytes As Byte() = IO.File.ReadAllBytes(oColourFrame.Item2)
                                            Using oColourMatrix = MatrixData(Of Byte).GetMatrixData(oColourBytes).GetMatrix
                                                CvInvoke.Watershed(oColourMatrix, oMarkerMatrix)

                                                Using oCompareMatrix As New Matrix(Of Integer)(oMask.Size)
                                                    oCompareMatrix.SetValue(1)
                                                    CvInvoke.Compare(oMarkerMatrix, oCompareMatrix, oOuterMask, CvEnum.CmpType.Equal)

                                                    oCompareMatrix.SetValue(2)
                                                    CvInvoke.Compare(oMarkerMatrix, oCompareMatrix, oInnerMask, CvEnum.CmpType.Equal)

                                                    Using oMarginMask As New Matrix(Of Byte)(oMask.Size)
                                                        oCompareMatrix.SetValue(-1)
                                                        CvInvoke.Compare(oMarkerMatrix, oCompareMatrix, oMarginMask, CvEnum.CmpType.Equal)

                                                        ' get median BGR values for inner and outer mask
                                                        Dim oMedianBGROuter As New [Structure].Bgr(0, 0, 0)
                                                        Dim oMedianBGRInner As New [Structure].Bgr(0, 0, 0)

                                                        Using oBMatrix As New Matrix(Of Byte)(oColourMatrix.Size)
                                                            Using oGMatrix As New Matrix(Of Byte)(oColourMatrix.Size)
                                                                Using oRMatrix As New Matrix(Of Byte)(oColourMatrix.Size)
                                                                    Using oVector As New Util.VectorOfMat
                                                                        CvInvoke.Split(oColourMatrix, oVector)
                                                                        oVector(0).CopyTo(oBMatrix)
                                                                        oVector(1).CopyTo(oGMatrix)
                                                                        oVector(2).CopyTo(oRMatrix)
                                                                    End Using

                                                                    Dim oBOuterList As New List(Of Byte)
                                                                    Dim oGOuterList As New List(Of Byte)
                                                                    Dim oROuterList As New List(Of Byte)
                                                                    Dim oBInnerList As New List(Of Byte)
                                                                    Dim oGInnerList As New List(Of Byte)
                                                                    Dim oRInnerList As New List(Of Byte)

                                                                    For y = 0 To oMask.Height - 1
                                                                        For x = 0 To oMask.Width - 1
                                                                            If oOuterMask(y, x) > 0 Then
                                                                                oBOuterList.Add(oBMatrix(y, x))
                                                                                oGOuterList.Add(oGMatrix(y, x))
                                                                                oROuterList.Add(oRMatrix(y, x))
                                                                            ElseIf oInnerMask(y, x) > 0 Then
                                                                                oBInnerList.Add(oBMatrix(y, x))
                                                                                oGInnerList.Add(oGMatrix(y, x))
                                                                                oRInnerList.Add(oRMatrix(y, x))
                                                                            End If
                                                                        Next
                                                                    Next

                                                                    If oBOuterList.Count > 0 Then
                                                                        oBOuterList.Sort()
                                                                        oGOuterList.Sort()
                                                                        oROuterList.Sort()
                                                                        Dim iMedianIndex As Integer = (oBOuterList.Count - 1) / 2
                                                                        oMedianBGROuter = New [Structure].Bgr(oBOuterList(iMedianIndex), oGOuterList(iMedianIndex), oROuterList(iMedianIndex))
                                                                    End If
                                                                    If oBInnerList.Count > 0 Then
                                                                        oBInnerList.Sort()
                                                                        oGInnerList.Sort()
                                                                        oRInnerList.Sort()
                                                                        Dim iMedianIndex As Integer = (oBInnerList.Count - 1) / 2
                                                                        oMedianBGRInner = New [Structure].Bgr(oBInnerList(iMedianIndex), oGInnerList(iMedianIndex), oRInnerList(iMedianIndex))
                                                                    End If

                                                                    ' set margin mask pixels
                                                                    For y = 0 To oMask.Height - 1
                                                                        For x = 0 To oMask.Width - 1
                                                                            If oMarginMask(y, x) > 0 Then
                                                                                Dim oPixel As New [Structure].Bgr(oBMatrix(y, x), oGMatrix(y, x), oRMatrix(y, x))
                                                                                Dim fDistanceOuter As Single = GetColourDistance(oMedianBGROuter, oPixel)
                                                                                Dim fDistanceInner As Single = GetColourDistance(oMedianBGRInner, oPixel)

                                                                                If fDistanceOuter <= fDistanceInner Then
                                                                                    oOuterMask(y, x) = 255
                                                                                Else
                                                                                    oInnerMask(y, x) = 255
                                                                                End If
                                                                            End If
                                                                        Next
                                                                    Next

                                                                    oInnerMask.CopyTo(oMask)
                                                                End Using
                                                            End Using
                                                        End Using
                                                    End Using
                                                End Using
                                            End Using
                                        End Using
                                    End Using
                                End Using
                            End If

                            Dim sFrameNumber As String = oDepthFrame.Item1.ToString("D6")
                            Dim sDuration As String = iKey.ToString("D10")
                            Dim sDepthScale As String = 0.ToString("E1", Globalization.CultureInfo.CreateSpecificCulture("en-US"))
                            Dim sDepthMask As String = [Enum].GetName(GetType(RealSenseCapture.StreamTypeEnum), RealSenseCapture.StreamTypeEnum.DepthMask)
                            Dim sFilename As String = "[" + sFrameNumber + "][" + sDuration + "][" + sDepthScale + "][" + sDepthMask + "].bin"

                            m_ByteFileWriter.EnqueueByte(sRetimedName + "\" + sFilename, (New MatrixData(Of Byte)(oMask)), m_CaptureClipData)

                            Dim oDepthFrameMask As Matrix(Of Byte) = oMask.Clone
                            Dim TaskDelegate As Action = Sub()
                                                             LImage = oDepthFrameMask
                                                             oDepthFrameMask.Dispose()
                                                         End Sub

                            CommonFunctions.SafeInvoke(TaskDelegate, UIDispatcher, True)
                        End Using
                    End If
                End Using
            Next
        End If
    End Sub
    Private Sub IgnoreDepth()
        If m_CaptureClipData.IgnoreDepth Then
            m_CaptureClipData.ColourFrames = iColourCount
            m_CaptureClipData.DepthFrames = iDepthCount
            m_CaptureClipData.ProcessingFrames = iTotalFrames

            ' write blank depth video
            Using oDepthVideoWriter As New Accord.Video.FFMPEG.VideoFileWriter
                oDepthVideoWriter.Open(sDepthVideoFileName, SaveVideoWidth, SaveVideoHeight, New Rational(fps), Accord.Video.FFMPEG.VideoCodec.FFV1)

                Using oResizedMask As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth)
                    oResizedMask.SetZero()
                    Using oSmallBitmap As System.Drawing.Bitmap = Converter.MatToBitmap(oResizedMask.Mat, 300)
                        For i = 0 To oDepthInterpolations.Keys.Count - 1
                            oDepthVideoWriter.WriteVideoFrame(oSmallBitmap)
                            m_CaptureClipData.ProcessingFrames -= 1
                        Next
                    End Using
                End Using
            End Using

            Return True
        Else
        End If
    End Sub
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
                                                Using oInterMatrix As Matrix(Of Single) = GetFlowFrame(oMatrix1, oMatrix2, oFlowMatrix, oMapMatrix, bNewFlow, ScaleDown, fFraction)
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
                                                                                CvInvoke.Normalize(oMask, oMask, 0, 255, CvEnum.NormType.MinMax)
                                                                            End If

                                                                            ' watershed processing
                                                                            If m_CaptureClipData.Watershed Then
                                                                                Using oOuterMask As Matrix(Of Byte) = oMask.Clone
                                                                                    Using oInnerMask As Matrix(Of Byte) = oMask.Clone
                                                                                        CvInvoke.MorphologyEx(oOuterMask, oOuterMask, CvEnum.MorphOp.Dilate, CvInvoke.GetStructuringElement(CvEnum.ElementShape.Rectangle, New System.Drawing.Size(3, 3), New System.Drawing.Point(-1, -1)), New System.Drawing.Point(-1, -1), WatershedMarginOuter, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(Byte.MinValue))
                                                                                        CvInvoke.MorphologyEx(oInnerMask, oInnerMask, CvEnum.MorphOp.Erode, CvInvoke.GetStructuringElement(CvEnum.ElementShape.Rectangle, New System.Drawing.Size(3, 3), New System.Drawing.Point(-1, -1)), New System.Drawing.Point(-1, -1), WatershedMarginInner, CvEnum.BorderType.Replicate, New [Structure].MCvScalar(Byte.MinValue))
                                                                                        CvInvoke.BitwiseNot(oOuterMask, oOuterMask)
                                                                                        Using oMarkerMatrix As New Matrix(Of Integer)(oMask.Size)
                                                                                            oMarkerMatrix.SetZero()
                                                                                            oMarkerMatrix.SetValue(2, oInnerMask)
                                                                                            oMarkerMatrix.SetValue(1, oOuterMask)

                                                                                            Using oVector As New Util.VectorOfMat
                                                                                                Using oCLAHEMatrix As New Matrix(Of Byte)(oColourMatrix.Height, oColourMatrix.Width, 3)
                                                                                                    ' run CLAHE on luminance channel before watershed
                                                                                                    CvInvoke.CvtColor(oColourMatrix, oCLAHEMatrix, CvEnum.ColorConversion.Bgr2Lab)
                                                                                                    CvInvoke.Split(oCLAHEMatrix, oVector)
                                                                                                    CvInvoke.CLAHE(oVector(0), 2, New System.Drawing.Size(8, 8), oVector(0))
                                                                                                    CvInvoke.Merge(oVector, oCLAHEMatrix)
                                                                                                    CvInvoke.CvtColor(oCLAHEMatrix, oCLAHEMatrix, CvEnum.ColorConversion.Lab2Bgr)

                                                                                                    CvInvoke.Watershed(oCLAHEMatrix, oMarkerMatrix)
                                                                                                End Using
                                                                                            End Using

                                                                                            Using oCompareMatrix As New Matrix(Of Integer)(oMask.Size)
                                                                                                oCompareMatrix.SetValue(1)
                                                                                                CvInvoke.Compare(oMarkerMatrix, oCompareMatrix, oOuterMask, CvEnum.CmpType.Equal)

                                                                                                oCompareMatrix.SetValue(2)
                                                                                                CvInvoke.Compare(oMarkerMatrix, oCompareMatrix, oInnerMask, CvEnum.CmpType.Equal)

                                                                                                Using oMarginMask As New Matrix(Of Byte)(oMask.Size)
                                                                                                    oCompareMatrix.SetValue(-1)
                                                                                                    CvInvoke.Compare(oMarkerMatrix, oCompareMatrix, oMarginMask, CvEnum.CmpType.Equal)

                                                                                                    ' get median BGR values for inner and outer mask
                                                                                                    Dim oMedianBGROuter As New [Structure].Bgr(0, 0, 0)
                                                                                                    Dim oMedianBGRInner As New [Structure].Bgr(0, 0, 0)

                                                                                                    Using oBMatrix As New Matrix(Of Byte)(oColourMatrix.Size)
                                                                                                        Using oGMatrix As New Matrix(Of Byte)(oColourMatrix.Size)
                                                                                                            Using oRMatrix As New Matrix(Of Byte)(oColourMatrix.Size)
                                                                                                                Using oVector As New Util.VectorOfMat
                                                                                                                    CvInvoke.Split(oColourMatrix, oVector)
                                                                                                                    oVector(0).CopyTo(oBMatrix)
                                                                                                                    oVector(1).CopyTo(oGMatrix)
                                                                                                                    oVector(2).CopyTo(oRMatrix)
                                                                                                                End Using

                                                                                                                Dim oBOuterList As New List(Of Byte)
                                                                                                                Dim oGOuterList As New List(Of Byte)
                                                                                                                Dim oROuterList As New List(Of Byte)
                                                                                                                Dim oBInnerList As New List(Of Byte)
                                                                                                                Dim oGInnerList As New List(Of Byte)
                                                                                                                Dim oRInnerList As New List(Of Byte)

                                                                                                                For y = 0 To oMask.Height - 1
                                                                                                                    For x = 0 To oMask.Width - 1
                                                                                                                        If oOuterMask(y, x) > 0 Then
                                                                                                                            oBOuterList.Add(oBMatrix(y, x))
                                                                                                                            oGOuterList.Add(oGMatrix(y, x))
                                                                                                                            oROuterList.Add(oRMatrix(y, x))
                                                                                                                        ElseIf oInnerMask(y, x) > 0 Then
                                                                                                                            oBInnerList.Add(oBMatrix(y, x))
                                                                                                                            oGInnerList.Add(oGMatrix(y, x))
                                                                                                                            oRInnerList.Add(oRMatrix(y, x))
                                                                                                                        End If
                                                                                                                    Next
                                                                                                                Next

                                                                                                                If oBOuterList.Count > 0 Then
                                                                                                                    oBOuterList.Sort()
                                                                                                                    oGOuterList.Sort()
                                                                                                                    oROuterList.Sort()
                                                                                                                    Dim iMedianIndex As Integer = (oBOuterList.Count - 1) / 2
                                                                                                                    oMedianBGROuter = New [Structure].Bgr(oBOuterList(iMedianIndex), oGOuterList(iMedianIndex), oROuterList(iMedianIndex))
                                                                                                                End If
                                                                                                                If oBInnerList.Count > 0 Then
                                                                                                                    oBInnerList.Sort()
                                                                                                                    oGInnerList.Sort()
                                                                                                                    oRInnerList.Sort()
                                                                                                                    Dim iMedianIndex As Integer = (oBInnerList.Count - 1) / 2
                                                                                                                    oMedianBGRInner = New [Structure].Bgr(oBInnerList(iMedianIndex), oGInnerList(iMedianIndex), oRInnerList(iMedianIndex))
                                                                                                                End If

                                                                                                                ' set margin mask pixels
                                                                                                                For y = 0 To oMask.Height - 1
                                                                                                                    For x = 0 To oMask.Width - 1
                                                                                                                        If oMarginMask(y, x) > 0 Then
                                                                                                                            Dim oPixel As New [Structure].Bgr(oBMatrix(y, x), oGMatrix(y, x), oRMatrix(y, x))
                                                                                                                            Dim fDistanceOuter As Single = GetColourDistance(oMedianBGROuter, oPixel)
                                                                                                                            Dim fDistanceInner As Single = GetColourDistance(oMedianBGRInner, oPixel)

                                                                                                                            If fDistanceOuter <= fDistanceInner Then
                                                                                                                                oOuterMask(y, x) = 255
                                                                                                                            Else
                                                                                                                                oInnerMask(y, x) = 255
                                                                                                                            End If
                                                                                                                        End If
                                                                                                                    Next
                                                                                                                Next

                                                                                                                oInnerMask.CopyTo(oMask)
                                                                                                            End Using
                                                                                                        End Using
                                                                                                    End Using
                                                                                                End Using
                                                                                            End Using
                                                                                        End Using
                                                                                    End Using
                                                                                End Using
                                                                            End If

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
    Private Function GetColourDistance(ByVal oPixel1 As [Structure].Bgr, ByVal oPixel2 As [Structure].Bgr) As Single
        ' gets colour distance between two bgr values
        Return Math.Sqrt((oPixel2.Blue - oPixel1.Blue) * (oPixel2.Blue - oPixel1.Blue) + (oPixel2.Green - oPixel1.Green) * (oPixel2.Green - oPixel1.Green) + (oPixel2.Red - oPixel1.Red) * (oPixel2.Red - oPixel1.Red))
    End Function
    Private Sub ClearClips()
        For i = 0 To oClipDataList.Count - 1
            With oClipDataList(i)
                .Accuracy.Clear()
                .SetAccuracy()
                .ProcessedCut = False
                .ProcessedGroundTruth = False
                .ProcessedSegmented = False
                .ProcessedVideo = False
            End With
        Next
    End Sub
    Private Function GetRetimedDictionary(ByVal sFolderName As String, ByVal oStreamList As List(Of RealSenseCapture.StreamTypeEnum)) As Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, Single)))
        ' gets the play dictionary from a folder for further processing
        Dim oDirectoryInfo As New IO.DirectoryInfo(sFolderName)
        Dim oFileInfoList As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("[??????][??????????][?.?E????][*].bin").ToList

        ' sort files into a dictionary
        Dim oRetimedDictionary As New Dictionary(Of Integer, List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, Single)))
        For Each oFileInfo In oFileInfoList
            Dim sName As String = Left(oFileInfo.Name, Len(oFileInfo.Name) - Len(oFileInfo.Extension))
            Dim sKey As String = Mid(sName, 2, 6)
            Dim sValue As String = Mid(sName, 10, 10)
            Dim sDepthScale As String = Mid(sName, 22, 8)
            Dim sStream As String = Mid(sName, 32, Len(sName) - 32)
            Dim iKey As Integer = 0
            Dim iValue As Integer = 0
            Dim fDepthScale As Single = 0
            Dim oStreamType As RealSenseCapture.StreamTypeEnum = RealSenseCapture.StreamTypeEnum.None
            If Integer.TryParse(sKey, iKey) AndAlso Integer.TryParse(sValue, iValue) AndAlso Single.TryParse(sDepthScale, fDepthScale) AndAlso [Enum].TryParse(Of RealSenseCapture.StreamTypeEnum)(sStream, oStreamType) Then
                If oStreamList.Contains(oStreamType) Then
                    If Not oRetimedDictionary.ContainsKey(iValue) Then
                        oRetimedDictionary.Add(iValue, New List(Of Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, Single)))
                    End If
                    oRetimedDictionary(iValue).Add(New Tuple(Of Integer, String, RealSenseCapture.StreamTypeEnum, Single)(iKey, oFileInfo.FullName, oStreamType, fDepthScale))
                End If
            End If
        Next

        ' sort by time
        oRetimedDictionary = (From oKeyValuePair In oRetimedDictionary Order By oKeyValuePair.Key Ascending Select oKeyValuePair).ToDictionary(Function(x) x.Key, Function(x) x.Value)

        Return oRetimedDictionary
    End Function
    Private Function GetSortedVertices(ByVal oRotatedRect As [Structure].RotatedRect) As System.Drawing.PointF()
        Dim oSource As List(Of System.Drawing.PointF) = oRotatedRect.GetVertices.ToList
        Dim oSourceSorted As System.Drawing.PointF() = {System.Drawing.PointF.Empty, System.Drawing.PointF.Empty, System.Drawing.PointF.Empty, System.Drawing.PointF.Empty}
        For i = 0 To 3
            Dim fAngle As Single = Math.Atan2(oSource(i).Y - oRotatedRect.Center.Y, oSource(i).X - oRotatedRect.Center.X) * 180 / Math.PI
            Dim fAngleDiff As Single = (fAngle - oRotatedRect.Angle + 360) Mod 360
            If fAngleDiff < 90 Then
                oSourceSorted(2) = oSource(i)
            ElseIf fAngleDiff < 180 Then
                oSourceSorted(3) = oSource(i)
            ElseIf fAngleDiff < 270 Then
                oSourceSorted(0) = oSource(i)
            Else
                oSourceSorted(1) = oSource(i)
            End If
        Next
        Return oSourceSorted
    End Function
    Private Sub ChangeSelections()
        Dim oTest = m_FrameDisplay.GetCurrentFrame
        Dim oFrame As New Tuple(Of Matrix(Of Byte), Matrix(Of Byte), String, List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)))(oTest.Item1, oTest.Item2, oTest.Item3, New List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)))


        Dim oNewSelections As New Dictionary(Of Integer, List(Of Tuple(Of List(Of Point), Point, Double, MainPage.SelectionTypeEnum)))
        For i = 0 To oSelections.Keys.Count - 1
            Dim oOldList As List(Of Tuple(Of List(Of Point), Point, Double, SelectionTypeEnum)) = oSelections(oSelections.Keys(i))

            Dim oNewList As New List(Of Tuple(Of List(Of Point), Point, Double, MainPage.SelectionTypeEnum))
            For j = 0 To oOldList.Count - 1
                oNewList.Add(New Tuple(Of List(Of Point), Point, Double, MainPage.SelectionTypeEnum)(oOldList(j).Item1, oOldList(j).Item2, oOldList(j).Item3, oOldList(j).Item4))
            Next

            oNewSelections.Add(oSelections.Keys(i), oNewList)
        Next
        Dim sDUPFileName As String = sFolderName + "\" + SelectionFileName + "dup"
        CommonFunctions.SerializeDataContractFile(sDUPFileName, oNewSelections)
    End Sub
    <DataContract()> Public Class Results
        <DataMember> Public Results As List(Of Result)

        Sub New()
            Results = New List(Of Result)
        End Sub
        Public ReadOnly Property Empty
            Get
                Return Results.Count = 0
            End Get
        End Property
        Public Shared Function GetEmpty()
            Return New Results()
        End Function
        <DataContract()> Public Structure Result
            <DataMember> Public TP As Integer
            <DataMember> Public TN As Integer
            <DataMember> Public FP As Integer
            <DataMember> Public FN As Integer
            <DataMember> Public ProcessTime As TimeSpan

            Sub New(ByVal iTP As Integer, ByVal iTN As Integer, ByVal iFP As Integer, ByVal iFN As Integer, ByVal oProcessTime As TimeSpan)
                TP = iTP
                TN = iTN
                FP = iFP
                FN = iFN
                ProcessTime = oProcessTime
            End Sub
            Public Shared Function GetKnownTypes() As List(Of Type)
                ' returns the list of additonal types
                Return New List(Of Type) From {GetType(TimeSpan)}
            End Function
        End Structure



        Public Shared Function GetKnownTypes() As List(Of Type)
            ' returns the list of additonal types
            Return New List(Of Type) From {GetType(TimeSpan)}
        End Function
    End Class
    <DataContract()> Public Structure Results
        <DataMember> Public TP As Integer
        <DataMember> Public TN As Integer
        <DataMember> Public FP As Integer
        <DataMember> Public FN As Integer
        <DataMember> Public ProcessTime As TimeSpan
        <DataMember> Public ProcessFrames As Integer

        Sub New(ByVal iTP As Integer, ByVal iTN As Integer, ByVal iFP As Integer, ByVal iFN As Integer, ByVal oProcessTime As TimeSpan, ByVal iProcessFrames As Integer)
            TP = iTP
            TN = iTN
            FP = iFP
            FN = iFN
            ProcessTime = oProcessTime
            ProcessFrames = iProcessFrames
        End Sub
        Public ReadOnly Property Empty
            Get
                Return TP < 0 OrElse TN < 0 OrElse FP < 0 OrElse FN < 0 OrElse ProcessFrames < 0
            End Get
        End Property
        Public Shared Function GetEmpty()
            Return New Results(-1, -1, -1, -1, TimeSpan.MinValue, -1)
        End Function
        Public Shared Function GetKnownTypes() As List(Of Type)
            ' returns the list of additonal types
            Return New List(Of Type) From {GetType(TimeSpan)}
        End Function
    End Structure
    Public ReadOnly Property ProcessedAccuracy As Boolean
        Get
            Dim iEmptyAlgorithms As Integer = Aggregate oAccuracy In m_Accuracy Where Not ExcludedSegTypes.Contains(oAccuracy.Key) Into Count(oAccuracy.Value.Empty)
            Dim bAllAlgorithms As Boolean = (m_Accuracy.Count = [Enum].GetNames(GetType(SegTypeEnum)).Count)
            Return (iEmptyAlgorithms = 0) AndAlso bAllAlgorithms
        End Get
    End Property
    Private Sub IntegrateFuzzy()
        Dim sFileName As String = "D:\Downloads\DYN_SEG_LBFuzzyGaussian_Cut.avi"
        Using oVideoReader As New Accord.Video.FFMPEG.VideoFileReader
            oVideoReader.Open(sFileName)

            Using oIntegratedMatrix As New Matrix(Of Byte)(SaveVideoHeight, SaveVideoWidth)
                oIntegratedMatrix.SetZero()

                Dim iTotalFrames As Integer = oVideoReader.FrameCount
                For i = 0 To iTotalFrames - 1
                    Using oBitmap As System.Drawing.Bitmap = oVideoReader.ReadVideoFrame(i)
                        Using oColourMatrix As Matrix(Of Byte) = Converter.BitmapToMatrix(oBitmap)
                            Using oMonoMatrix As New Matrix(Of Byte)(oColourMatrix.Size)
                                ' convert to single channel matrices
                                CvInvoke.CvtColor(oColourMatrix, oMonoMatrix, CvEnum.ColorConversion.Bgr2Gray)
                                CvInvoke.Threshold(oMonoMatrix, oMonoMatrix, 127, 255, CvEnum.ThresholdType.Binary)

                                oIntegratedMatrix.SetValue(255, oMonoMatrix)
                            End Using
                        End Using
                    End Using
                Next

                CvInvoke.MedianBlur(oIntegratedMatrix, oIntegratedMatrix, 3)

                Dim iCount As Integer = CvInvoke.CountNonZero(oIntegratedMatrix)
                Dim fFPR As Double = CDbl(iCount) / CDbl(SaveVideoHeight * SaveVideoWidth)
                MsgBox(fFPR.ToString("N3"))
            End Using
        End Using
    End Sub
    Private Sub CompressSegmented()
        ' compress to MJPEG
        Dim oFolderBrowserDialog As New Forms.FolderBrowserDialog
        oFolderBrowserDialog.Description = "Compress To MJPEG"
        oFolderBrowserDialog.ShowNewFolderButton = False
        oFolderBrowserDialog.RootFolder = Environment.SpecialFolder.Desktop
        If oFolderBrowserDialog.ShowDialog = Forms.DialogResult.OK Then
            Dim oDirectoryInfo As New IO.DirectoryInfo(oFolderBrowserDialog.SelectedPath)
            Dim oVideoFiles As List(Of IO.FileInfo) = oDirectoryInfo.EnumerateFiles("*.avi", IO.SearchOption.AllDirectories).ToList

            Dim iCurrent As Integer = 0
            For Each oVideoFile In oVideoFiles
                ' create compressed directory if not present
                Dim sNewDirectory As String = oVideoFile.DirectoryName + "\Compressed"
                If Not IO.Directory.Exists(sNewDirectory) Then
                    IO.Directory.CreateDirectory(sNewDirectory)
                End If

                Dim sNewFile As String = sNewDirectory + "\" + Left(oVideoFile.Name, Len(oVideoFile.Name) - Len(oVideoFile.Extension)) + "_Compressed" + oVideoFile.Extension
                Using oVideoReader As New Accord.Video.FFMPEG.VideoFileReader
                    oVideoReader.Open(oVideoFile.FullName)
                    Dim iFrameCount As Integer = oVideoReader.FrameCount

                    Using oVideoWriter As New VideoWriter(sNewFile, -1, oVideoReader.FrameRate.ToDouble, New System.Drawing.Size(oVideoReader.Width, oVideoReader.Height), False)
                        For i = 0 To iFrameCount - 1
                            Using oBitmap As System.Drawing.Bitmap = oVideoReader.ReadVideoFrame(i)
                                Using oGrayBitmap As System.Drawing.Bitmap = Converter.BitmapConvertGrayscale(oBitmap)
                                    Using oGrayMatrix As Matrix(Of Byte) = Converter.BitmapToMatrix(oBitmap)
                                        oVideoWriter.Write(oGrayMatrix.Mat)
                                    End Using
                                End Using
                            End Using
                        Next
                    End Using

                    iCurrent += 1
                    Dim TaskDelegate1 As Action = Sub()
                                                      TextTimer.Text = iCurrent.ToString + "/" + oVideoFiles.Count.ToString
                                                  End Sub

                    CommonFunctions.SafeInvoke(TaskDelegate1, UIDispatcher, True)
                End Using
            Next

            Dim TaskDelegate2 As Action = Sub()
                                              TextTimer.Text = String.Empty
                                          End Sub

            CommonFunctions.SafeInvoke(TaskDelegate2, UIDispatcher, True)
        End If
    End Sub
    Private Sub Compress_Handler(sender As Object, e As EventArgs) Handles ButtonCompress.Click
        Dim oAction As Action = Sub()
                                    CompressSegmented()
                                End Sub
        CommonFunctions.ClickCheck(sender, e, oAction)
    End Sub
    ' <local:HighlightButton x:Name="ButtonCompress" HBInnerToolTip="Compress Segmented Videos" HBReference="{Binding ElementName=ButtonBackground, Path=ActualHeight, Mode=Default, Converter={StaticResource MultiplierConverter}, ConverterParameter=2}"/>
    Sub Test()
        CommonIcons.Add("XAMLCompress", Converter.XamlToDrawingImage(My.Resources.XAMLCompress))
        ButtonCompress.HBSource = CommonFunctions.GetIcon("XAMLCompress")
    End Sub
End Class

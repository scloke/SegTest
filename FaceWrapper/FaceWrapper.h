// FaceWrapper.h
#pragma unmanaged
#include "facedetect-dll.h"
#pragma managed

#pragma once

using namespace System;

namespace FaceWrapper {

	public ref class FaceWrapper
	{
	private:
		~FaceWrapper();
		!FaceWrapper();

	public:
		FaceWrapper();

		/**
		* \brief Detects faces based on a supplied image, with optional landmark detection.
		* \param[in] result_buffer		Buffer memory for storing face detection results, !!its size must be 0x20000 Bytes!!.
		* \param[in] gray_image_data	OpenCV Mat ptr for image. Must be grayscale.
		* \param[in] width				OpenCV Mat col.
		* \param[in] height				OpenCV Mat row.
		* \param[in] step				OpenCV Mat step.
		* \param[in] scale				Scale factor for scan windows.
		* \param[in] min_object_width	Minimum possible face size. Faces smaller than that are ignored.
		* \param[in] max_object_width	Maximum possible face size. Faces larger than that are ignored. It is the largest posible when max_object_width=0.
		* \param[in] doLandmark			Landmark detection, either 0 or 1.
		*/
		static IntPtr FaceWrapper::FaceWrapper::FD_multiview_reinforce(IntPtr result_buffer, //buffer memory for storing face detection results, !!its size must be 0x20000 Bytes!!
			IntPtr gray_image_data, int width, int height, int step, //input image, it must be gray (single-channel) image!
			float scale, //scale factor for scan windows
			int min_neighbors, //how many neighbors each candidate rectangle should have to retain it
			int min_object_width, //Minimum possible face size. Faces smaller than that are ignored.
			int max_object_width, //Maximum possible face size. Faces larger than that are ignored. It is the largest posible when max_object_width=0.
			int doLandmark); // landmark detection

	};
}
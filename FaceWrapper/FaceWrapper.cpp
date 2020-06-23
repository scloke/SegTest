// This is the main DLL file.

#include "stdafx.h"
#include "FaceWrapper.h"

#pragma comment(lib,"libfacedetect-x64.lib")

FaceWrapper::FaceWrapper::~FaceWrapper()
{
	this->!FaceWrapper();
}

FaceWrapper::FaceWrapper::!FaceWrapper()
{
}

FaceWrapper::FaceWrapper::FaceWrapper()
{
}

IntPtr FaceWrapper::FaceWrapper::FD_multiview_reinforce(IntPtr result_buffer, IntPtr gray_image_data, int width, int height, int step, float scale, int min_neighbors, int min_object_width, int max_object_width, int doLandmark)
{
	System::Object^ obj = gcnew System::Object();
	System::Threading::Monitor::Enter(obj);
	try
	{
		unsigned char* pbybuffer = reinterpret_cast<unsigned char*>(result_buffer.ToPointer());
		unsigned char* pbyimage = reinterpret_cast<unsigned char*>(gray_image_data.ToPointer());

		int* presult = facedetect_multiview_reinforce(pbybuffer, pbyimage, width, height, step, scale, min_neighbors, min_object_width, max_object_width, doLandmark);
		
		return IntPtr(presult);
	}
	finally
	{
		System::Threading::Monitor::Exit(obj);
	}

	delete obj;
}

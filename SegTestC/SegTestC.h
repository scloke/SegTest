// SegTestC.h - Contains declarations of SegTest C++ functions
#pragma once

#ifdef SEGTEST_EXPORTS
#define SEGTEST_API __declspec(dllexport)
#else
#define SEGTEST_API __declspec(dllimport)
#endif

#pragma warning (disable : 26451)

#include <chrono>
#include <unordered_map>
#include "../bgslibrary/algorithms/algorithms.h"

extern "C" SEGTEST_API void initSegmenter(int task, int segtype);

extern "C" SEGTEST_API void exitSegmenter(int task);

extern "C" SEGTEST_API bool validbgs(int task);

extern "C" SEGTEST_API bool segment(int task, BYTE* matstruct_in, BYTE* buffer_in, BYTE* matstruct_out, BYTE* buffer_out, int* duration);

#ifndef SEGTEST_H
#define SEGTEST_H

// utility functions
class utility
{
public:
	// BUFFER ACCESS
	// gets mat from matstruct and buffer
	static cv::Mat getMat(BYTE* matstruct, BYTE* buffer)
	{
		std::tuple<int, int, int, int, int, int> returnStruct = getMatStruct(matstruct);
		int width = std::get<0>(returnStruct);
		int height = std::get<1>(returnStruct);
		int type = std::get<4>(returnStruct);
		int step = std::get<5>(returnStruct);

		cv::Mat mat(height, width, type, buffer, step);

		return mat;
	}

	static std::tuple<int, int, int, int, int, int> getMatStruct(BYTE* matstruct)
	{
		int width = getIntFromBuffer(matstruct, 0);
		int height = getIntFromBuffer(matstruct, 4);
		int channels = getIntFromBuffer(matstruct, 8);
		int length = getIntFromBuffer(matstruct, 12);
		int basetype = getIntFromBuffer(matstruct, 16);

		int type = basetype + ((channels - 1) * 8);
		int step = length / height;

		return std::make_tuple(width, height, channels, length, type, step);
	}

	// gets int from buffer location
	static int getIntFromBuffer(BYTE* buffer, int offset)
	{
		int returnInt = *(int*)(buffer + offset);
		return returnInt;
	}

	// VECTOR FUNCTIONS
	// checks if a value is present in the vector
	template<typename T> static bool findVal(const std::vector<T>& vec, T value)
	{
		return std::find(vec.begin(), vec.end(), value) != vec.end();
	}

private:
};

class segmenter
{
public:
	segmenter(int segtype)
	{
		algorithmsName = BGS_Factory::Instance()->GetRegisteredAlgorithmsName();
		algorithmName = segtypeToString(segtype);
		if (utility::findVal<std::string>(algorithmsName, algorithmName)) {
			bgs = BGS_Factory::Instance()->Create(algorithmName);
			bgs->setShowOutput(false);
		}
	}
	~segmenter()
	{
		bgs.~shared_ptr();
	}

	// checks if bgs successfully initialised
	bool validbgs()
	{
		return bgs != nullptr;
	}

	bool segment(BYTE* matstruct_in, BYTE* buffer_in, BYTE* matstruct_out, BYTE* buffer_out, int* duration)
	{
		if (bgs != nullptr) {
			cv::Mat inMat = utility::getMat(matstruct_in, buffer_in);
			cv::Mat outMat = utility::getMat(matstruct_out, buffer_out);

			cv::Mat mask;
			cv::Mat backgroundModel;

			auto tProcessingStart = std::chrono::high_resolution_clock::now();
			bgs->process(inMat, mask, backgroundModel);
			auto tProcessingEnd = std::chrono::high_resolution_clock::now();
			*duration = (int)std::chrono::duration_cast<std::chrono::microseconds>(tProcessingEnd - tProcessingStart).count();

			if (mask.channels() == 3) {
				mask.copyTo(outMat);
			}
			else {
				cv::cvtColor(mask, outMat, CV_GRAY2BGR);
			}
			mask.release();
			backgroundModel.release();

			return true;
		}
		else {
			return false;
		}
	}

private:
	std::shared_ptr<bgslibrary::algorithms::IBGS> bgs;
	std::vector<std::string> algorithmsName;
	std::string algorithmName;

	// converts segtype to string
	std::string segtypeToString(int segtype)
	{
		std::string returnString = "";

		switch (segtype) {
		case 1:
			returnString = "FrameDifference";
			break;
		case 2:
			returnString = "StaticFrameDifference";
			break;
		case 3:
			returnString = "WeightedMovingMean";
			break;
		case 4:
			returnString = "WeightedMovingVariance";
			break;
		case 5:
			returnString = "MixtureOfGaussianV1";
			break;
		case 6:
			returnString = "MixtureOfGaussianV2";
			break;
		case 7:
			returnString = "AdaptiveBackgroundLearning";
			break;
		case 8:
			returnString = "AdaptiveSelectiveBackgroundLearning";
			break;
		case 9:
			returnString = "KNN";
			break;
		case 10:
			returnString = "GMG";
			break;
		case 11:
			returnString = "DPAdaptiveMedian";
			break;
		case 12:
			returnString = "DPGrimsonGMM";
			break;
		case 13:
			returnString = "DPZivkovicAGMM";
			break;
		case 14:
			returnString = "DPMean";
			break;
		case 15:
			returnString = "DPWrenGA";
			break;
		case 16:
			returnString = "DPPratiMediod";
			break;
		case 17:
			returnString = "DPEigenbackground";
			break;
		case 18:
			returnString = "DPTexture";
			break;
		case 19:
			returnString = "T2FGMM_UM";
			break;
		case 20:
			returnString = "T2FGMM_UV";
			break;
		case 21:
			returnString = "T2FMRF_UM";
			break;
		case 22:
			returnString = "T2FMRF_UV";
			break;
		case 23:
			returnString = "FuzzySugenoIntegral";
			break;
		case 24:
			returnString = "FuzzyChoquetIntegral";
			break;
		case 25:
			returnString = "LBSimpleGaussian";
			break;
		case 26:
			returnString = "LBFuzzyGaussian";
			break;
		case 27:
			returnString = "LBMixtureOfGaussians";
			break;
		case 28:
			returnString = "LBAdaptiveSOM";
			break;
		case 29:
			returnString = "LBFuzzyAdaptiveSOM";
			break;
		case 30:
			returnString = "LBP_MRF";
			break;
		case 31:
			returnString = "MultiLayer";
			break;
		case 32:
			returnString = "PixelBasedAdaptiveSegmenter";
			break;
		case 33:
			returnString = "VuMeter";
			break;
		case 34:
			returnString = "KDE";
			break;
		case 35:
			returnString = "IndependentMultimodal";
			break;
		case 36:
			returnString = "MultiCue";
			break;
		case 37:
			returnString = "SigmaDelta";
			break;
		case 38:
			returnString = "SuBSENSE";
			break;
		case 39:
			returnString = "LOBSTER";
			break;
		case 40:
			returnString = "PAWCS";
			break;
		case 41:
			returnString = "TwoPoints";
			break;
		case 42:
			returnString = "ViBe";
			break;
		case 43:
			returnString = "CodeBook";
			break;
		default:
			break;
		}

		return returnString;
	}
};



#endif

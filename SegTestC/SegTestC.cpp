// SegTestC.cpp : Defines the exported functions for the DLL.
#include "pch.h" // use pch.h in Visual Studio 2019

#include "SegTestC.h"

static std::unordered_map<int, cv::Ptr<segmenter>> seg;


void initSegmenter(int task, int segtype)
{
	if (seg.count(task) == 0) {
		seg.emplace(task, new segmenter(segtype));
	}
}

void exitSegmenter(int task)
{
	if (seg.count(task) == 1) {
		seg.erase(task);
	}
}

bool validbgs(int task)
{
	return seg[task]->validbgs();
}

bool segment(int task, BYTE* matstruct_in, BYTE* buffer_in, BYTE* matstruct_out, BYTE* buffer_out, int* duration)
{
	return seg[task]->segment(matstruct_in, buffer_in, matstruct_out, buffer_out, duration);
}

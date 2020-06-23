#pragma once

#include "IBGS.h"

#include "opencv2/core/version.hpp"
#if CV_MAJOR_VERSION >= 2 && CV_MAJOR_VERSION <= 3

#include "lb/BGModelFuzzySom.h"

namespace bgslibrary
{
  namespace algorithms
  {
    class LBFuzzyAdaptiveSOM : public IBGS
    {
    private:
      lb::BGModel* m_pBGModel;
      int sensitivity;
      int trainingSensitivity;
      int learningRate;
      int trainingLearningRate;
      int trainingSteps;

    public:
      LBFuzzyAdaptiveSOM();
      ~LBFuzzyAdaptiveSOM();

      void process(const cv::Mat &img_input, cv::Mat &img_output, cv::Mat &img_bgmodel);

    private:
      void save_config(cv::FileStorage &fs);
      void load_config(cv::FileStorage &fs);
    };

    bgs_register(LBFuzzyAdaptiveSOM);
  }
}

#endif

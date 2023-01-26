/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "scoring.h"

using namespace ldt;

template <bool hasWeight, bool isBinary> AUC<hasWeight, isBinary>::AUC(){};

template <bool hasWeight, bool isBinary> AUC<hasWeight, isBinary>::AUC(Ti n){};

template <bool hasWeight, bool isBinary>
void AUC<hasWeight, isBinary>::Calculate(Matrix<Tv> &y, Matrix<Tv> &scores,
                                         Matrix<Tv> *weights,
                                         Matrix<Tv> *multi_class_weights_) {
  Ti n = y.length();

  Tv sum_weights_;
  if constexpr (hasWeight) {
    sum_weights_ = weights->Sum();
  } else if constexpr (true) {
    sum_weights_ = (Tv)n;
  }

  if constexpr (isBinary) {
    if (scores.ColsCount != 2)
      throw std::logic_error("Invalid number of columns in score (expected 2)");
    auto sorted_idx = std::vector<Ti>();
    SortIndexes(&scores.Data[n], n,
                sorted_idx); // second column in scores is for positive

    Tv cur_pos = 0; // temp sum of positive label
    Tv cur_neg = 0; // temp sum of negative label
    Tv sum_pos = 0; // total sum of positive label
    Tv accum = 0;   // accumulate of AUC

    Tv threshold = scores.Data[sorted_idx[0]];
    for (Ti i = 0; i < n; i++) {
      const Tv cur_label = y.Data[sorted_idx[i]];
      const Tv cur_score = scores.Data[sorted_idx[i]];
      if (cur_score != threshold) { // new threshold
        threshold = cur_score;
        accum += cur_neg * (cur_pos * 0.5 + sum_pos); // accumulate
        sum_pos += cur_pos;
        cur_neg = cur_pos = 0; // reset
      }
      Tv cur_weight = 1;
      if constexpr (hasWeight) {
        cur_weight = weights->Data[sorted_idx[i]];
      }
      cur_neg += (cur_label <= 0) * cur_weight;
      cur_pos += (cur_label > 0) * cur_weight;
    }
    accum += cur_neg * (cur_pos * 0.5 + sum_pos);
    sum_pos += cur_pos;
    Result = 1;
    if (sum_pos > 0 && sum_pos != sum_weights_) {
      Result = accum / (sum_pos * (sum_weights_ - sum_pos));
    }
    // Result = std::max(Result, 1 - Result); ????!!!!
  } else {
    // this part is rather incomplete
    // I just copied and fixed the errors
    // for multi-class-weights see:
    // https://github.com/microsoft/LightGBM/blob/83627ff0d6baf49e3b18991f47b12d9fa5724cbc/src/io/config.cpp#L157

    Tv kEpsilon = 1e-15;
    Ti num_class_ = scores.ColsCount;

    // sort the data indices by true class
    auto sorted_data_idx_ = std::vector<Ti>();
    SortIndexes(y.Data, n, sorted_data_idx_);

    // get size of each class and total weight of data in each class
    auto class_sizes_ = std::vector<Ti>(num_class_, 0);
    auto class_data_weights_ = std::vector<Tv>(num_class_, 0);
    for (Ti i = 0; i < n; i++) {
      Ti curr_label = static_cast<Ti>(y.Data[i]);
      class_sizes_[curr_label]++;
      if constexpr (hasWeight) {
        class_data_weights_[curr_label] += weights->Data[i];
      }
    }

    auto S = std::vector<std::vector<Tv>>(num_class_,
                                          std::vector<Tv>(num_class_, 0));
    Ti i_start = 0;
    for (Ti i = 0; i < num_class_; i++) {
      Ti j_start = i_start + class_sizes_[i];
      for (Ti j = i + 1; j < num_class_; ++j) {
        auto curr_v = std::vector<Tv>(num_class_, 0);
        if (multi_class_weights_->Data) {
          for (Ti k = 0; k < num_class_; ++k) {
            curr_v.at(k) = multi_class_weights_->Get(i, k) -
                           multi_class_weights_->Get(j, k);
          }
        }

        Tv t1 = curr_v[i] - curr_v[j];
        // extract the data indices belonging to class i or j
        std::vector<Ti> class_i_j_indices;
        class_i_j_indices.assign(sorted_data_idx_.begin() + i_start,
                                 sorted_data_idx_.begin() + i_start +
                                     class_sizes_[i]);
        class_i_j_indices.insert(
            class_i_j_indices.end(), sorted_data_idx_.begin() + j_start,
            sorted_data_idx_.begin() + j_start + class_sizes_[j]);
        // sort according to distance from separating hyperplane
        std::vector<std::pair<Ti, Tv>> dist;
        for (Ti k = 0; static_cast<size_t>(k) < class_i_j_indices.size(); k++) {
          Ti a = class_i_j_indices[k];
          Tv v_a = 0;
          for (Ti m = 0; m < num_class_; m++) {
            v_a += curr_v[m] * scores.Data[n * m + a]; //???? use get
          }
          dist.push_back(std::pair<Ti, Tv>(a, t1 * v_a));
        }
        std::sort(dist.begin(), dist.end(),
                  [&](std::pair<Ti, Tv> a, std::pair<Ti, Tv> b) {
                    // if scores are equal, put j class first
                    if (std::fabs(a.second - b.second) < kEpsilon) {
                      return y.Data[a.first] > y.Data[b.first];
                    } else if (a.second < b.second) {
                      return true;
                    } else {
                      return false;
                    }
                  });
        // calculate AUC
        Tv num_j = 0;
        Tv last_j_dist = 0;
        Tv num_current_j = 0;

        for (size_t k = 0; k < dist.size(); ++k) {
          Ti a = dist[k].first;
          Tv curr_dist = dist[k].second;
          Tv curr_weight = 1;
          if constexpr (hasWeight) {
            curr_weight = weights->Data[a];
          }
          if (y.Data[a] == i) {
            if (std::fabs(curr_dist - last_j_dist) < kEpsilon) {
              S[i][j] +=
                  curr_weight *
                  (num_j - 0.5 * num_current_j); // members of class j with same
                                                 // distance as a contribute 0.5
            } else {
              S[i][j] += curr_weight * num_j;
            }
          } else {
            num_j += curr_weight;
            if (std::fabs(curr_dist - last_j_dist) < kEpsilon) {
              num_current_j += curr_weight;
            } else {
              last_j_dist = dist[k].second;
              num_current_j = curr_weight;
            }
          }
        }
        j_start += class_sizes_[j];
      }
      i_start += class_sizes_[i];
    }
    Result = 0;
    for (Ti i = 0; i < num_class_; i++) {
      for (Ti j = i + 1; j < num_class_; ++j) {
        if constexpr (hasWeight) {
          Result += (S[i][j] / class_data_weights_[i]) / class_data_weights_[j];
        } else if constexpr (true) {
          Result += (S[i][j] / class_sizes_[i]) / class_sizes_[j];
        }
      }
    }
    Result = (2.0 * Result / num_class_) / (num_class_ - 1);
  }
}

template class ldt::AUC<true, true>;
template class ldt::AUC<true, false>;
template class ldt::AUC<false, true>;
template class ldt::AUC<false, false>;

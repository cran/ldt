/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "array.h"

using namespace ldt;

// #pragma region Index Range

IndexRange::IndexRange(Ti start, Ti end) {
  if (start > end || start < 0 || end < 0) {
    StartIndex = 1;
    EndIndex = 0;
  } else {
    StartIndex = start;
    EndIndex = end;
  }
}

bool IndexRange::IsNotValid() const { return StartIndex > EndIndex; }

Ti IndexRange::Count() const { return EndIndex - StartIndex + 1; }

// #pragma endregion

template <typename Tw>
IndexRange Array<Tw>::GetRange(const Tw *data, const Ti &length) {

  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    Ti start, end;
    for (start = 0; start < length; start++)
      if (std::isnan(data[start]) == false)
        break;

    for (end = length; end > 0; end--)
      if (std::isnan(data[end - 1]) == false) {
        end--;
        break;
      }

    return IndexRange(start, end);

  } else if constexpr (true) {
    throw std::logic_error(
        "invalid operation.Type has no NA defined."); // there is no NAN
  }
}

template <typename Tw>
IndexRange Array<Tw>::GetRange(const Tw *data, const Ti &length,
                               bool &hasMissing) {

  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {
    Ti start, end;
    hasMissing = false;
    for (start = 0; start < length; start++)
      if (std::isnan(data[start]) == false)
        break;

    for (end = length; end > 0; end--)
      if (std::isnan(data[end - 1]) == false) {
        end--;
        break;
      }

    if (start > end)
      return IndexRange(start, end);

    for (Ti i = start; i <= end; i++)
      if (std::isnan(data[i])) {
        hasMissing = true;
        break;
      }

    return IndexRange(start, end);

  } else if constexpr (true) {
    throw std::logic_error(
        "invalid operation.Type has no NA defined."); // there is no NAN
  }
}

template <typename Tw>
IndexRange Array<Tw>::Interpolate(Tw *data, const Ti &length, Ti &count) {

  if constexpr (std::numeric_limits<Tw>::has_quiet_NaN) {

    bool hasMissing = false;
    auto range = Array<Tw>::GetRange(data, length, hasMissing);
    count = 0;
    if (hasMissing) {
      bool inMissing = false;
      Tw first = NAN, last = NAN;
      Ti length = 1;

      for (Ti i = range.StartIndex; i <= range.EndIndex; i++) {
        auto isNaN = std::isnan(data[i]);
        if (isNaN)
          length++;
        if (isNaN == false && inMissing) {
          last = data[i];
          // calculate and set
          Tw step = (last - first) / length;
          for (int p = 1; p < length; p++) {
            data[i - p] = data[i] - p * step;
            count++;
          }
          length = 1;
          inMissing = false;
        }
        if (isNaN && inMissing == false) {
          first = data[i - 1];
          inMissing = true;
        }
      }
    }
    return range;
  } else if constexpr (true)
    throw std::logic_error("invalid operation"); // there is no NAN
}

template <typename Tw>
void Array<Tw>::PartitionEqual(const std::vector<Tw> &data,
                               std::vector<std::vector<Tw>> &result, Ti size,
                               bool fromEnd) {
  result.clear();
  if (fromEnd) {
    for (Ti i = (Ti)data.size(); i >= 0; i -= size) {
      int start = std::max(i - size, 0);
      result.insert(result.begin(),
                    std::vector<Tw>(data.begin() + start, data.begin() + i));
    }
  } else {
    for (Ti i = 0; i < (Ti)data.size(); i += size) {
      int end = std::min(i + size, static_cast<int>(data.size()));
      result.emplace_back(data.begin() + i, data.begin() + end);
    }
  }
}

template class ldt::Array<Tv>;

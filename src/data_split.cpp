/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "data_split.h"

using namespace ldt;

// #pragma region Discrete

DataSplitDiscrete::DataSplitDiscrete(Ti rows, Ti cols, Ti numChoices) {

  mNumChoices = numChoices;
  Rows = std::vector<std::vector<Ti> *>(numChoices);
  Counts = std::vector<Ti>(numChoices);
  CountsSortedIndexes = std::vector<Ti>(numChoices);

  StorageSize = rows * cols;
  WorkSizeI = rows;
}

DataSplitDiscrete::~DataSplitDiscrete() {
  for (auto a : Rows)
    delete a;
}

void DataSplitDiscrete::Calculate(const Matrix<Tv> &data, Tv *storage,
                                  Tv trainRatio, Ti trainFixSize) {

  auto rows = data.RowsCount;
  auto cols = data.ColsCount;
  mTrainRatio = trainRatio;
  mTrainFixSize = trainFixSize;

  // allocate size
  Ti N0 = mTrainFixSize > 0 ? mTrainFixSize
                            : static_cast<Ti>(std::round(rows * trainRatio));
  Ti N1 = rows - N0;
  Sample0.SetData(storage, N0, cols);
  Sample1.SetData(&storage[N0 * cols], N1, cols);

  // find indexes and prepare for Shuffle
  Ti i, j;
  Y.SetData(data.Data, rows, 1);

  for (i = 0; i < mNumChoices; i++)
    Counts.at(i) = 0;

  // count and check variance of y
  for (i = 0; i < rows; i++) {
    j = static_cast<Ti>(Y.Data[i]);
    Counts.at(j)++;
  }
  for (i = 0; i < mNumChoices; i++)
    if (Counts.at(i) == 0)
      throw LdtException(
          ErrorType::kLogic, "datasplit",
          "at least one group is empty (in discrete choice sampling)");

  for (i = 0; i < mNumChoices; i++) {
    j = Counts.at(i);
    if (Rows.at(i))
      delete Rows.at(i);
    Rows.at(i) = new std::vector<Ti>();
  }

  for (i = 0; i < rows; i++) {
    j = static_cast<Ti>(Y.Data[i]);
    Rows.at(j)->push_back(i); // signal that i-th row is of type j
  }

  SortIndexes<Ti>(Counts, CountsSortedIndexes);
}

void DataSplitDiscrete::Shuffle(const Matrix<Tv> &data, Ti *workI,
                                std::mt19937 &eng) {

  Ti N0 = Sample0.RowsCount;

  std::vector<Ti> *Rowi = nullptr;
  Ti Mi, Mi0, i, v;
  Ti sumM = 0, b = 0,
     ii = 0; // index we set rows of x0 and y0 in different groups
  Ti jj = 0; // index we set rows of x1 and y1

  for (auto a : CountsSortedIndexes) {
    // Tv aa = static_cast<Tv>(a);
    Mi = Counts.at(a);
    Rowi = Rows.at(a);
    if (b == mNumChoices - 1) {
      Mi0 = N0 - sumM; // fill it
      if (Mi0 <= 0)
        throw LdtException(
            ErrorType::kLogic, "datasplit",
            "invalid group length. All contain just 1 obs. Mi0=" +
                std::to_string(Mi0));
      if (Mi0 > Mi)
        throw LdtException(
            ErrorType::kLogic, "datasplit",
            "invalid training percentage"); // percentage is too high
                                            // (or maybe too low?!)
    } else {
      Mi0 = static_cast<Ti>(std::round(mTrainRatio * Mi));
      if (Mi0 >= Mi)
        Mi0 = Mi - 1;
      if (Mi0 == 0) // don't use 'else', because it might have just 1 obs. Of
                    // course, there is no change to test this group
        Mi0 = 1;
      sumM += Mi0;
    }
    b++;

    for (i = 0; i < Mi; i++)
      workI[i] = i;

    std::shuffle(&workI[0], &workI[Mi], eng);
    for (i = 0; i < Mi0; i++) {
      v = Rowi->at(workI[i]);
      Sample0.SetRowFromRow0(ii, data, v);
      ii++;
    }

    for (i = Mi0; i < Mi; i++) {
      v = Rowi->at(workI[i]);
      Sample1.SetRowFromRow0(jj, data, v);
      jj++;
    }
  }
}

// #pragma endregion

// #pragma region Split

DataSplit::DataSplit(Ti rows, Ti cols) {
  StorageSize = rows * cols;
  WorkSizeI = rows;
}

void DataSplit::Calculate(const Matrix<Tv> &data, Tv *storage, Tv trainRatio,
                          Ti trainFixSize) {

  auto rows = data.RowsCount;
  auto cols = data.ColsCount;
  mTrainRatio = trainRatio;
  mTrainFixSize = trainFixSize;

  // allocate size
  Ti N0 = mTrainFixSize > 0 ? mTrainFixSize
                            : static_cast<Ti>(std::round(rows * trainRatio));
  Ti N1 = rows - N0;

  if (N0 <= 0 || N0 >= rows)
    throw LdtException(ErrorType::kLogic, "datasplit",
                       "training sample size is too low/high with respect to "
                       "the available observations");

  Sample0.SetData(storage, N0, cols);
  Sample1.SetData(&storage[N0 * cols], N1, cols);
}

void DataSplit::Shuffle(const Matrix<Tv> &data, Ti *workI, std::mt19937 &eng) {

  Ti N0 = Sample0.RowsCount;
  Ti N1 = Sample1.RowsCount;
  Ti i;
  for (i = 0; i < data.RowsCount; i++)
    workI[i] = i;

  std::shuffle(&workI[0], &workI[data.RowsCount], eng);
  for (i = 0; i < N0; i++)
    Sample0.SetRowFromRow0(i, data, workI[i]);

  for (i = 0; i < N1; i++)
    Sample1.SetRowFromRow0(i, data, workI[N0 + i]);
}

// #pragma endregion

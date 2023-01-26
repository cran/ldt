/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "varma.h"

using namespace ldt;

VarmaRestriction::VarmaRestriction(const VarmaSizes &sizes,
                                   VarmaRestrictionType type,
                                   Ti generalRescrictionCount) {
  IsRestricted = false;
  pSizes = &sizes;
  mType = type;
  mGeneralRestrictionCount = generalRescrictionCount;

  Ti q = 0;
  if (type == VarmaRestrictionType::kNone) {
    return;
  } else if (type == VarmaRestrictionType::kGeneral) {
    q = sizes.NumParams - mGeneralRestrictionCount;
    if (mGeneralRestrictionCount <= 0)
      throw std::logic_error("invalid number of restrictions");
  } else if (type == VarmaRestrictionType::kMaFinal) {
    if (sizes.EqsCount == 1 || sizes.HasMa == false)
      return;
    q = sizes.EqsCount * sizes.NumParamsEq -
        sizes.MaLength * (sizes.EqsCount * sizes.EqsCount - 1);
  } else
    throw std::logic_error("not implemented");

  IsRestricted = true;
  R = Matrix<Tv>(sizes.NumParams, q);
  StorageSize = sizes.NumParams * q;
}

void VarmaRestriction::Calculate(Tv *storage,
                                 std::vector<Ti> *generalRestrictedIndexes) {
  if (IsRestricted == false)
    return; // it is not a restricted model
  auto sizes = *pSizes;
  Ti params = R.RowsCount;

  if (mType == VarmaRestrictionType::kGeneral) {
    if (!generalRestrictedIndexes)
      throw std::logic_error("list of restriction indexes is missing");
    Ti r = (Ti)generalRestrictedIndexes->size();
    Ti q = params - r;
    R.Restructure0(params, q);
    R.SetData(0, storage);
    if (mGeneralRestrictionCount >
        r) // we can set larger restrictions, but not fewer
      throw std::logic_error("inconsistent number of restrictions");

    Ti j = -1;
    for (Ti i = 0; i < params; i++) {
      j++;
      bool isRestricted = std::find(generalRestrictedIndexes->begin(),
                                    generalRestrictedIndexes->end(),
                                    i) != generalRestrictedIndexes->end();
      if (isRestricted)
        j--;
      else
        R.Set(i, j, 1);
      // leave an empty row if it is a restricted parameter
    }
  } else if (mType == VarmaRestrictionType::kMaFinal) {
    Ti q = R.ColsCount;
    R.SetData(0, storage);
    Ti row = 0;
    for (; row < sizes.MaStart * sizes.EqsCount; row++)
      R.Set0(row, row, 1);
    Ti col = row;
    while (true) {
      for (Ti j = 0; j < sizes.EqsCount; j++)
        R.Set0(row + j * (sizes.EqsCount + 1), col, 1);
      row += sizes.EqsCount * sizes.EqsCount;
      col++;
      if (col == q)
        break;
    }
  } else
    throw std::logic_error("not implemented");
}

Ti VarmaRestriction::GetNumRestrictionInEq(Matrix<Tv> &R, Ti eqIndex,
                                           Ti eqCount) {
  // just a guess. check it
  throw std::logic_error("Not yet tested and unreliable");

  // empty rows indicate a restricted parameter
  auto m = (Ti)R.RowsCount / eqCount;

  auto sumr = std::unique_ptr<Tv[]>(new Tv[m]);
  auto sumrm = Matrix<Tv>(sumr.get(), m, 1);
  auto rowinds = std::vector<Ti>(m);
  std::iota(rowinds.begin(), rowinds.end(), m * eqIndex);
  R.RowsSum(sumrm, rowinds);

  Ti c = 0;
  for (Ti i = 0; i < m; i++)
    if (sumrm.Data[i] == 0)
      c++;

  return c;
}

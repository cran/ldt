/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "functionx.h"

using namespace ldt;

template <bool vert>
AucPoints<vert>::AucPoints(const std::vector<std::tuple<Tv, Tv>> &points,
                           Tv baseLine) {
  Result = 0;
  if (points.size() <= 1)
    return;

  Tv x0 = std::get<0>(points.at(0)), y0 = std::get<1>(points.at(0)), x = 0,
     y = 0, dx = 0, dy = 0;
  for (auto const &p : points) {
    x = std::get<0>(p);
    dx = x - x0;
    y = std::get<1>(p);
    dy = y - y0;

    if constexpr (vert == true) {
      Result += dy * (x0 - baseLine + dx / 2); // a rectangle and a triangle
    } else {
      Result += dx * (y0 - baseLine + dy / 2);
    }

    x0 = x;
    y0 = y;
  }
}

template class ldt::AucPoints<true>;
template class ldt::AucPoints<false>;

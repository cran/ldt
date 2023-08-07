/*
 Copyright (C) 2022-2023 Ramin Mojab
 Licensed under the GPL 3.0.
 See accompanying file LICENSE
*/

#include "clustering.h"
#include <stack>

using namespace ldt;

template <HClusterLinkage method> HCluster<method>::HCluster(Ti n) {
  this->n = n;
  this->Nodes = std::vector<HClusterNode *>();
  for (Ti i = 0; i < n; i++) {
    auto cn = new HClusterNode();
    cn->id = i;
    cn->distanceIndex = i;
    this->Nodes.push_back(cn);
  }
  // there will be n-1 more merged Nodes
  // up to n, the distance indexes are the 'ids'
  //    after that, we override the Distances
}
template <HClusterLinkage method> HCluster<method>::~HCluster() {
  for (auto a : this->Nodes)
    delete a;
}

template <HClusterLinkage method>
HClusterNode *HCluster<method>::Merge2(Ti &n_i, HClusterNode &leftNode,
                                       HClusterNode &rightNode,
                                       Tv leftDistanceRight) {

  auto cn = new HClusterNode();
  cn->id = n_i;
  cn->nodesWithin = leftNode.nodesWithin +
                    rightNode.nodesWithin; // update number of Nodes within
  cn->idLeft = leftNode.id;
  cn->idRight = rightNode.id;
  cn->leftDistanceRight = leftDistanceRight;
  cn->distanceIndex = std::min(leftNode.distanceIndex, rightNode.distanceIndex);

  leftNode.isMerged = true;
  rightNode.isMerged = true;

  // update the distance Matrix<Tv>
  Tv left_distance = 0;
  Tv right_distance = 0;
  Tv calculated_distance = 0;
  for (auto &a : this->Nodes) {
    if (a->isMerged)
      continue;

    left_distance = Distances->Get0(leftNode.distanceIndex, a->distanceIndex);
    right_distance = Distances->Get0(rightNode.distanceIndex, a->distanceIndex);
    calculated_distance = CalculateDistance(
        leftNode.nodesWithin, rightNode.nodesWithin, a->nodesWithin,
        left_distance, right_distance, leftDistanceRight);

    Distances->Set0(a->distanceIndex, cn->distanceIndex, calculated_distance);
  }

  // add to cluster array after updating
  n_i++;
  this->Nodes.push_back(cn);

  return cn;
}

template <HClusterLinkage method>
HClusterNode *HCluster<method>::GetNearestNeighbor(const HClusterNode &node,
                                                   Tv &distance) {
  Tv d;
  distance = INFINITY;
  HClusterNode *neighbor = nullptr;
  for (auto &a : this->Nodes) {
    if (a == &node || a->isMerged)
      continue;

    d = Distances->Get0(node.distanceIndex, a->distanceIndex);

    if (d < distance) {
      distance = d;
      neighbor = a;
    }
  }
  return neighbor;
}

template <HClusterLinkage method>
void HCluster<method>::Calculate(MatrixSym<false> &distances) {

  if (distances.Any(NAN))
    throw LdtException(ErrorType::kLogic, "hcluster",
                       "NaN (invalid) distance is found");

  this->Distances = &distances;
  if (distances.length_array() != n * (n - 1) / 2)
    throw LdtException(ErrorType::kLogic, "hcluster",
                       "invalid length"); // invalid length

  Tv nearest_1_distance = 0, nearest_2_distance = 0;
  Ti n_i = n, s; // 'n_i' is used to set the merged id
  HClusterNode *top_node, *nearest_1, *nearest_2, *agg_node;
  std::stack<Ti> neighbors_stack;

  // initialize
  top_node = this->Nodes.at(0); // arbitrarily chosen (a one-point clusters)
  neighbors_stack.push(0);
  nearest_1 = GetNearestNeighbor(*top_node, nearest_1_distance);

  while (true) { // update 'top_node', 'nearest_1', and 'nearest_1_distance' in
                 // each iteration
    if (n_i == 2 * n - 1) // max is n-1 merges (1/2+1/4+1/9+...=1)
      break;

    nearest_2 = GetNearestNeighbor(*nearest_1,
                                   nearest_2_distance); // find second nearest

    if (nearest_2->id == top_node->id) { // top and nearest must be merged
      neighbors_stack.pop();             // remove top node
      agg_node =
          Merge2(n_i, *top_node, *nearest_1, nearest_1_distance); // merge them

      s = (Ti)neighbors_stack.size();
      if (s == 0) {          // we need a new random node
        top_node = agg_node; // use the merged node
        neighbors_stack.push(top_node->id);
        nearest_1 = GetNearestNeighbor(*top_node, nearest_1_distance);
      } else if (s == 1) { // find and add nearest
        top_node = this->Nodes.at(neighbors_stack.top());
        nearest_1 = GetNearestNeighbor(*top_node, nearest_1_distance);
      } else { // go one level up
        nearest_1 = this->Nodes.at(neighbors_stack.top());
        neighbors_stack.pop();                            // remove new nearest
        top_node = this->Nodes.at(neighbors_stack.top()); // set the top
        nearest_1_distance =
            Distances->Get0(top_node->distanceIndex, nearest_1->distanceIndex);
      }
    } else { // push nearest into the stack and go one level down
      neighbors_stack.push(nearest_1->id);
      top_node = nearest_1;
      nearest_1 = nearest_2;
      nearest_1_distance = nearest_2_distance;
    }
  }
}

static void set_group_var(const std::vector<HClusterNode *> &Nodes,
                          HClusterNode &node, Matrix<Ti> &group_i, Ti last) {

  if (node.nodesWithin == 1) { // a single node
    group_i.Set0(node.id, 0, last);
  } else {
    set_group_var(Nodes, *Nodes.at(node.idLeft), group_i, last);
    set_group_var(Nodes, *Nodes.at(node.idRight), group_i, last);
  }
}

template <HClusterLinkage method>
void HCluster<method>::Group(std::vector<std::vector<Ti> *> &map) const {
  Ti k = (Ti)map.size(), i, j, m0, m1;
  if (k == n) { // n variables, n groups
    for (i = 0; i < n; i++)
      map.at(i)->push_back(i);
  } else if (k == 1) { // 1 group
    for (i = 0; i < n; i++)
      map.at(0)->push_back(i);
  } else {
    std::set<Ti> gr;
    auto group_i_data = std::unique_ptr<Ti[]>(new Ti[n]);
    Matrix<Ti> group_i =
        Matrix<Ti>(group_i_data.get(), n); // current group of variables
    group_i.SetValue(0);                   // first, all are one group
    Ti last = 0;
    HClusterNode *node, *node_left, *node_right;

    for (i = 2 * n - 2; i >= n; i--) { // i-th merge
      node = this->Nodes.at(i);

      node_left = this->Nodes.at(node->idLeft);
      node_right = this->Nodes.at(node->idRight);

      // an split occurred at this point
      // set the index of just one of the branches to a new index
      last++;
      set_group_var(this->Nodes,
                    node_left->nodesWithin < node_right->nodesWithin
                        ? *node_left
                        : *node_right,
                    group_i, last);

      // does this merge give us k groups?
      gr.clear();
      for (j = 0; j < n; j++) {
        m0 = group_i.Data[j];
        gr.insert(m0);
      }
      if ((Ti)gr.size() == k)
        break;
    }

    // group based on indexes
    m1 = 0;
    for (auto g : gr) {
      for (i = 0; i < n; i++)
        if (group_i.Data[i] == g)
          map.at(m1)->push_back(i);
      m1++;
    }
  }
}

template <HClusterLinkage method>
void HCluster<method>::MergeR(Matrix<Ti> &merge, Matrix<Tv> &heights,
                              std::vector<Ti> &order) const {

  HClusterNode *node;
  HClusterNode *node_left;
  HClusterNode *node_right;

  auto merge0_data = std::unique_ptr<Ti[]>(new Ti[merge.length()]);
  auto heights0_data = std::unique_ptr<Tv[]>(new Tv[heights.length()]);
  Matrix<Ti> merge0 =
      Matrix<Ti>(merge0_data.get(), merge.RowsCount, merge.ColsCount);
  Matrix<Tv> heights0 = Matrix<Tv>(heights0_data.get(), heights.length());

  for (Ti n_i = n; n_i < 2 * n - 1; n_i++) {
    node = this->Nodes.at(n_i);
    node_left = this->Nodes.at(node->idLeft);
    node_right = this->Nodes.at(node->idRight);

    auto s = n_i - n;
    heights0.Set0(s, 0, node->leftDistanceRight);
    merge0.Set0(s, 0,
                (node_left->nodesWithin > 1 ? ((int)node_left->id - (int)n + 1)
                                            : -((int)node_left->id + 1)));
    merge0.Set0(s, 1,
                (node_right->nodesWithin > 1
                     ? ((int)node_right->id - (int)n + 1)
                     : -((int)node_right->id + 1)));
  }

  heights0.SortIndicesVector(order, true);
  heights0.SortByVector(heights, order);
  // sort rows, but we should adjust the positive indexes too
  Ti j = 0;
  for (auto &i : order) {
    merge.SetRowFromRow(j, merge0, i);
    for (Ti t = 0; t < 2; t++) {
      auto d = merge.Get0(j, t) - 1;
      if (d >= 0) {
        auto it = (find(order.begin(), order.end(), (Ti)d) - order.begin());
        merge.Set0(j, t, (int)it + 1);
      }
    }
    j++;
  }
}

template <HClusterLinkage method>
Tv HCluster<method>::CalculateDistance(Ti nLeft, Ti nRight, Ti nNode,
                                       Tv disLeft, Tv disRight,
                                       Tv leftDisRight) {

  if constexpr (method == HClusterLinkage::kSingle) {
    return disLeft > disRight ? disRight : disLeft;
  } else if constexpr (method == HClusterLinkage::kComplete) {
    return disLeft > disRight ? disLeft : disRight;
  } else if constexpr (method == HClusterLinkage::kAverageU) {
    Tv weight = (Tv)nLeft / ((Tv)nLeft + (Tv)nRight);
    return weight * disLeft + ((Tv)1 - weight) * disRight;
  } else if constexpr (method == HClusterLinkage::kAverageW) {
    return (disLeft + disRight) / (Tv)2;
  } else if constexpr (method == HClusterLinkage::kWard) {
    Tv n = (Tv)(nLeft + nRight + nNode);
    Tv w_l = (Tv)(nLeft + nNode) / n;
    Tv w_r = (Tv)(nRight + nNode) / n;
    Tv w_n = (Tv)nNode / n;
    return w_l * disLeft + w_r * disRight - w_n * leftDisRight;
  } else if constexpr (true) {
    throw LdtException(ErrorType::kLogic, "hcluster", "not implemented");
  }
}

std::unique_ptr<HClusterBase> HClusterBase::GetFromType(HClusterLinkage linkage,
                                                        Ti n) {

  switch (linkage) {
  case ldt::HClusterLinkage::kSingle:
    return std::unique_ptr<HClusterBase>(
        new HCluster<HClusterLinkage::kSingle>(n));
  case ldt::HClusterLinkage::kComplete:
    return std::unique_ptr<HClusterBase>(
        new HCluster<HClusterLinkage::kComplete>(n));
  case ldt::HClusterLinkage::kAverageU:
    return std::unique_ptr<HClusterBase>(
        new HCluster<HClusterLinkage::kAverageU>(n));
  case ldt::HClusterLinkage::kAverageW:
    return std::unique_ptr<HClusterBase>(
        new HCluster<HClusterLinkage::kAverageW>(n));
  case ldt::HClusterLinkage::kWard:
    return std::unique_ptr<HClusterBase>(
        new HCluster<HClusterLinkage::kWard>(n));
  default:
    throw LdtException(ErrorType::kLogic, "hcluster",
                       "not implemented (linkage type)");
  }
}

template class ldt::HCluster<HClusterLinkage::kAverageU>;
template class ldt::HCluster<HClusterLinkage::kAverageW>;
template class ldt::HCluster<HClusterLinkage::kComplete>;
template class ldt::HCluster<HClusterLinkage::kSingle>;
template class ldt::HCluster<HClusterLinkage::kWard>;

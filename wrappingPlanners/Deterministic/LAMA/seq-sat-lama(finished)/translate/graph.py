#! /usr/bin/env python
# -*- coding: latin-1 -*-

#######################################################################
#
# Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
# (C) Copyright 2003-2004 Malte Helmert
#
# This file is part of LAMA.
#
# LAMA is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the license, or (at your option) any later version.
#
# LAMA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.
#
#######################################################################

class Graph:
  def __init__(self, nodes):
    self.nodes = nodes
    self.neighbours = dict((u, set()) for u in nodes)
  def connect(self, u, v):
    self.neighbours[u].add(v)
    self.neighbours[v].add(u)
  def connected_components(self):
    remaining_nodes = set(self.nodes)
    result = []
    def dfs(node):
      result[-1].append(node)
      remaining_nodes.remove(node)
      for neighbour in self.neighbours[node]:
        if neighbour in remaining_nodes:
          dfs(neighbour)
    while remaining_nodes:
      node = iter(remaining_nodes).next()
      result.append([])
      dfs(node)
    return result


def transitive_closure(pairs):
  # Warshall's algorithm.
  result = set(pairs)
  nodes = set(u for (u, v) in pairs) | set(v for (u, v) in pairs)
  for k in nodes:
    for i in nodes:
      for j in nodes:
        if (i, j) not in result and (i, k) in result and (k, j) in result:
          result.add((i, j))
  return sorted(result)


if __name__ == "__main__":
  g = Graph([1, 2, 3, 4, 5, 6])
  g.connect(1, 2)
  g.connect(1, 3)
  g.connect(4, 5)
  print g.connected_components()

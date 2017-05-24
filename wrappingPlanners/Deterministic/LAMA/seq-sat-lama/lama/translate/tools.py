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

def cartesian_product(sequences):
  # This isn't actually a proper cartesian product because we
  # concatenate lists, rather than forming sequences of atomic elements.
  if not sequences:
    yield []
  else:
    temp = list(cartesian_product(sequences[1:]))
    for item in sequences[0]:
      for sequence in temp:
        yield item + sequence

def permutations(alist):
  # Note: The list is changed in place as the algorithm is performed.
  #       The original value is restored when we are done.
  #       Since this is a generator, the caller must be aware
  #       of this side effect and copy the input parameter if necessary.
  # This is Knuth's "Algorithm P" ("plain changes").
  N = len(alist)
  if N <= 1:   # Special-case small numbers for speed.
    yield alist
  elif N == 2:
    yield alist
    yield [alist[1], alist[0]]
  else:
    c = [0] * N
    d = [1] * N
    while True:
      yield alist
      s = 0
      for j in xrange(N - 1, -1, -1):
        q = c[j] + d[j]
        if q == j + 1:
          if j == 0:
            return
          s += 1
        elif q >= 0:
          index1, index2 = j - c[j] + s, j - q + s
          alist[index1], alist[index2] = alist[index2], alist[index1]
          c[j] = q
          break
        d[j] = -d[j]

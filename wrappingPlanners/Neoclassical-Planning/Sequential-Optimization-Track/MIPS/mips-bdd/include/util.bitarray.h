// ***********************************************************
// 
//  Book:       Heuristic Search
// 
//  Authors:    S.Edelkamp, S.Schroedl
// 
//  See file README for information on using and copying 
//  this software.
// 
//  Project:    Mips - model checking integrated planning
//              system
// 
//  Module:     mips\include\util.bitarray.h
//  Authors:    S.Edelkamp, M.Helmert
// 
//  Low level bit vector implementation for efficient handling of 
//  bounded sets. Bit vectors are maintained as unsigned ints. 
//  Structure provides Boolean operations like assigment (=),
//  equality (==), implication (<=,>=), intersection (&), union (|),
//  and setdiff (^). Construction is invoked by maximum cardinality 
//  of set and STL vector of integers and transfer back to an 
//  vector or array of integers. Access to the vecotor by set, clear 
//  and flip. Two different hash functions can be computed and the
//  string representation be returned.
//
// ***********************************************************

#ifndef _BITARRAY_H
#define _BITARRAY_H

// ***********************************************************
// 
//  Book:       Heuristic Search
// 
//  Authors:    S.Edelkamp, S.Schroedl
// 
//  See file README for information on using and copying 
//  this software.
// 
//  Project:    Mips - model checking integrated planning
//              system
// 
//  Module:     mips\src\util.bitarray.cc
//  Authors:    S.Edelkamp, M.Helmert
// 
// ***********************************************************

#include <algorithm>    // swap
#include <vector>
#include <string>

using namespace std;

 
class BitArray {
  /** reference represents one word length entry */
  class reference {
    unsigned int mask;
    unsigned int &ref;
  public:
    reference(unsigned int m, unsigned int &r) : mask(m), ref(r) {}
    reference &operator=(bool val) {
      if(val)
        ref |= mask;
      else
        ref &= ~mask;
      return *this;
    }
    operator bool()   {return (ref & mask) != 0;}     // returns 0 or 1
    bool operator~()  {return !(bool(*this));}
  };

  enum {BITS = sizeof(unsigned int) * 8};
  unsigned int *data;
  int bitSize;
  int chunkCount;
  void init(int size);
  void assign(const BitArray &copy);
public:
  unsigned int *posn1, *posn2, *endn1;
  explicit BitArray(int size = 0)           {init(size);}
  BitArray(const BitArray &copy)            {assign(copy);}
  BitArray(int size, const vector<int> &set); // dual to toIntVector
  void refresh();
  BitArray &operator=(const BitArray &copy) {
    bitSize = copy.bitSize;
    chunkCount = copy.chunkCount;
    // data = new unsigned int[chunkCount];
    for(int i = 0; i < chunkCount; i++)
    data[i] = copy.data[i];
    // delete[] data; assign(copy); 
    return *this;
  }
  ~BitArray()                               {delete[] data;}
  void resize(int size)                     {delete[] data; init(size);}
  void swap(BitArray &other);
  int hash(int max);
  int hash(int max,int value);
  int hash2(int max,int value);

  int size() {return bitSize;}
  reference operator[](int idx) {return reference(1UL << (idx % BITS), data[idx / BITS]);}

  // methods for very fast manipulation and access
  unsigned int get(int idx) const {   // returns 0 or non-0
    return data[idx / BITS] & (1UL << (idx % BITS));
  }
  void setInt(int value, int base, int rvalue);
  void setAll(int base, int rvalue);
  void set(int idx)   {data[idx / BITS] |= 1UL << (idx % BITS);}
  void clear(int idx) {data[idx / BITS] &= ~(1UL << (idx % BITS));}
  void flip(int idx)  {data[idx / BITS] ^= 1UL << (idx % BITS);}

  BitArray &operator|=(const BitArray &other) {
    //    if(bitSize != other.bitSize) throw InvalidSizeException();
    posn1 = data; endn1 = data + chunkCount;
    posn2 = other.data;
    while(posn1 != endn1)
      *posn1++ |= *posn2++;
    return *this;
  }

  bool operator==(const BitArray &comp) {
    //  if(bitSize != comp.bitSize) return false;
    posn1 = data; endn1 = data + chunkCount;
    posn2 = comp.data;
    while(posn1 != endn1)
      if(*posn1++ != *posn2++)
        return false;
    return true;
  }

  bool operator<=(const BitArray &comp) {
    //  if(bitSize != comp.bitSize) return false;
    posn1 = data; endn1 = data + chunkCount;
    posn2 = comp.data;
    while(posn1 != endn1) 
      if(*posn2++ != (*posn2 & *posn1++))
        return false;
    return true;
  }

  BitArray &operator&=(const BitArray &other) {
    //    if(bitSize != other.bitSize) throw InvalidSizeException();
    posn1 = data; endn1 = data + chunkCount;
    posn2 = other.data;
    while(posn1 != endn1)
      *posn1++ &= *posn2++;
    return *this;
  }
  BitArray &operator^=(const BitArray &other) {
    //    if(bitSize != other.bitSize) throw InvalidSizeException();
    posn1 = data; endn1 = data + chunkCount;
    posn2 = other.data;
    while(posn1 != endn1)
      *posn1++ ^= *posn2++;
    return *this;
  }
  BitArray &operator-=(const BitArray &other) {
    //    if(bitSize != other.bitSize) throw InvalidSizeException();
    posn1 = data; endn1 = data + chunkCount;
    posn2 = other.data;
    while(posn1 != endn1)
      *posn1++ &= (-1UL ^ *posn2++);
    return *this;
  }

  vector<int> toIntVector() const;
  void toIntVector(vector<int> &vec) const;
  string toString() {
    string back;
    for (int i=0; i<bitSize; i++) 
      if (get(i)) back += "1"; else back += "0";
    back += "\n";
    return back;
  }
  void BitArray::toIntArray(int* vec, int& i) const;

  struct InvalidSizeException {};
};

#endif

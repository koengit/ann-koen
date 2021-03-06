/***********************************************************************************[SolverTypes.h]
MiniSat -- Copyright (c) 2003-2005, Niklas Een, Niklas Sorensson

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/


#ifndef SolverTypes_h
#define SolverTypes_h

#include <cassert>

//=================================================================================================
// Variables, literals, lifted booleans, clauses:


// NOTE! Variables are just integers. No abstraction here. They should be chosen from 0..N,
// so that they can be used as array indices.

typedef int Var;
#define var_Undef (-1)


struct Lit {
    int     x;

    Lit() : x(2*var_Undef)                                              { }   // (lit_Undef)
    explicit Lit(Var var, bool sign = false) : x((var+var) + (int)sign) { }
};

// Don't use these for constructing/deconstructing literals. Use the normal constructors instead.
inline  int  toInt       (Lit p)           { return p.x; }                   // A "toInt" method that guarantees small, positive integers suitable for array indexing.
inline  Lit  toLit       (int i)           { Lit p; p.x = i; return p; }     // Inverse of 'toInt()'

inline  Lit  operator   ~(Lit p)           { Lit q; q.x = p.x ^ 1; return q; }
inline  bool sign        (Lit p)           { return p.x & 1; }
inline  int  var         (Lit p)           { return p.x >> 1; }
inline  Lit  unsign      (Lit p)           { Lit q; q.x = p.x & ~1; return q; }
inline  Lit  id          (Lit p, bool sgn) { Lit q; q.x = p.x ^ (int)sgn; return q; }

inline  bool operator == (Lit p, Lit q)    { return toInt(p) == toInt(q); }
inline  bool operator != (Lit p, Lit q)    { return toInt(p) != toInt(q); }
inline  bool operator <  (Lit p, Lit q)    { return toInt(p)  < toInt(q); }  // '<' guarantees that p, ~p are adjacent in the ordering.


const Lit lit_Undef(var_Undef, false);  // }- Useful special constants.
const Lit lit_Error(var_Undef, true );  // }


//=================================================================================================
// Lifted booleans:


class lbool {
    char     value;
    explicit lbool(int v) : value(v) { }

public:
    lbool()       : value(0) { }
    lbool(bool x) : value((int)x*2-1) { }
    int toInt(void) const { return value; }

    bool  operator == (const lbool& other) const { return value == other.value; }
    bool  operator != (const lbool& other) const { return value != other.value; }
    //lbool operator ~  (void)               const { return lbool(-value); }
    lbool operator ^ (bool b) const { return b ? lbool(-value) : lbool(value); }

    friend int   toInt  (lbool l);
    friend lbool toLbool(int   v);
};
inline int   toInt  (lbool l) { return l.toInt(); }
inline lbool toLbool(int   v) { return lbool(v);  }

const lbool l_True  = toLbool( 1);
const lbool l_False = toLbool(-1);
const lbool l_Undef = toLbool( 0);

//=================================================================================================
// Clause -- a simple class for representing a clause:

class Clause {
    unsigned int size_etc;
    union { float act; unsigned int abst; } apa;
    Lit     data[0];
public:
    // NOTE: This constructor cannot be used directly (doesn't allocate enough memory).
    template<class V>
    Clause(const V& ps, bool learnt) {
        size_etc = (ps.size() << 3) | (unsigned int)learnt;
        for (int i = 0; i < ps.size(); i++) data[i] = ps[i];
        if (learnt) apa.act = 0; else apa.abst = 0; }

    // -- use this function instead:
    template<class V>
    static Clause* Clause_new(const V& ps, bool learnt = false) {
        assert(sizeof(Lit)      == sizeof(unsigned int));
        assert(sizeof(float)    == sizeof(unsigned int));
        void* mem = malloc(sizeof(Clause) + sizeof(unsigned int)*(ps.size()));
        return new (mem) Clause(ps, learnt); }

    int          size        ()      const   { return size_etc >> 3; }
    void         shrink      (int i)         { assert(i <= size()); size_etc = (((size_etc >> 3) - i) << 3) | (size_etc & 7); }
    void         pop         ()              { shrink(1); }
    bool         learnt      ()      const   { return size_etc & 1; }
    unsigned int mark        ()      const   { return (size_etc >> 1) & 3; }
    void         mark        (unsigned int m){ size_etc = (size_etc & ~6) | ((m & 3) << 1); }
    const Lit&   last        ()      const   { return data[size()-1]; }
    Lit          operator [] (int i) const   { return data[i]; }
    Lit&         operator [] (int i)         { return data[i]; }
    operator const Lit* (void) const         { return data; }
    
    float&       activity    ()              { return apa.act; }
    unsigned int abstraction ()      const   { return apa.abst; }

    void calcAbstraction() {
        unsigned int abstraction = 0;
        for (int i = 0; i < size(); i++)
            abstraction |= 1 << (var(data[i]) & 31);
        apa.abst = abstraction;  }
};


//=================================================================================================
// TrailPos -- Stores an index into the trail as well as an approximation of a level. This data
// is recorded for each assigment. (Replaces the old level information)


class TrailPos {
    int tp;
 public:
    explicit TrailPos(int index, int level) : tp( (index << 5) + (level & 31) ) { }

    friend int abstractLevel(const TrailPos& p) { return 1 << (p.tp & 31); }
    friend int position     (const TrailPos& p) { return p.tp >> 5; }

    bool operator ==  (TrailPos other) const { return tp == other.tp; }
    bool operator <   (TrailPos other) const { return tp <  other.tp; }
};

#endif

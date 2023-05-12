// https://www.catch22.net/tuts/neatpad/piece-chains/
//
// Piece Chains are the perfect fngi data structure for modifying text.
// * Their parent type can be a SllSlc (nice!). This means that algorithms
//   (like find, regex, etc) target the SllSlc
// * They are very useful for editing, since they can be split up easily

#include <stdint.h>
#include <stddef.h>
#define U1  unsigned char
#define U2  uint16_t
#define S   size_t

typedef struct {void* data; void* methods;} Arena; // See civc for actual definition

typedef struct {
  struct SllSlc* next; // Sll
  U1* dat;    U2 len;  // Slc
} SllSlc;

typedef struct {
  struct Piece* next; // Sll
  U1* dat;    U2 len; // Slc
  struct Piece* prev;
} Piece;

// Casting is safe since they have identical memory layout in their prefix. This
// means that methods implemented for SllSlc also work for Piece.
static inline SllSlc* Piece_toSllSlc(Piece* p) { return (SllSlc*)p; }

// When doing search the node and the offset of (i.e.) the start of a match are
// returned.
// This is done in-case modifications need to be made on the ref node after
// searching. It's trivial to convert an SllSlcRef into an SllSlc if that's
// what is wanted.
typedef struct { SllSlc* ref; U2 offset; } SllSlcRef;
SllSlc SllSlcRef_toSllSlc(SllSlcRef* r) {
  SllSlc* ref = r->ref;
  return (SllSlc) {
    .next = ref->next,
    .dat = ref->dat + r->offset,
    .len = ref->len - r->offset,
  };
}

// Search in haystack for needle, returning it's location.
// Note that this works with Piece as well, but the `ref`
// will be a pointer to Piece instead of SllSlc (typecast needed in C,
// overriden unty implementation needed in fngi).
SllSlcRef SllSlc_find(SllSlc* haystack, SllSlc needle);

//// Insertion
// The below demonstrate the power of Piece: modifying text.
//
// The allocator is used for allocating new Piece nodes. Note that
// **no string buffers are ever allocated**. Any Slcs simply point to
// (pieces of) the original data or the "replacement" strings given.
//
// Note that supporting undo/redo would require something like the spanrange
// and a slightly different implementation. This is fine -- the base string
// manip will not support undo/redo and separate methods will be built for
// supporting that. The Piece data structure does not change!

// Insert ss into p at index. This mutates p
Piece* Piece_insert(Piece* p, Arena a, S index, SllSlc ss);

// Replace all instances of pattern with replacement.
Piece* Piece_replace(Piece* p, Arena a, SllSlc pattern, SllSlc replacement);

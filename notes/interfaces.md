I love interfaces (java/js/golang/etc), also called traits in rust. I think they
are a fantastic way to specify a set of _actions_ that a particular piece of
_data_ can perform.

In some previous versions of fngi I claimed to want to do "rust style traits",
and I recently came across
[this tweet](https://merveilles.town/@neauoire/107038809041856242) expressing
concern on this. I wanted to get a few of my thoughts down on what a fngi
interface would look like.

## C layout

Before we talk about fngi, let's discuss C. Let's say you were designing a
Farming game. The farming game has animals, each with a favorite food and
a different way of handling movement.

> Clearly this is all really ugly in C. Neither the C compiler, nor the language
> syntax help you. I believe these can be implemented more "natively" and in a
> type-checked way without much complexity in fngi, see the botom of this
> document.

```
typedef struct {
  size_t lastSaidIndex;
  Meat favoriteMeat;
} Dog;

typedef struct {
  size_t lastSaidIndex;
  Grass favoriteGrass;
} Goat;
```

Let's also say there were a set of methods you wanted to pass around with these:

```
typedef struct {
  char* (*talk)(void* animal);               // "talk" function pointer
  char* (*move)(void* animal, int x, int y); // "move to x,y" function pointer
} Animal;
```

One way you could make Dog or Goat a virtual type is to essentially mimic
Rust/C++ "fat pointers" and pass around a pointer to the (global) method
implementation along with a pointer to the data.

```
char* goatTalk(Goat* goat) {
  print(goatWords[goat->lastSaidIndex]);
  goat->lastSaidIndex += 1;
}

// ... etc for goatMove

Animal goatMethods = (Animal) {
  .talk = goatTalk;
  .move = goatMove;
};

void talkAndMove(Animal* m, void* animal, int16_t x, int16_t y) {
  printf(m->talk(animal));
  m->move(animal, x, y);
}

// and then in your code
talkAndMove(&goatMethods, &myGoat, 5, 10);
```

The advantage here is radical simplicity over implementing a vtable, which would
(at least) involve something like multiple layered "perfect hash" tables of
organized type ids (or something), fundamentally pointing to the method. The
compiler would then need to bundle some kind of syntax to generate all of this
for the programmer.

However, there are several real disadvantages over rust/C++ virtual traits:

* There is no type unification. For instance you can't specify both the Animal
  and Equal interface simultaniously.
  * Instead, you would have to pass in two pointers.
  * Or you have to have a AnimalEqual struct and pass that in instead.
* This doesn't implicitly support code generation: the compiler has no way to
  know that these functions are implemented for these types, and therefore can't
  generate code which uses this knowledge. All that is supported is minimalistic
  runtime lookup.

Neither of these are overly concerning to me. Dynamic types are an extremely
useful tool, but a tool that should be used sparingly. It's great to have things
like "reader/writer objects" for bytestreams or "loggers" that can be swapped
out at runtime, or can be changed when passed to certain functions. Testing
is another area: a state-heavy piece of code can benefit immensely from being
able to having a Fake used for testing clients of it.

On the code generation front, fngi supports insanely powerful macros instead. If
you want to generate a function for multiple types then it should be possible to
create a set of INSTANT functions (macros) to help you with that; but as a
library, not as part of the core language. IMO creating bloat with something
like code generation should be explicit, not implicit in using a new concrete
type with a function.

## How it would look in fngi

In fngi, a `Struct` or `Enum` is it's own dictionary/namespace. Therefore you
can define methods for it and the dot compiler (name compiler) will know to
check if the top stack item is the struct itself and pass in a reference if so.
This allows you to do `.animal.walk(5, 6)`, etc.

For interfaces, you can simply do:

```
\ Define animal interface
interface Animal [
  fn talk [#Animal];
  fn move [x:U2, y:U2, #Animal];
]

\ Stores an Animal implementation in Goat
implement Goat do Animal [
  talk = goatTalk; \ previously defined method
  move = goatMove; \ previously defined method
]

fn talkAndMove [x: U2, y: U2, animal: #Animal] do (
  logUser(.animal.talk());
  .animal.move(.x, .y);
)

\ and then in the code
talkAndMove(.myGoat#Animal, 5, 10)
```

the `.` are the "dot compiler" (variable lookup) and `#` is an explicit "fat
pointer" syntax, which is really two values on the stack. The dot compiler sees
this syntax and hands off the `myGoat` struct pointer and the Animal token to
the sub-compiler for `#`.  This isn't _quite_ as "clean" as rust -- but
explicitness is a goal of fngi whenever possible.

I could also imagine being able to create "interface bundles" (i.e. AnimalEqual)
which are a struct of pointers to the interface implementations. As for passing
two interface implementations for the same animal... that's probably not
supported by the syntax. That really should be rarely necessary, and you could
(of course) pass the same data twice for the rare cases it is needed.

## What about that favorite food?
These have all been concrete types, aka non-generic. What about if you wanted to
have a `fn feed [Food]` method, where `Food` is generic over `Grass` and `Meat`?

Haha, gotcha! There is no way in Hel fngi will support generics.  However,
`Food` could be an Enum... or even an interface type itself!


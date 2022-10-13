TCL is a cool language. I'm not sold on the "everything is a string" but it
certainly does it far better than ba(sh).
- [TCL The Misunderstood](http://antirez.com/articoli/tclmisunderstood.html)
  was a fun read. I think basing the macro system (partly) on how TCL does it
  makes some sense. This would map quite easily to fngi, which could expand
  a string template and set the compiler object until it's compiled quite
  seamlessly.
- Pure [TCL editor](https://github.com/slebetman/tcled/blob/master/tcled) in
  ~3k lines.

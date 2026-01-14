# Calyx

Calyx is a dependently typed language, with a focus on pragmatism, minimalism, and targeting constrained environments like shaders and bare metal applications.

This is in early stages, only two weeks old: we are currently compiling to JS while working out semantics, and while it can produce legal programs (see fibonacci example in [example.calyx](example.calyx)), expect things to break often.

Calyx is not intended to be a proof assistant. Its type system is motivated by the desire to avoid awkward constructs like kind systems, const generics, and GADTs. Many of the features that motivate these constructs fall out of simple Normalization by Evaluation style type checkers, Calyx aims to leverage that.

Planned features include:
- Row polymorphic records
- First-class lenses
- Algebraic effects
- Partial evaluation and compile-time reflection in place of macros
- A simple 'Prop' sort for refinement types

# Usage

You can get a taste of the syntax in [example.calyx](example.calyx) (more examples to come), simply call
```bash
dune exec calyx -- my_calyx_file.calyx
```
to run your program and see the compiled output.

If you use Calyx, an experience report would be greatly appreciated!


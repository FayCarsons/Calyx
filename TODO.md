
- [ ] Implicits
  - We want something similar to Lean or Agda here, implicit function params for type variables
  - Explicit syntax: `def id {a} (x : a) -> a` or `def id {a : Type} (x : a) -> a`
  - Implicit syntax: `def id (x : a) -> a`
  - This should be simple, add either an `is_implicit` field to `Pi` or similar
    - When encountering an implicit `Pi`, we just generate an `Equals` constraint like:
      ```haskell 
      let f : {a} -> a -> a = \x -> x in 
      let x = 42 in 
      f x
      ```
      Produces `Equals (Var x) ?M` where `?M` is the meta representing the `{a}` in `f`.
  - In the future we should add implicit forms that do some accelerated search 
    through a prefix tree of typeclass instances, like `[S : Show a]`
- [ ] Modularize backend 
  - There should be some module functor which takes a DSL mapping IR to string, 
    identifier replacement map, and standard library 
  - And returns a module with a single 'compile' function of 'toplevel_declaration list -> string'

- [ ] Fix record typing 
  - Record types may have one of: 
    1. An implicit tail `{ x : Int, y : Int }`
    2. An explicit tail `{ x : Int, y : Int | rest }`
    3. An explicitly closed structure `{ x : Int, y : Int !}`
  - Currently, only closed records are supported (if that, pending refactors etc)
  - In adding support we should add property tests asserting: 
    - `is_subtype t t = true`
    - `is_subtype { x : Int } { x : Int, y : Int} = true`
    - Other things I'm not thinking of, idk TODO

- [ ] Use Gospel for formal verification
- [ ] Add sum types
  - Difficult for WGSL backend as we cannot cast types of arbitrary value
    so some trickery with `array<u32>` and manual unpacking may be necessary
  - Similar row polymorphism:
    - Implicit tail `[ Red | Green | Blue ]`
    - Explicit tail `[ Red | Green | Blue | {rest} ]` (syntax could be better)
    - Explicitly closed `[ Red | Green | Blue !]`

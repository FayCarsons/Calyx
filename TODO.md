- Fix Type declaration and representation
  - Currently have a mess of `RecordDecl`/`RecordType` + `SumDecl`/`SumType`
  - This should be one structure which holds:
    1. Parameters
    2. Indices
    1. Kind, a pi type describing the type (i.e. `Option` is `Type -> Type`, `Vector` is `Type -> Nat -> Type`)
    2. Structure, `Product` is `label:Ident.t * type:Term.t`, `Sum` is `constructor:Ident.t * args:(Term.t list)`
    3. A cached list of either the types of fields or the types of members
- Add ANF IR w/ joins
  - This requires ensuring all bindings have some unique identifier, like giving `Let` + `Pi` + `Lam` a `stamp:int` parameter
  - Type information becomes `Repr.t`
    - This is maybe two types: 
      1. `Scalar` for `Int`, `Float`, `Char` etc 
      2. `Struct` for records and constructors
  - Handle closures:
    1. Do lambda lifting + monomorphization where possible
    2. Do closure conversion everywhere else
  - Add some basic optimizations while I'm at it probably
- Module system
  - Modules desugar to records plus insertion in instance resolution prefix tree
  - Figure out how module binding works:
    - Current thought is lexical binding via explicit `with` operator that we also use for toplevel function bindings a la Haskell
- C Backend
  - Want to put Calyx on a microcontroller with a display to show off

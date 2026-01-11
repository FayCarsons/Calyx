- Modularize backend 
  - There should be some module functor which takes a DSL mapping IR to string, identifier replacement map, and standard library 
  - And returns a module with a single 'compile' function of 'toplevel_declaration list -> string'

- Fix record typing 
  - If a record literal is in the type position (i.e. `{ name : String, age : Int }`), it is structural and creates 'HasField' constraints against arguments the function is applied to
  - Argument types which are identifiers are nominal, always
  - Not quite ideal? 
    - We could differentiate nominal and structural record declarations like 'shape Foo' vs 'data Foo'?

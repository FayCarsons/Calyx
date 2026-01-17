```calyx
-- Product types are similar to Lean
data MyProduct where 
  x : Int,
  y : Int

data Nat where
  | Succ Nat 
  | Zero

-- You can declare a generic parameter like this:
data Annotated {a} where 
  inner : a,
  annotation : String

-- Or like this:
data Annotated (a : Type) where 
  inner : a,
  annotation : String


{-
  In the future we will have GADT style syntax for indexed types, like: 
  data Vector (a : Type) : Nat -> Type where 
    | Cons : a -> Vector a n -> Vector a (Succ n)
    | Nil : Vector a Zero

-}

-- 'if' and 'match' are terminated by the 'end' keyword
-- In the future we may make this optional, so it may be used only when scope 
-- is ambiguous
def fibonacci (n : Int) -> Int do 
  if n < 2 then 
    n 
  else 
    fibonacci (n - 1) + fibonacci (n - 2)
  end

-- Generic parameters can be implicit
def map (self : Option a) (f : a -> b) -> Option b do 
  match self with 
  | Some x -> Some (f x)
  | None -> None
  end

{-
  We may have multi-line comments in which we talk
  and talk
  and talk
  to our hearts content.
-}

def bind {a, b} (self : Option a) (f : a -> Option b) -> Option b do 
  match self with 
  | Some x -> f x 
  | None -> None
  end

-- All toplevels must have a signature
const Zero : Int = 0
const IgnoreList : Vector String 4 = [".gitignore", ".DS_Store", ".env", "*.lock"]

-- Entry point is always main
def main () -> Int do 
  fibonacci 42
```

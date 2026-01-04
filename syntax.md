
```rust 
def main (argc : Int) (argv : List String) -> IO Unit do 
  Iter.toex argc (print . argv.get)
end
```

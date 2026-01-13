module Ident = struct
  type t = (int64[@opaque])

  let equal = Int64.equal
  let compare = Int64.compare
  let hash = Int64.hash
  let succ : t -> t = Int64.succ
  let ofInt : int -> t = Int64.of_int
  let toInt : t -> int = Int64.to_int
end

module Intern = struct
  module Bloom = Bloom.String
  module Table = Hashtbl.Make (String)

  type interner =
    { bloomFilter : Bloom.t
    ; stringToIdent : Ident.t Table.t
    ; identToString : string Vector.t
    ; mutable counter : Ident.t
    }

  let _add : interner -> string -> Ident.t =
    fun self s ->
    let index = self.counter in
    Table.add self.stringToIdent s index;
    Vector.push self.identToString s;
    Bloom.add self.bloomFilter s;
    self.counter <- Ident.succ self.counter;
    assert (Ident.equal (Ident.ofInt @@ Vector.length self.identToString) self.counter);
    index
  ;;

  let _intern : interner -> string -> Ident.t =
    fun self s ->
    if Bloom.member self.bloomFilter s
    then (
      match Table.find_opt self.stringToIdent s with
      | Some ident -> ident
      | None -> _add self s)
    else _add self s
  ;;

  let _lookup : interner -> Ident.t -> string =
    fun self index ->
    match Vector.get self.identToString @@ Ident.toInt index with
    | Some s -> s
    | None -> failwith "Something has gone horribly wrong"
  ;;

  let default () =
    { bloomFilter = Bloom.create ~size:8192 ~numHashes:3
    ; stringToIdent = Table.create 1024
    ; identToString = Vector.create ~capacity:1024 ()
    ; counter = 0L
    }
  ;;

  let _global = default ()
  let intern : string -> Ident.t = _intern _global
  let lookup : Ident.t -> string = _lookup _global
  let underscore = intern "_"
end

module Map = Map.Make (Ident)
include Ident

let%test "intern then lookup returns original string" =
  let id = Intern.intern "hello" in
  String.equal (Intern.lookup id) "hello"
;;

let%test "same string returns same ident" =
  let id1 = Intern.intern "foo" in
  let id2 = Intern.intern "foo" in
  Ident.equal id1 id2
;;

let%test "different strings return different idents" =
  let id1 = Intern.intern "bar" in
  let id2 = Intern.intern "baz" in
  not (Ident.equal id1 id2)
;;

let%test "underscore is pre-interned" =
  let id = Intern.intern "_" in
  Ident.equal id Intern.underscore
;;

let%test_unit "intern/lookup roundtrip" =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~count:100
       ~name:"intern then lookup returns original"
       QCheck.(string_of_size (Gen.int_range 1 50))
       (fun s ->
          let id = Intern.intern s in
          String.equal (Intern.lookup id) s)
;;

let%test_unit "interning same string is idempotent" =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~count:100
       ~name:"interning same string twice returns same id"
       QCheck.(string_of_size (Gen.int_range 1 50))
       (fun s ->
          let id1 = Intern.intern s in
          let id2 = Intern.intern s in
          Ident.equal id1 id2)
;;

let%test_unit "different strings get different ids" =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~count:100
       ~name:"different strings get different ids"
       QCheck.(
         pair (string_of_size (Gen.int_range 1 20)) (string_of_size (Gen.int_range 1 20)))
       (fun (s1, s2) ->
          if String.equal s1 s2
          then true
          else (
            let id1 = Intern.intern s1 in
            let id2 = Intern.intern s2 in
            not (Ident.equal id1 id2)))
;;

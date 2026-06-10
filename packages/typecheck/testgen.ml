open Core
open Term
module Intern = Ident.Intern

(** Support for the datatype-invariant property tests (inline tests in
    [Checker], [Zonk], [Positivity], [Ir] and [Codegen_js]): random ADT
    specifications and Term-level program builders. Depends only on [Term] and
    [Context] so every test site can use it without module cycles. *)

type field_spec =
  | FInt
  | FBool
  | FParam of int
  | FRec
[@@deriving sexp]

type adt_spec =
  { n_params : int
  ; ctor_fields : field_spec list list
    (* constructor 0 is recursion-free: the base case for value trees *)
  }
[@@deriving sexp]

let no_pos = Pos.pos_empty, Pos.pos_empty
let data_name = Intern.intern "PropD"
let int_name = Intern.intern "Int"
let bool_name = Intern.intern "Bool"
let param_name i = Intern.intern (Printf.sprintf "pp%d" i)
let ctor_name i = Intern.intern (Printf.sprintf "PropC%d" i)
let field_binder i j = Intern.intern (Printf.sprintf "fld%d_%d" i j)

let stdlib : Context.entry Ident.Map.t =
  Ident.Map.of_alist_exn
    [ int_name, Context.Typed (`Opaque, `Type)
    ; bool_name, Context.Typed (`Opaque, `Type)
    ]
;;

let n_ctors spec = List.length spec.ctor_fields

let params_of_spec spec : (Ident.t * Term.t) list =
  List.init spec.n_params ~f:(fun i -> param_name i, (`Type : Term.t))
;;

let app_spine (f : Term.t) (xs : Term.t list) : Term.t =
  List.fold xs ~init:f ~f:(fun acc x -> `App (acc, x))
;;

let applied_data spec : Term.t =
  app_spine
    (`Var data_name)
    (List.init spec.n_params ~f:(fun i -> (`Var (param_name i) : Term.t)))
;;

let field_syntax spec : field_spec -> Term.t = function
  | FInt -> `Var int_name
  | FBool -> `Var bool_name
  | FParam i when spec.n_params > 0 -> `Var (param_name (i mod spec.n_params))
  | FParam _ -> `Var int_name
  | FRec -> applied_data spec
;;

let adt_of_spec spec : Term.t Term.sum_type =
  let constructors =
    List.mapi spec.ctor_fields ~f:(fun i fields ->
      ctor_name i, List.map fields ~f:(field_syntax spec))
    |> Ident.Map.of_alist_exn
  in
  { ident = data_name; params = params_of_spec spec; constructors; position = no_pos }
;;

(** [D] with every parameter instantiated to [Int]. *)
let inst_type spec : Term.t =
  app_spine (`Var data_name) (List.init spec.n_params ~f:(fun _ -> (`Var int_name : Term.t)))
;;

(** A closed constructor tree of type [inst_type spec]. *)
let rec value_tree spec depth : Term.t =
  let pick = if depth <= 0 then 0 else depth mod n_ctors spec in
  let fields = List.nth_exn spec.ctor_fields pick in
  let arg = function
    | FInt -> (`Lit (Int 42) : Term.t)
    | FBool -> `Lit (Bool true)
    | FParam _ -> `Lit (Int 7)
    | FRec -> value_tree spec (depth - 1)
  in
  app_spine (`Var (ctor_name pick)) (List.map fields ~f:arg)
;;

let constant_of spec depth : Term.t Term.declaration =
  Constant
    { ident = Intern.intern "propVal"
    ; typ = inst_type spec
    ; body = value_tree spec depth
    ; position = no_pos
    }
;;

(** Match arm for constructor [i]: flat variable patterns. The body returns the
    first field when it is Int-typed (exercising arm binder types), otherwise
    an Int literal. *)
let arm_for spec i : Term.t Term.pattern * Term.t =
  let fields = List.nth_exn spec.ctor_fields i in
  let pat =
    PCtor (ctor_name i, List.mapi fields ~f:(fun j _ -> PVar (field_binder i j)))
  in
  let body : Term.t =
    match fields with
    | (FInt | FParam _) :: _ -> `Var (field_binder i 0)
    | _ -> `Lit (Int i)
  in
  pat, body
;;

(** A function matching a [D Int ...] scrutinee with arms for the constructors
    in [idxs]. *)
let match_fn ?(catch_all = false) spec idxs : Term.t Term.declaration =
  let x = Intern.intern "propScrut" in
  let arms = List.map idxs ~f:(arm_for spec) in
  let arms = if catch_all then arms @ [ PWild, (`Lit (Int 99) : Term.t) ] else arms in
  let typ : Term.t =
    `Pi { plicity = Explicit; ident = x; dom = inst_type spec; cod = `Var int_name }
  in
  let body : Term.t = `Lam (Explicit, x, `Match (`Var x, arms)) in
  Function { ident = Intern.intern "propConsume"; typ; body; position = no_pos }
;;

let consume_fn spec = match_fn spec (List.init (n_ctors spec) ~f:Fn.id)

(** Full program: declaration, a constructor-tree constant, a total match. *)
let program spec depth : Term.t Term.declaration list =
  [ SumDecl (adt_of_spec spec); constant_of spec depth; consume_fn spec ]
;;

(* {1 Leak detection}

   Encodings must never escape conversion: elaborated/zonked terms may not
   contain [`Self] nodes or any "$"-prefixed identifier (the reserved prefix
   for encoding binders). *)

let dollar id = String.is_prefix (Intern.lookup id) ~prefix:"$"

let rec term_clean : Term.t -> bool = function
  | `Self _ -> false
  | `Var x -> not (dollar x)
  | `App (f, x) -> term_clean f && term_clean x
  | `Infix { left; op; right } -> term_clean left && term_clean op && term_clean right
  | `Ann (x, t) -> term_clean x && term_clean t
  | `Type | `Err _ | `Meta _ -> true
  | `Lit lit -> literal_clean lit
  | `Proj (t, _) -> term_clean t
  | `Match (scrut, arms) ->
    term_clean scrut
    && List.for_all arms ~f:(fun (pat, body) -> pattern_clean pat && term_clean body)
  | `Lam (_, x, b) -> (not (dollar x)) && term_clean b
  | `Pi { ident; dom; cod; _ } -> (not (dollar ident)) && term_clean dom && term_clean cod
  | `Let (x, t, v, b) ->
    (not (dollar x))
    && Option.value_map t ~default:true ~f:term_clean
    && term_clean v
    && term_clean b
  | `Pos (_, t) -> term_clean t
  | `RecordType { fields; tail } ->
    List.for_all (Map.data fields) ~f:term_clean
    && Option.value_map tail ~default:true ~f:term_clean

and pattern_clean : Term.t Term.pattern -> bool = function
  | PVar x -> not (dollar x)
  | PWild -> true
  | PCtor (c, ps) -> (not (dollar c)) && List.for_all ps ~f:pattern_clean
  | PLit lit -> literal_clean lit
  | PRec fields -> List.for_all fields ~f:(fun (_, p) -> pattern_clean p)

and literal_clean : Term.t Term.literal -> bool = function
  | Record fields -> List.for_all (Map.data fields) ~f:term_clean
  | Int _ | UInt _ | Float _ | Bool _ -> true
;;

let decl_clean : Term.t Term.declaration -> bool = function
  | Function { typ; body; _ } | Constant { typ; body; _ } ->
    term_clean typ && term_clean body
  | RecordDecl _ | SumDecl _ -> true
;;

let term_eq (a : Term.t) (b : Term.t) : bool =
  Sexp.equal (Term.sexp_of_t a) (Term.sexp_of_t b)
;;

(* {1 Generators} *)

let gen_adt_spec : adt_spec QCheck.Gen.t =
  let open QCheck.Gen in
  int_range 0 2
  >>= fun n_params ->
  let param_fields =
    if n_params > 0 then [ map (fun i -> FParam i) (int_bound (n_params - 1)) ] else []
  in
  let base_field = oneof ([ return FInt; return FBool ] @ param_fields) in
  let any_field = oneof ([ return FInt; return FBool; return FRec ] @ param_fields) in
  int_range 1 4
  >>= fun n_ctors ->
  list_size (int_range 0 3) base_field
  >>= fun base ->
  list_size (return (n_ctors - 1)) (list_size (int_range 0 3) any_field)
  >>= fun rest -> return { n_params; ctor_fields = base :: rest }
;;

let print_spec s = Sexp.to_string_hum (sexp_of_adt_spec s)
let arb_adt_spec = QCheck.make ~print:print_spec gen_adt_spec

(** Spec plus a valid constructor index. *)
let arb_adt_with_index =
  let gen =
    let open QCheck.Gen in
    gen_adt_spec >>= fun spec -> int_bound (n_ctors spec - 1) >>= fun i -> return (spec, i)
  in
  QCheck.make ~print:(fun (s, i) -> Printf.sprintf "%s, ctor %d" (print_spec s) i) gen
;;

(** Spec plus a value-tree depth. *)
let arb_adt_with_depth =
  let gen =
    let open QCheck.Gen in
    gen_adt_spec >>= fun spec -> int_bound 3 >>= fun d -> return (spec, d)
  in
  QCheck.make ~print:(fun (s, d) -> Printf.sprintf "%s, depth %d" (print_spec s) d) gen
;;

(** Spec with at least two constructors, plus a proper nonempty subset mask. *)
let arb_adt_with_subset =
  let gen =
    let open QCheck.Gen in
    gen_adt_spec
    >>= fun spec ->
    let spec =
      if n_ctors spec < 2
      then { spec with ctor_fields = spec.ctor_fields @ [ [] ] }
      else spec
    in
    let n = n_ctors spec in
    int_range 1 ((1 lsl n) - 2) >>= fun mask -> return (spec, mask)
  in
  QCheck.make ~print:(fun (s, m) -> Printf.sprintf "%s, mask %d" (print_spec s) m) gen
;;

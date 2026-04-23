open Types;;
open Exceptions;;
open Utils;;

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = 
  (*
    This function is responsible for generating a string from a given type.
    This string will be representative of the type.

    Parameters
    ty : ty

    Returns : string
  *)
  match ty with
    (AtTy TyBool) ->
      "Bool"
  | (AtTy TyNat) ->
      "Nat"
  | TyArr (ty1, ty2) ->
      let ty1str, ty2str = string_of_ty ty1, string_of_ty ty2 in
      (print_ty_paren ty1str ty1) ^ " -> " ^ (print_ty_paren ty2str ty2)
  | TyFix ->
      "Fix"
  | TyTop ->
      "Top"
  | TyBot ->
      "Bot"
  | AtTy TyString ->
      "String"
  | TyTuple ty_list ->
      no_last_comma ("(" ^ (List.fold_left (fun acc x -> acc ^ (string_of_ty x) ^ ",") "" ty_list) ^ ")")
  | TyList [] ->
      "'a list"
  | TyList ty_list ->
      no_last_comma ("[" ^ (List.fold_left (fun acc x -> acc ^ (string_of_ty x) ^ ",") "" ty_list) ^ "]")
  | TyRecord (name, fields) ->
      no_last_comma (name ^ " = {" ^ (List.fold_left (fun acc (x,y) -> acc ^ x ^ ":" ^ (string_of_ty y) ^ ";") "" fields) ^ "}")
  | TyVariant (name, fields) ->
      no_last_comma (name ^ " = <" ^ (List.fold_left (fun acc (x,y) -> acc ^ x ^ ":" ^ (string_of_ty y) ^ ";") "" fields) ^ ">")
  | AtTy (TyVar x) -> x
;;

let rec is_subtype tyS tyT =
  (*
    This function will check if tyS is a subtype of tyT.
    The subtyping criteria are explained in the user manual.

    Parameters:
    tyS : ty
    tyT : ty

    Returns : bool
  *)
  match (tyS, tyT) with
  _ when tyS=tyT -> true
  | _, TyTop -> true
  | TyBot, _ -> true
  (* T1 <: S1 y S2 <: T2 *)
  | TyArr (s1, s2), TyArr (t1, t2) ->
      is_subtype t1 s1 && is_subtype s2 t2
  | TyRecord (_, fieldsS), TyRecord (_, fieldsT) ->
      List.for_all
        (fun (labelT, tyT) ->
          try
            let tyS = List.assoc labelT fieldsS in
            is_subtype tyS tyT
          with
            Not_found -> false)
        fieldsT

  (* Base case: not a subtype *)
  | _ -> false

let rec typeof ctx tm =
  (*
    This function will return the type of a given term/expression, while also performing type checking

    Parameters:
    ctx : ty_context
    tm : term

    Returns : ty
  *)
  match tm with
    (* T-True *)
    (AtTerm TmTrue) ->
      (AtTy TyBool)

    (* T-False *)
  | (AtTerm TmFalse) ->
      (AtTy TyBool)

    (* T-String *)
  | (AtTerm (TmString _)) ->
      (AtTy TyString)

    (* T-Concat *)
  | TmConcat (t1, t2) ->
      if typeof ctx t1 = AtTy (TyString) && typeof ctx t2 = AtTy (TyString) then AtTy (TyString)
      else raise (Type_error "Concatenating a non string object")
    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = (AtTy TyBool) then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | (AtTerm TmZero) ->
      (AtTy TyNat)

    (* T-Succ *)
  | AtTerm (TmSucc t1) ->
      if typeof ctx t1 = (AtTy TyNat) then (AtTy TyNat)
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | AtTerm (TmPred t1) ->
      if typeof ctx t1 = (AtTy TyNat) then (AtTy TyNat)
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | AtTerm (TmIsZero t1) ->
      if typeof ctx t1 = (AtTy TyNat) then (AtTy TyBool)
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | AtTerm (TmVar x) ->
      (try ty_getbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-TyVar *)
  | AtTerm (TmTyVar x) ->
      (try ty_getbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ty1 =
      (match tyT1 with
        AtTy (TyVar x) -> 
          (
            try
              ty_getbinding ctx x
            with
              _ -> raise (Type_error ("no binding type for variable " ^ x))
          )
        | _ -> tyT1) in
      let ctx' = ty_addbinding ctx (x, ty1) in
      let tyT2 = typeof ctx' t2 in
      TyArr (ty1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if is_subtype tyT2 tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = ty_addbinding ctx (x, tyT1) in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix t -> TyFix

    (* T-Letrec *)
  | TmLetRec (f, ty, t1, t2) ->
      let ctx' = ty_addbinding ctx (f, ty) in
      typeof ctx' t2

    (* T-Assign *)
  | TmAssign (id, t) ->
      typeof ctx t

    (* T-Tuple *)
  | TmTuple tm_list -> 
      TyTuple (List.map (typeof ctx) tm_list)

    (* T-Proj *)
  | TmProj (tuple, nth) ->
      let tyt = typeof ctx tuple in
      (
        match tyt with
        TyTuple ty_list ->
          if nth < List.length ty_list then List.nth ty_list nth
          else raise (Type_error "Projection index out of bounds")
        | _ -> raise (Type_error "Expected a tuple for projection")
      )

    (* T-List *)
  | TmList (h::t) -> 
      let list_ty = typeof ctx h in
      if (List.for_all (function x -> typeof ctx x = list_ty) t) then
        TyList [list_ty](*(List.map (typeof ctx) (h::t))*)
      else
        raise (Type_error "Type mismatch: All elements of the list should be the same type")

    (* T-EmptyList *)
  | TmList [] ->
      TyList []

    (* T-Head *)
  | TmHead tm_list ->
      let t = typeof ctx tm_list in
      (
        match t with
          TyList (h::t) -> h
          | TyList [] -> raise (Type_error "Couldn't apply head to empty list")
          | _ -> raise (Type_error "A list was expected")
      )

    (* T-Tail *)
  | TmTail tm_list ->
      let t = typeof ctx tm_list in
      (
        match t with
          (*TyList (h::t) -> TyList t*)
          TyList [typ] -> t
          | TyList [] -> raise (Type_error "Couldn't apply tail to empty list")
          | _ -> raise (Type_error "A list was expected")
      )

    (* T-IsEmpty *)
  | AtTerm (TmIsEmpty li) ->
      AtTy (TyBool)

    (* T-Cons *)
  | TmCons (elem, li) ->
      let elemty = typeof ctx elem in
      let litype = typeof ctx li in
      (
        match litype with
          TyList [aux] ->
            if elemty = aux then litype else raise (Type_error ("Couldn't concat a " ^ string_of_ty elemty ^ " element to a " ^ string_of_ty aux ^ " list"))
          | TyList [] -> TyList [elemty]
          | _ -> raise (Type_error ("Couldn't apply Cons operation to something that is not a List: " ^ string_of_ty litype))
      )

    (* T-TyRecord *)
  | TmTyRecord(name, fields) ->
      TyRecord(name, fields)

    (* T-Record *)
  | TmRecord(name, fields) ->
      (
        try
          match ty_getbinding ctx name with
            TyRecord (_, ty_fields) ->
              (* Verify both fields *)
              if check_types ctx fields ty_fields true then
                TyRecord (name, ty_fields)
              else
                raise (Type_error "Type mismatch")
            | _ -> raise Not_found
        with
          Type_error e -> raise (Type_error e)
        | _ -> raise (Type_error ("Record type " ^ name ^ " not found"))
      )

    (* T-ProjLabel *)
  | TmProjLabel (record, field) ->
      let tyr = typeof ctx record in
      (
        try
          match tyr with
            TyRecord (_, ty_fields) -> ty_getbinding ty_fields field
            | AtTy (TyVar v) -> 
              (try
                let tyr2 = (function typ -> 
                  match typ with
                    AtTy (TyVar v) -> (try ty_getbinding ctx v with Not_found -> raise (Type_error (v^" type not found")))
                    | _ -> typ) tyr in
                match tyr2 with
                  TyRecord (_, ty_fields2) -> ty_getbinding ty_fields2 field
                  | _ -> raise (Type_error ("Not valid record: "^string_of_ty tyr2))
              with
                Not_found -> raise (Type_error ("Not a valid label for record type "^string_of_ty (AtTy (TyVar v)))))
            | _ -> raise (Type_error ("Not valid record: "^string_of_ty tyr))
        with
         Not_found -> raise (Type_error ("Not a valid label for record type "^string_of_ty tyr))
      )

    (* T-TyVariant *)
  | TmTyVariant(name, fields) ->
      TyVariant(name, fields)

    (* T-Variant *)
  | TmVariant(name, field) ->
      (
        try
          match ty_getbinding ctx name with
            TyVariant (_, ty_fields) ->
              (* Verify both fields match *)
              if check_types ctx [field] ty_fields false then
                TyVariant (name, ty_fields)
              else
                raise (Type_error "Type mismatch")
            | _ -> raise Not_found
        with
          Type_error e -> raise (Type_error e)
        | _ -> raise (Type_error ("Variant type " ^ name ^ " not found"))
      )

    (* T-Case *)
  | TmCase (t, li) ->
    (
      let variant = typeof ctx t in
      match variant with
        TyVariant(_, ty_fields) -> 
          if List.for_all (function (name, value, t) -> 
            try let v = List.assoc name ty_fields in
              let ctx' = ty_addbinding ctx (value, v) in
              typeof ctx' t = variant
            with _ -> false) li then
              variant
          else
            raise (Type_error ("Invalid field name or field type for variant " ^ string_of_ty variant)) (*The for_all can return false for both reasons*)
        | _ -> raise (Type_error ("Couldn't apply \"case of\" to something (" ^ string_of_ty variant ^ ") that is not a Variant Type"))
    )

    (* T-Alias *)
  | TmTyAlias (str, typ) ->
    typ
(* The function check_types compares the two lists *)
and check_types (ctx: ty_context) (terms: term_context) (types: ty_context) (all: bool): bool =
  (*
    This function is used to check that all the terms existing in a record are correct with respect to
    the specific record type and that all the fields of the record type are used in the record to be created

    Parameters:
    ctx : ty_context
    terms : term_context
    types : ty_context
    all : bool (This parameter is used for variants, where not all fields of the particular variant type need to exist)

    Returns : bool
  *)
  let rec aux t_list =
    match t_list with
    | [] -> true  (* All terms have been checked *)
    | (name, term)::rest ->
        if not (field_exists name types) then
          false  (* The field does not exist in the types list *)
        else
          let expected_type = 
            (function typ -> 
              match typ with
                AtTy (TyVar v) -> (try ty_getbinding ctx v with Not_found -> raise (Type_error (v^" type not found")))
                | _ -> typ) (get_type name types) in
          let term_type = typeof ctx term in
          if term_type <> expected_type then
            false  (* The types do not match *)
          else
            aux rest  (* Check the rest of the terms *)
  in if all then
      aux terms && all_fields_exist terms types  (* Verify both criteria *)
    else
      aux terms
;;

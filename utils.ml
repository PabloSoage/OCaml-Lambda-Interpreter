open Parsing;;
open Lexing;;

open Parser;;
open Lexer;;

open Types;;
open Exceptions;;

(* CONTEXT MANAGEMENT *)
let term_emptyctx =
  []
;;
let term_addbinding ctx (x, bind) =
  (*
    This function is used to add an element to the terms context, given a label.

    Parameters:
    ctx : term_context
    x : string
    bind : term

    Returns: term_context
  *)
  (x, bind) :: ctx
;;
let term_getbinding ctx x =
  (*
    This function is used to retrieve an element of the terms context, given a label.

    Parameters:
    ctx : term_context
    x : string

    Returns: term
  *)
  List.assoc x ctx
;;

let ty_getbinding ctx x =
  (*
    This function is used to retrieve an element of the types context, given a label.

    Parameters:
    ctx : ty_context
    x : string

    Returns: ty
  *)
  List.assoc x ctx
;;
let ty_addbinding ctx (x, bind) =
  (*
    This function is used to add an element to the types context, given a label.

    Parameters:
    ctx : ty_context
    x : string
    bind : ty

    Returns: ty_context
  *)
  (x, bind) :: ctx
;;
let ty_emptyctx =
  []
;;

(* Utilities *)
(*
  fix_t is the second part of this TmAbs,
    (fun f -> (fun x -> f (fun y -> x x y)) (fun x -> f (fun y -> x x y)))
  used for the internal fixed point operator in recursive functions
*)
let fix_t =
    TmApp (TmAbs ("x", TyFix,
      TmApp (AtTerm (TmVar "f"), 
        TmAbs ("y",  TyFix,
          TmApp (TmApp (AtTerm (TmVar "x"), AtTerm (TmVar "x")), AtTerm (TmVar "y"))))),
    TmAbs ("x",  TyFix,
      TmApp (AtTerm (TmVar "f"), 
          TmAbs ("y",  TyFix,
            TmApp (TmApp (AtTerm (TmVar "x"), AtTerm (TmVar "x")), AtTerm (TmVar "y"))))))
;;
let no_last_comma str =
  (*
    This function is mainly used in functions like string_of_term and string_of_ty,
    for terms/list types, records, tuples and variants
    Its purpose is to remove the last comma/semicolon from an enumeration

    Parameters:
    str : string

    Returns : string
  *)
  let length = String.length str in
  if length >= 2 && (List.mem (String.sub str (length - 2) 1) [","; ";"]) then
    String.sub str 0 (length - 2) ^ String.sub str (length - 1) 1
  else
    str
;;
let field_exists name types =
  (*
    Function that searches for a type field in a list of pairs
    
    Parameters:
    name : string
    types : ty_context

    Returns : bool
  *)
  List.exists (fun (n, _) -> n = name) types
;;
let get_type name types =
  (*
    Function that gets the type of a field, assuming it always exists

    Parameters:
    name : string
    types : ty_context

    Returns : ty
  *)
  List.assoc name types
;;
let all_fields_exist terms types =
  (*
    Function that verifies that all fields in types are in terms
    
    Parameters:
    terms : term_context
    types : ty_context

    Returns : bool
  *)
  List.for_all (fun (n, _) -> field_exists n terms) types
;;
let print_ty_paren str = function
  (*
    This function will add "( )" to the string representing the given ty, if it is not atomic

    Parameters:
    str : string
    _ : ty

    Returns : string
  *)
  AtTy t1 -> str
  | _ -> "(" ^ str ^ ")"
;;
let tuple_nth n = function
  (*
    This function will return the nth term of a given tuple, as long as it does not exceed the length of the tuple

    Parameters:
    n : int
    _ : term

    Returns : term
  *)
  TmTuple elems ->
           if n < List.length elems then List.nth elems n
           else raise (Term_error "Projection index out of bounds")
  | _ -> raise (Term_error "Expected a tuple for projection")
;;

let rec subst_in_abs ctx tm =
  (*
    (This function is used in the main, and is called when a term is to be saved)
    Its objective is to recursively traverse the term, so that if it finds a TmVar, it looks
    for it in the context and "overwrites" it in the original expression with the recovered value
    The main objective is that the expressions are functional, and that if the value of a TmVar changes
    (is redefined) in the context, the behavior of the functions/expressions that could use said TmVar is not modified

    Parameters:
    ctx : term_context
    tm : term

    Returns : term
  *)
  match tm with
  | AtTerm (TmVar x) ->
      (try term_getbinding ctx x with Not_found -> tm)
  | TmAbs (x, ty, t_body) ->
      let ctx_without_x = List.filter (fun (key, _) -> key <> x) ctx in  (* Remove local variable from context *)
      TmAbs (x, ty, subst_in_abs ctx_without_x t_body)
  | TmApp (t1, t2) ->
      TmApp (subst_in_abs ctx t1, subst_in_abs ctx t2)
  | TmIf (t1, t2, t3) ->
      TmIf (subst_in_abs ctx t1, subst_in_abs ctx t2, subst_in_abs ctx t3)
  | TmLetIn (x, t1, t2) ->
      let t1' = subst_in_abs ctx t1 in
      let ctx_without_x = List.filter (fun (key, _) -> key <> x) ctx in
      TmLetIn (x, t1', subst_in_abs ctx_without_x t2)
  | TmTuple ts ->
      TmTuple (List.map (subst_in_abs ctx) ts)
  | TmRecord (name, fields) ->
      TmRecord (name, List.map (fun (label, t) -> (label, subst_in_abs ctx t)) fields)
  | TmVariant (name, field) ->
      TmVariant (name, (fun (label, t) -> (label, subst_in_abs ctx t)) field)
  | TmProj (t, i) ->
      TmProj (subst_in_abs ctx t, i)
  | TmList ts ->
      TmList (List.map (subst_in_abs ctx) ts)
  | TmProjLabel (t, label) ->
      TmProjLabel (subst_in_abs ctx t, label)
  | TmFix t ->
      TmFix (subst_in_abs ctx t)
  | TmLetRec (f, ty, t1, t2) ->
      let ctx_without_f = List.filter (fun (key, _) -> key <> f) ctx in
      TmLetRec (f, ty, subst_in_abs ctx t1, subst_in_abs ctx_without_f t2)
  | _ -> tm
;;

let term_to_atomic = function
  (*
    Given a term, if it is atomic (packed into a term), returns the atomic term itself

    Parameters:
    _ : term

    Returns: atomicTerm
  *)
  AtTerm x -> x
  | _ -> raise NotAnAtomicTerm
;;
let print_term_paren str = function
  (*
    This function will add "( )" to the string representing the given term, if it is not atomic

    Parameters:
    str : string
    _ : term

    Returns : string
  *)
  AtTerm t1 -> str
  | _ -> "(" ^ str ^ ")"
;;
let quit_quotes str =
  (*
    Used in string_of_term on string concatenation, so that the quotes that delimit the string are removed

    Parameters:
    str : string

    Returns : string
  *)
  let len = String.length str in
  if len >= 2 && String.sub str 0 1 = "\"" && String.sub str (len - 1) 1 = "\"" then
    String.sub str 1 (len - 2)
  else
    str
;;

let rec ldif l1 l2 = 
  (*
    Returns the difference between two lists (the elements in the first list
    that are not present in the second list)

    Parameters:
    l1 : 'a list
    l2 : 'a list

    Returns : 'a list
  *)
  match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = 
  (*
    Returns the union of two lists (a list containing all the unique elements
    from both input lists, preserving the order of elements in the first list)

    Parameters:
    l1 : 'a list
    l2 : 'a list

    Returns : 'a list
  *)
  match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec fresh_name x l =
  (*
    Generates a fresh name by appending a "'" to the given name until a unique 
    name is found that does not already exist in the provided list.

    Parameters:
    x : string
    l : string list

    Returns : string
  *)
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec isnumericval tm = 
  (*
    Checks if a given term is a number

    Parameters:
    tm : term

    Returns : bool
  *)
  match tm with
    (AtTerm TmZero) -> true
  | AtTerm (TmSucc t) -> isnumericval t
  | _ -> false
;;

let rec isval tm =
  (*
    Checks if a given term is a value or not
    This is mainly used for the evaluation of TmAbs and TmApp

    Parameters:
    tm : term

    Returns : bool
  *)
  match tm with
    (AtTerm TmTrue)  -> true
  | (AtTerm TmFalse) -> true
  | AtTerm (TmString _) -> true
  | TmAbs _ -> true
  | TmTuple _ -> true
  | TmList _ -> true
  | TmVariant (_, _) -> true
  | TmRecord (_, _) -> true
  | t when isnumericval t -> true
  | _ -> false
;;

let read_multiline () =
  (*
    Function responsible for enabling the user to write multiline expressions.
    Continuously reads line after line until the EOF token is found (defined as ";;" in the lexer)
    Anything after this token on the same line will be ignored, as stated in the user manual

    Returns : string
  *)
  let rec loop buf =
    let line = read_line () in
    let auxbuf = buf^line in
    let lexbuf = from_string line in
    let rec process_tokens lexbuf =
      if lexbuf.lex_buffer_len = lexbuf.lex_curr_pos then
        false  (* If the end of the lexbuf has been reached, stop *)
      else
        match token lexbuf with
        | EOF -> true  (* If we find EOF, stop reading *)
        | _ -> process_tokens lexbuf  (* If not EOF, continue reading tokens *)
    in
    if process_tokens lexbuf then auxbuf  (* If EOF is found, we return *)
    else loop auxbuf  (*If no EOF was found, we continue reading more lines*)
  in
  loop ""
;;
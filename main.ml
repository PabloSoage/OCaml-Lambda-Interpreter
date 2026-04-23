open Parsing;;
open Lexing;;

open Parser;;
open Lexer;;

open Types;;
open Exceptions;;
open Utils;;
open Termops;;
open Typeops;;

let rec unsolved_letrec ctx tm =
  (*
    The purpose of this function is to recursively traverse the given term to check if it contains
    at any point an unresolved "letrec" expression in order to hide the internal implementation of
    fix (the internal fixed-point operator)

    Parameters:
      ctx : term_context
      tm : term

    Returns : bool
  *)
  match tm with
  | TmLetRec (_, _, _, _) -> 
      (
        match (eval ctx tm) with
          AtTerm _ -> false
        | _ -> true
      )
  | TmAssign (_, t) -> unsolved_letrec ctx t
  | TmLetIn (_, t1, t2) -> unsolved_letrec ctx t1 || unsolved_letrec ctx (eval ctx tm)
  | TmIf (t1, t2, t3) -> unsolved_letrec ctx t1 || unsolved_letrec ctx t2 || unsolved_letrec ctx t3
  (*| TmAbs (_, TyFix, _) -> true*)
  | TmAbs (_, _, t) -> (try unsolved_letrec ctx t with _ -> raise Pack)
  | TmApp (t1, t2) -> let ev = eval ctx tm in
        if ev <> tm then (*in order to avoid hypothetical infinite loop*)
          unsolved_letrec ctx (eval ctx tm)
        else
          false
  | AtTerm (TmVar id) -> (try unsolved_letrec ctx (term_getbinding ctx id) with _ -> raise Pack)
  | AtTerm (TmIsZero t) | AtTerm (TmIsEmpty t) | AtTerm (TmSucc t) | AtTerm (TmPred t) -> unsolved_letrec ctx t
  | _ -> false
;;

let safe_unpack ctx tm etm tyTm =
  (*
    This function relies on unsolved_letrec to check whether a term contains an
    unsolved letrec or not, but also checks projection functions on lists, tuples
    and records. In addition, it is responsible for forming the string that will be
    displayed to the user on the screen (where the fix type will not be displayed)

    Parameters:
    ctx : term_context
    tm : term
    etm : term (evaluated one)
    tyTm : ty (type of the term)

    Returns : string
  *)
  try
    (*let unsolved = unsolved_letrec ctx tm in*)
    let unsolved = unsolved_letrec ctx etm in
      if not unsolved then
        let one_time_eval = (eval ~one_time:true ctx tm) in
        let unsolved_proj =
          try
            match tm with
              TmProjLabel (record, label) -> unsolved_letrec ctx one_time_eval
              | TmHead li -> unsolved_letrec ctx one_time_eval
              | TmProj (t, n) -> unsolved_letrec ctx one_time_eval
              | _ -> false
          with
            Pack -> true
        in let aux = 
          if not unsolved_proj then
            etm
          else
            one_time_eval
        in string_of_term aux ^ " : " ^ string_of_ty tyTm
      else
        string_of_term tm
  with
    Pack -> string_of_term tm ^ " : " ^ string_of_ty tyTm
;;

let top_level_loop () =
  (*
    Main loop of the program.
    It is responsible for reading the user input, performing type checking, evaluating
    the term and displaying the results on the screen. Additionally, it keeps track of
    the type and term context and manages possible exceptions
  *)
  print_endline "Evaluator of lambda expressions...";
  let rec loop term_ctx ty_ctx =
    print_string ">> ";
    flush stdout;
    try
      let input = read_multiline () in
      let tm = s token (from_string input) in
      (*if a type is incorrect or does not match a given expression, "typeof" will return an error,
        preventing the expression from being evaluated further*)
      let tyTm = typeof ty_ctx tm in
      let str, ctx_aux =
      (
        let etm = eval term_ctx tm in
        let out_str = safe_unpack term_ctx tm etm tyTm
        in match tm with
            TmAssign(id, TmTyRecord (_, _)) ->
              string_of_ty tyTm, [((id, subst_in_abs term_ctx etm),(id, tyTm))]
          | TmAssign (id, t) ->
            out_str, [((id, subst_in_abs term_ctx etm),(id, tyTm))]
          | _ -> out_str, []
      )
      in print_endline(str);
      match ctx_aux with
      h::t -> loop (term_addbinding term_ctx (fst h)) (ty_addbinding ty_ctx (snd h))
      | [] -> loop term_ctx ty_ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop term_ctx ty_ctx
     | Parse_error ->
         print_endline "syntax error";
         loop term_ctx ty_ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop term_ctx ty_ctx
     | Term_error e ->
         print_endline(e);
         loop term_ctx ty_ctx
     | End_of_file ->
         print_endline "...bye!!!"
     | Stack_overflow ->
         print_endline "Stack overflow";
         loop term_ctx ty_ctx
     | ex ->
        Printexc.print_backtrace stdout;
        print_endline("Exception: " ^ Printexc.to_string ex);
        loop term_ctx ty_ctx
  in
    loop term_emptyctx ty_emptyctx
  ;;

top_level_loop ()
;;


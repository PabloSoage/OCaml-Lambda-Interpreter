open Types;;
open Exceptions;;
open Utils;;
open Typeops;;

(* TERMS MANAGEMENT (EVALUATION) *)
let rec string_of_term = function
  (*
    This function is responsible for generating a string from a given term.
    This string will be representative of the term.
    It will also try to solve basic operations not handled
    in the evaluation function, such as succ, pred and so on

    Parameters
    _ : term

    Returns : string
  *)
    (AtTerm TmTrue) ->
      "true"
  | (AtTerm TmFalse) ->
      "false"
  | AtTerm (TmString s) ->
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
    ( 
      match t1,t2 with
      AtTerm (TmString s1), AtTerm (TmString s2) -> "\"" ^ s1^s2 ^ "\""
      | AtTerm (TmString s1), _ -> "\"" ^ s1 ^ quit_quotes (string_of_term t2) ^ "\""
      | _, AtTerm (TmString s2) -> "\"" ^ quit_quotes (string_of_term t1) ^ s2 ^ "\""
      | _ -> "(concat " ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
    )
  | TmIf (t1,t2,t3) ->
    (
      let t1str, t2str, t3str =
        string_of_term t1, string_of_term t2, string_of_term t3 in
      match t1 with
        AtTerm (TmTrue) -> print_term_paren t2str t2
      | AtTerm (TmFalse) -> print_term_paren t3str t3
      | _ -> "if " ^ (print_term_paren t1str t1) ^
             " then\n\t" ^ (print_term_paren t2str t2) ^
             "\nelse\n\t" ^ (print_term_paren t3str t3)
    )
  | (AtTerm TmZero) ->
      "0"
  | AtTerm (TmSucc t) ->
    (
      try
        let rec f n t' = match t' with
            TmZero -> string_of_int n
          | TmSucc s -> f (n+1) (term_to_atomic s)
          | _ -> let tstr = string_of_term t in
              "(succ " ^ tstr ^ ")"
        in f 1 (term_to_atomic t)
      with
        NotAnAtomicTerm -> let tstr = string_of_term t in
              "(succ " ^ tstr ^ ")"
    )
  | AtTerm (TmPred t) ->
    (
      try
        match t with
        AtTerm (TmZero) -> "0"
        | _ -> 
         let rec f n t' = match t' with
              TmZero -> string_of_int n
            | TmSucc s -> f (n+1) (term_to_atomic s)
            | _ -> let tstr = string_of_term t in
                "(pred " ^ tstr ^ ")"
          in f (-1) (term_to_atomic t)
      with
        NotAnAtomicTerm -> let tstr = string_of_term t in
                "(pred " ^ tstr ^ ")"
    )
  | AtTerm (TmIsZero t) ->
    (
      try
        match term_to_atomic t with
        TmZero -> "true"
        | TmSucc _ -> "false"
        | _ -> let tstr = string_of_term t in
                "(iszero " ^ tstr ^ ")"
      with
        NotAnAtomicTerm -> let tstr = string_of_term t in
                "(iszero " ^ tstr ^ ")"
    )
  | AtTerm (TmVar s) | AtTerm (TmTyVar s) ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) -> (*can give you an atomic type or other function*)
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t -> 
      let tstr = string_of_term t in
      "(fix " ^ tstr ^ ")"
  | TmLetRec (f, ty, t1, t2) ->
      "letrec " ^ f ^ ":" ^ string_of_ty ty ^ " = \n\t" ^ string_of_term t1 ^
          "\nin " ^ string_of_term t2
  | TmAssign (id, t) ->
      string_of_term t
  | TmTuple tm_list ->
      no_last_comma ("(" ^ (List.fold_left (fun acc x -> acc ^ (string_of_term x) ^ ",") "" tm_list) ^ ")")
  | TmProj (tuple, nth) ->
      string_of_term (tuple_nth nth tuple)
  | TmList tm_list ->
      no_last_comma ("[" ^ (List.fold_left (fun acc x -> acc ^ (string_of_term x) ^ ",") "" tm_list) ^ "]")
  | TmHead li ->
      "hd(" ^ string_of_term li ^ ")"
  | TmTail li ->
      "tl(" ^ string_of_term li ^ ")"
  | AtTerm (TmIsEmpty li) ->
      "isempty(" ^ string_of_term li ^ ")"
  | TmCons (elem, li) ->
      "("^string_of_term elem^"::"^string_of_term li^")"
  | TmTyRecord (name, fields) ->
      no_last_comma (name ^ " = {" ^ (List.fold_left (fun acc (x,y) -> acc ^ x ^ ":" ^ (string_of_ty y) ^ ";") "" fields) ^ "}")
  | TmRecord (name, fields) ->
      no_last_comma (name ^ " {" ^ (List.fold_left (fun acc (x,y) -> acc ^ x ^ "=" ^ (string_of_term y) ^ ";") "" fields) ^ "}")
  | TmProjLabel (record, field) ->
      string_of_term record ^ "." ^ field
  | TmTyVariant (name, fields) ->
      no_last_comma (name ^ " = <" ^ (List.fold_left (fun acc (x,y) -> acc ^ x ^ ":" ^ (string_of_ty y) ^ ";") "" fields) ^ ">")
  | TmVariant (name, (x, y)) ->
      "<" ^ x ^ "=" ^ string_of_term y ^ "> as " ^ name
  | TmCase (t, li) ->
      let (lab, val_id, t2) = List.hd(li) in
      "case " ^ string_of_term t ^ " of\n" ^
      "\t<"^lab^"="^val_id^"> -> "^ string_of_term t2 ^"\n"^
      (
        let rec aux acc = function
          (label, id, res)::itm2::tl -> aux ("\t| <"^label^"="^id^"> -> "^ string_of_term res ^"\n"^ acc) (itm2::tl)
          | (label, id, res)::tl -> "\t| <"^label^"="^id^"> -> "^ string_of_term res ^"\n"^ acc
          | [] -> ""
        in aux "" (List.tl li)
      )
  | TmTyAlias (str, typ) ->
      str
;;

let rec free_vars tm = 
  (*
    This function returns a list with the free variables of the given term / expression

    Parameters:
    tm : term

    Returns : string list
  *)
  match tm with
    (AtTerm TmTrue) ->
      []
  | (AtTerm TmFalse) ->
      []
  | AtTerm (TmString _) ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | (AtTerm TmZero) ->
      []
  | AtTerm (TmSucc t) ->
      free_vars t
  | AtTerm (TmPred t) ->
      free_vars t
  | AtTerm (TmIsZero t) ->
      free_vars t
  | AtTerm (TmVar s) | AtTerm (TmTyVar s) ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmLetRec (f, _, t1, t2) ->
      lunion (ldif (free_vars t2) [f]) (free_vars t1)
  | TmAssign (id, t) ->
      free_vars t
  | TmTuple tm_list ->
      List.fold_left (fun acc x -> lunion (free_vars x) acc) [] tm_list
  | TmProj (tuple, nth) ->
      free_vars (tuple_nth nth tuple)
  | TmList tm_list ->
      List.fold_left (fun acc x -> lunion (free_vars x) acc) [] tm_list
  | TmHead li ->
      free_vars li
  | TmTail li ->
      free_vars li
  | TmCons (elem, li) -> lunion (free_vars elem) (free_vars li)
  | AtTerm (TmIsEmpty li) ->
      free_vars li
  | TmTyRecord (name, fields) ->
      ldif (List.fold_left (fun acc (x,_) -> lunion (free_vars (AtTerm (TmVar x))) acc) [] fields) [name]
  | TmRecord (name, fields) ->
      ldif (List.fold_left (fun acc (x,_) -> lunion (free_vars (AtTerm (TmVar x))) acc) [] fields) [name]
  | TmProjLabel (record, label) ->
      (
        match record with
          TmRecord(name, fields) ->
            ldif (List.fold_left (fun acc (x,_) -> lunion (free_vars (AtTerm (TmVar x))) acc) [] fields) [name]
          | AtTerm (TmVar v) -> ldif [label] [v]
          | _ -> raise (Type_error ("Couldn't do projection of " ^ string_of_term record))
      )
  | TmTyVariant (name, fields) ->
      ldif (List.fold_left (fun acc (x,_) -> lunion (free_vars (AtTerm (TmVar x))) acc) [] fields) [name]
  | TmVariant (name, (x, _)) ->
      ldif (free_vars (AtTerm (TmVar x))) [name]
  | TmCase (t, li) ->
      ldif (List.fold_left (fun acc (label, id, t2) -> lunion (ldif (free_vars t2) [label;id]) acc) [] li) (free_vars t)
  | TmTyAlias (str, typ) -> [str]
;;

let rec subst x s tm = 
  (*
    This function replaces "x" with "s" in the given term

    Parameter:
    x : string
    s : term
    tm : term

    Returns : term
  *)
  match tm with
    (AtTerm TmTrue) ->
      (AtTerm TmTrue)
  | (AtTerm TmFalse) ->
      (AtTerm TmFalse)
  | AtTerm (TmString s) ->
      AtTerm (TmString s)
  | TmConcat (t1, t2) ->
      TmConcat(subst x s t1, subst x s t2)
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | (AtTerm TmZero) ->
      (AtTerm TmZero)
  | AtTerm (TmSucc t) ->
      AtTerm (TmSucc (subst x s t))
  | AtTerm (TmPred t) ->
      AtTerm (TmPred (subst x s t))
  | AtTerm (TmIsZero t) ->
      AtTerm (TmIsZero (subst x s t))
  | AtTerm (TmVar y) | AtTerm (TmTyVar y) ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (AtTerm (TmVar z)) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (AtTerm (TmVar z)) t2))
  | TmFix t -> subst x s t
  | TmLetRec (f, ty, t1, t2) ->
      if f=x then TmLetRec (f, ty, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem f fvs)
           then TmLetRec (f, ty, subst x s t1, subst x s t2)
           else let z = fresh_name f (free_vars t2 @ fvs) in
                TmLetRec (z, ty, subst x s t1, subst x s (subst f (AtTerm (TmVar z)) t2))
  | TmAssign (id, t) ->
      TmAssign (id, subst x s t)
  | TmTuple tm_list -> 
      TmTuple (List.map (function t -> subst x s t) tm_list)
  | TmProj (tuple, nth) ->
      subst x s (tuple_nth nth tuple)
  | TmList tm_list -> 
      TmList (List.map (function t -> subst x s t) tm_list)
  | TmHead li ->
      TmHead (subst x s li)
  | TmTail li ->
      TmTail (subst x s li)
  | TmCons (elem, li) ->
      TmCons (subst x s elem, subst x s li)
  | AtTerm (TmIsEmpty li) ->
      AtTerm (TmIsEmpty (subst x s li))
  | TmTyRecord (name, fields) ->
      TmTyRecord (name, List.map (function (AtTerm (TmVar v),t) -> (v,t) | _ -> raise (Term_error "Not a TmVar"))
        (List.map (function (r,t) -> (subst x s (AtTerm (TmVar r)),t)) fields))
  | TmRecord (name, fields) ->
      TmRecord (name, List.map (function (AtTerm (TmVar v),t) -> (v,t) | _ -> raise (Term_error "Not a TmVar"))
        (List.map (function (r,t) -> (subst x s (AtTerm (TmVar r)),t)) fields))
  | TmProjLabel (record, label) ->
      TmProjLabel (subst x s record, ((function AtTerm (TmVar v) -> v | _ -> raise (Term_error "Not a TmVar")) 
        (subst x s (AtTerm (TmVar label)))))
  | TmTyVariant (name, fields) ->
      TmTyVariant (name, List.map (function (AtTerm (TmVar v),t) -> (v,t) | _ -> raise (Term_error "Not a TmVar"))
        (List.map (function (r,t) -> (subst x s (AtTerm (TmVar r)),t)) fields))
  | TmVariant (name, (r, t)) ->
      TmVariant (name, (function (AtTerm (TmVar v),t) -> (v,t) | _ -> raise (Term_error "Not a TmVar"))
        (subst x s (AtTerm (TmVar r)), subst x s t))
  | TmCase (t, li) ->
      TmCase(subst x s t, List.map (function (AtTerm (TmVar label), v, t2) -> label, v ,t2 | _ -> raise (Term_error "Not a TmVar"))
        (List.map (function label, v, t2 -> subst x s (AtTerm (TmVar label)), v, subst x s t2) li))
  | TmTyAlias (str, typ) -> if str = x then TmTyAlias ((function AtTerm (TmVar v) -> v | _ -> raise (Term_error "Not a TmVar"))s, typ) else tm
;;

let rec eval1 ctx tm =
  (*
    This function is the main evaluator of the program.
    It is responsible for, given a term (and the context of terms), evaluating it
    and giving an error if necessary (although the latter is not usually necessary
    due to typeof type validation)

    Parameters:
    ctx : term_context
    tm : term

    Returns : term
  *)
  match tm with
    (* E-IfTrue *)
    TmIf ((AtTerm TmTrue), t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf ((AtTerm TmFalse), _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | AtTerm (TmSucc t1) ->
      let t1' = eval1 ctx t1 in
      AtTerm (TmSucc t1')

    (* E-PredZero *)
  | AtTerm (TmPred (AtTerm TmZero)) ->
      (AtTerm TmZero)

    (* E-PredSucc *)
  | AtTerm (TmPred (AtTerm (TmSucc nv1))) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | AtTerm (TmPred t1) ->
      let t1' = eval1 ctx t1 in
      AtTerm (TmPred t1')

    (* E-IszeroZero *)
  | AtTerm (TmIsZero (AtTerm TmZero)) ->
      (AtTerm TmTrue)

    (* E-IszeroSucc *)
  | AtTerm (TmIsZero (AtTerm (TmSucc nv1))) when isnumericval nv1 ->
      (AtTerm TmFalse)

    (* E-Iszero *)
  | AtTerm (TmIsZero t1) ->
      let t1' = eval1 ctx t1 in
      AtTerm (TmIsZero t1')

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)

    (* E-Fix *)
  | TmFix f ->
      subst "f" f fix_t

    (* E-LetRec *)
  | TmLetRec (f, ty, t1, t2) ->
      let t1' = eval1 ctx (TmFix (TmAbs (f, ty, t1))) in
      subst f t1' t2

    (* E-Concat *)
  | TmConcat(AtTerm (TmString s1), AtTerm (TmString s2)) -> 
      AtTerm (TmString (s1^s2))

    (* E-Assign*)
  | TmAssign (id, t) ->
      t

    (* E-Var *)
  | AtTerm (TmVar x) ->
      (try term_getbinding ctx x with
       _ -> raise (Term_error ("no binding term for variable " ^ x)))

      (* E-List *)
  | TmTuple li ->
      let li2 = (List.map (function h -> 
          try(
            match h with
            TmLetRec (_, _, _, _) -> raise NoRuleApplies
            | _ -> 
              let elem =
                let rec aux e =
                  try
                    let e2 = eval1 ctx e in
                    if e = e2 then e
                    else aux e2
                  with
                    NoRuleApplies -> e
                in aux (eval1 ctx h)
              in match elem with
                  AtTerm _ -> elem
                  | _ -> h
          )with
            NoRuleApplies -> h
      ) li) in
      if li = li2 then
        raise NoRuleApplies
      else
        TmList li2

    (* E-Proj *)
  | TmProj (tuple, nth) ->
      (
        match tuple with
        TmTuple tm_list ->
          if nth < List.length tm_list then List.nth tm_list nth
          else raise (Term_error "Projection index out of bounds")
        | _ -> 
          (
            try
              TmProj (eval1 ctx tuple, nth)
            with
              _ -> raise (Term_error "Expected a tuple for projection")
          )
      )

    (* E-List *)
  | TmList li ->
      let li2 = (List.map (function h -> 
          try(
            match h with
            TmLetRec (_, _, _, _) -> raise NoRuleApplies
            | _ -> 
              let elem =
                let rec aux e =
                  try
                    let e2 = eval1 ctx e in
                    if e = e2 then e
                    else aux e2
                  with
                    NoRuleApplies -> e
                in aux (eval1 ctx h)
              in match elem with
                  AtTerm _ -> elem
                  | _ -> h
          )with
            NoRuleApplies -> h
      ) li) in
      if li = li2 then
        raise NoRuleApplies
      else
        TmList li2

    (* E-ListHead *)
  | TmHead li ->
      (
        match li with
          TmList (h::t) -> h
          | _ -> 
            (
              try
                TmHead (eval1 ctx li)
              with
                _ -> raise (Term_error (string_of_term li ^ " is not a list"))
            )
      )

    (* E-ListTail *)
  | TmTail li ->
      (
        match li with
          TmList (h::t) -> TmList t
          | _ -> (
              try
                TmTail (eval1 ctx li)
              with
                _ -> raise (Term_error (string_of_term li ^ " is not a list"))
            )
      )

    (* E-IsEmpty *)
  | AtTerm (TmIsEmpty li) ->
      (
        match li with
          TmList (h::t) -> AtTerm (TmFalse)
          | TmList [] -> AtTerm (TmTrue)
          | _ -> 
            (
              try
                AtTerm (TmIsEmpty (eval1 ctx li))
              with
                _ -> raise (Term_error (string_of_term li ^ " is not a list"))
            )
      )

    (* E-Cons *)
  | TmCons (elem, li) ->
      (
        match li with
          TmList (h::t) -> TmList (elem::h::t)
          | TmList [] -> TmList [elem]
          | _ -> 
            (
              try
               TmCons (elem, eval1 ctx li)
              with
                _ -> raise (Term_error "Cons can't be applied to something that is not a list")
            )
      )

    (* E-Record *)
  | TmRecord (name, li) ->
      let li2 = (List.map (function (str, h) -> 
          try(
            match h with
            TmLetRec (_, _, _, _) -> raise NoRuleApplies
            | _ -> 
              let elem =
                let rec aux e =
                  try
                    let e2 = eval1 ctx e in
                    if e = e2 then e
                    else aux e2
                  with
                    NoRuleApplies -> e
                in aux (eval1 ctx h)
              in match elem with
                  AtTerm _ -> (str, elem)
                  | _ -> (str, h)
          )with
            NoRuleApplies -> (str, h)
      ) li) in
      if li = li2 then
        raise NoRuleApplies
      else
        TmRecord (name, li2)

    (* E-LabelProj *)
  | TmProjLabel (record, label) ->
    (
      match record with
        TmRecord(name, fields) ->
          (
            try
              term_getbinding fields label
            with
            | _ -> raise (Term_error ("Field " ^ label ^ " not found in record " ^ name))
          )
        | _ -> 
            (
              try
               TmProjLabel (eval1 ctx record, label)
              with
                _ -> raise (Term_error (string_of_term record ^ " is not a valid record"))
            )
    )

    (* E-Variant *)
  | TmVariant (name, t) ->
      let t2 = (function (str, h) -> 
          try(
            match h with
            TmLetRec (_, _, _, _) -> raise NoRuleApplies
            | _ -> 
              let elem =
                let rec aux e =
                  try
                    let e2 = eval1 ctx e in
                    if e = e2 then e
                    else aux e2
                  with
                    NoRuleApplies -> e
                in aux (eval1 ctx h)
              in match elem with
                  AtTerm _ -> (str, elem)
                  | _ -> (str, h)
          )with
            NoRuleApplies -> (str, h)
      ) t in
      if t = t2 then
        raise NoRuleApplies
      else
        TmVariant (name, t2)

    (* E-Case *)
  | TmCase (t, li) -> 
    (
      try
        (
          match t with
          TmVariant (_, (label, value)) ->
            let _, case_value, t2 = List.find (function (name, _, _) -> label=name) li in
            (try
              let ev_value = eval1 ctx value in subst case_value ev_value t2
            with
              NoRuleApplies -> subst case_value value t2)
          | _ -> 
            (
              try
               TmCase (eval1 ctx t, li)
              with
                _ -> raise NoRuleApplies
            )
        )
      with
        _ -> raise (Term_error (string_of_term t ^ " is not a valid variant"))
    )
  | _ ->
      raise NoRuleApplies
;;

let rec eval ?(one_time=false) ctx tm =
  (*
    This function acts as an interface to the "eval1" function,
    re-launching the previous one as necessary, in order to recursively
    resolve/evaluate the previous one

    Parameters:
    ?one_time : bool (This parameter is to evaluate the term only once and not recursively. (It is mainly used in the "safe_unpack" function of main.ml file))
    ctx : term_context
    tm : term

    Returns : term
  *)
  try
    let tm' = eval1 ctx tm in
    if not one_time then
      eval ctx tm'
    else
      tm'
  with
    NoRuleApplies -> tm
;;

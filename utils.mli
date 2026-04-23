open Types;;
val tuple_nth : int -> term -> term;;
val subst_in_abs : term_context -> term -> term;;

val fix_t : term;;
val no_last_comma : string -> string;;

val field_exists : string -> ty_context -> bool;;
val get_type : string -> ty_context -> ty;;
val all_fields_exist : term_context -> ty_context -> bool;;

val print_ty_paren : string -> ty -> string;;
val print_term_paren : string -> term -> string;;
val quit_quotes : string -> string;;

val term_to_atomic : term -> atomicTerm;;

val ldif : 'a list -> 'a list -> 'a list;;
val lunion : 'a list -> 'a list -> 'a list;;
val fresh_name : string -> string list -> string;;
val isnumericval : term -> bool;;
val isval : term -> bool;;

val term_emptyctx : term_context;;
val term_addbinding : term_context -> (string * term) -> term_context;;
val term_getbinding : term_context -> string -> term;;

val ty_emptyctx : ty_context;;
val ty_addbinding : ty_context -> (string * ty) -> ty_context;;
val ty_getbinding : ty_context -> string -> ty;;

val read_multiline : unit -> string;;
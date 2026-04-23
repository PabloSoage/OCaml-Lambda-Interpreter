open Types;;

val string_of_ty : ty -> string;;
val typeof : ty_context -> term -> ty;;
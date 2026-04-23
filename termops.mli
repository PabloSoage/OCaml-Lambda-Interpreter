open Types;;

val string_of_term : term -> string;;
val eval : ?one_time:bool -> term_context -> term -> term;;
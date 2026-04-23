(* TYPE DEFINITIONS *)
type atomicTy =
  | TyBool
  | TyNat
  | TyString
  | TyVar of string
;;

type ty =
    AtTy of atomicTy
  | TyArr of ty * ty
  | TyFix
  | TyTuple of ty list
  | TyList of ty list
  | TyRecord of string * (string * ty) list
  | TyVariant of string * (string * ty) list
  | TyTop
  | TyBot
;;

type atomicTerm =
    TmTrue
  | TmFalse
  | TmZero
  | TmVar of string
  | TmTyVar of string
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmString of string
  | TmIsEmpty of term
and term =
    AtTerm of atomicTerm
  | TmIf of term * term * term
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmLetRec of string * ty * term * term
  | TmFix of term
  | TmConcat of term * term
  | TmAssign of string * term
  | TmTuple of term list
  | TmProj of term * int
  | TmList of term list
  | TmHead of term
  | TmTail of term
  | TmCons of term * term
  | TmTyRecord of string * (string * ty) list         (* Declaration of a record type *)
  | TmRecord of string * (string * term) list         (* Instance of an existing record *)
  | TmProjLabel of term * string                      (* Projection of a field per label *)
  | TmTyVariant of string * (string * ty) list
  | TmVariant of string * (string * term)
  | TmCase of term * (string * string * term) list
  | TmTyAlias of string * ty
;;

type ty_context =
  (string * ty) list
;;

type term_context =
  (string * term) list
;;
type ty =
  | TVoid
  | TInt
  | TFloat
  | TBool
  | TEnum of string
  | TStruct of string
  | TPointer of ty
  | TAlias of string

type const = CInt of int | CBool of bool | CFloat of float

type expr =
  | EConst of const
  | EVar of string
  | EAddr of expr (* &x *)
  | EArrow of expr * expr (* x->y *)
  | EDot of expr * expr (* x.y *)
  | EUnOp of Ttree.unop * expr
  | EBinOp of Ttree.binop * expr * expr
  | ECall of string * expr list

type lhs = LVar of string | LArrow of lhs * lhs | LDot of lhs * lhs
type attr = Static | Extern | Const | Inline

type stmt =
  | SExpr of expr
  | SVarDecl of attr list * ty * string
  | SVarDef of attr list * ty * string * expr
  | SAssign of lhs * expr
  | SReturn of expr
  | SIf of expr * stmt list * stmt list
  | SSwitch of expr * case list

and case = Case of case_pattern * stmt list
and case_pattern = EnumCase of string

type preproc =
  | PMacroApp of string * expr list
  | PIncl of string
  | PIf of string
  | PIfNot of string
  | PDef of string * string option
  | PEndIf

type global =
  | GEnum of string * string list
  | GStruct of string * (string * ty) list
  | GFunc of string * (string * ty) list * ty * stmt list
  | GProto of string * ty list * ty
  | GVarDecl of attr list * ty * string
  | GVarDef of attr list * ty * string * expr
  | GPre of preproc

type t = global list

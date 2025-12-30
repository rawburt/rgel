type identifier = string [@@deriving show]

type parsed_type = { type_desc : type_desc; type_loc : Location.t }
[@@deriving show]

and type_desc = Type_name of identifier [@@deriving show]

type binary_operator =
  | Binop_add
  | Binop_sub
  | Binop_mul
  | Binop_div
  | Binop_eq
[@@deriving show]

type literal = Lit_bool of bool | Lit_int of int | Lit_string of string
[@@deriving show]

type parsed_expr = { expr_desc : expr_desc; expr_loc : Location.t }
[@@deriving show]

and expr_desc =
  | Expr_literal of literal
  | Expr_ident of identifier
  | Expr_call of call_expr
  | Expr_binary_op of {
      binop_left : parsed_expr;
      binop_operator : binary_operator;
      binop_right : parsed_expr;
    }
[@@deriving show]

and call_expr = { call_def : parsed_expr; call_args : parsed_expr list }
[@@deriving show]

type parsed_stmt = { stmt_desc : stmt_desc; stmt_loc : Location.t }
[@@deriving show]

and stmt_desc = Stmt_expr of parsed_expr [@@deriving show]

type parsed_block = { block_stmts : parsed_stmt list; block_loc : Location.t }
[@@deriving show]

type def_param = {
  param_name : identifier;
  param_type : parsed_type;
  param_loc : Location.t;
}
[@@deriving show]

type parsed_def = {
  def_name : identifier;
  def_params : def_param list;
  def_body : parsed_block;
  def_loc : Location.t;
}
[@@deriving show]

type parsed_extern = {
  extern_name : identifier;
  extern_type_params : identifier list;
  extern_params : parsed_type list;
  extern_return_type : parsed_type;
  extern_foreign_name : string;
  extern_loc : Location.t;
}
[@@deriving show]

type toplevel = Toplevel_def of parsed_def | Toplevel_extern of parsed_extern
[@@deriving show]

type parsed_module = {
  module_name : identifier;
  module_toplevels : toplevel list;
}
[@@deriving show]

type typed_expr = {
  texpr_desc : texpr_desc;
  texpr_type : Types.t;
  texpr_loc : Location.t;
}
[@@deriving show]

and texpr_desc =
  | TExpr_literal of Parsed_ast.literal
  | TExpr_variable of string
  | TExpr_call of typed_call_expr
  | TExpr_rec of { rec_fields : (string * typed_expr) list }
  | TExpr_binary_op of {
      binop_left : typed_expr;
      binop_operator : Parsed_ast.binary_operator;
      binop_right : typed_expr;
    }
  | TExpr_member_access of {
      member_object : typed_expr;
      member_name : string;
      is_method : bool;
    }
[@@deriving show]

and typed_call_expr = { call_def : typed_expr; call_args : typed_expr list }
[@@deriving show]

type typed_stmt = { tstmt_desc : tstmt_desc; tstmt_loc : Location.t }
[@@deriving show]

and tstmt_desc =
  | TStmt_expr of typed_expr
  | TStmt_var of {
      var_name : string;
      var_type : Types.t;
      var_value : typed_expr;
    }
  | TStmt_return of typed_expr
[@@deriving show]

type typed_block = typed_stmt list [@@deriving show]

type typed_def = {
  tdef_name : string;
  tdef_params : (string * Types.t) list;
  tdef_return_type : Types.t;
  tdef_body : typed_block;
  tdef_loc : Location.t;
}
[@@deriving show]

type typed_rec = {
  trec_name : string;
  trec_fields : (string * Types.t) list;
  trec_methods : typed_def list;
  trec_loc : Location.t;
}
[@@deriving show]

type typed_toplevel =
  | ToplevelT_def of typed_def
  | ToplevelT_extern of Parsed_ast.parsed_extern
  | ToplevelT_rec of typed_rec
[@@deriving show]

type typed_module = {
  tmodule_name : string;
  tmodule_toplevels : typed_toplevel list;
}
[@@deriving show]

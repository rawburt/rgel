%{
open Parsed_ast
%}

%token <string> IDENT
%token <string> TIDENT
%token <string> STRING_LITERAL
%token <int> INT_LITERAL
%token DOT LPAREN RPAREN COMMA COLON
%token LBRACKET RBRACKET
%token EQ PLUS MINUS STAR SLASH EQEQ
%token TRUE FALSE
%token DEF DO END EXTERN REC VAR RET
%token EOF

%nonassoc EQEQ
%left PLUS MINUS
%left STAR SLASH
%left DOT
%nonassoc LPAREN

%start <toplevel list> toplevels
%%

toplevels: list(toplevel) EOF { $1 }

toplevel:
| def { Toplevel_def $1 }
| extern { Toplevel_extern $1 }
| record { Toplevel_rec $1 }

parsed_type: type_desc { { type_desc = $1; type_loc = Location.make_loc $loc; } }
type_desc:
| IDENT { Type_name $1 }
| TIDENT { Type_name $1 }

type_params: option(type_params_desc) { Option.value $1 ~default:[] }
type_params_desc: LBRACKET params=separated_list(COMMA, TIDENT) RBRACKET { params }

extern:
| EXTERN DEF extern_name=IDENT extern_type_params=type_params LPAREN extern_params=separated_list(COMMA, parsed_type) RPAREN extern_return_type=parsed_type EQ extern_foreign_name=STRING_LITERAL
  {
    {
      extern_name;
      extern_type_params;
      extern_params;
      extern_return_type;
      extern_foreign_name;
      extern_loc = Location.make_loc $loc;
    }
  }

def:
| DEF ident=IDENT LPAREN params=separated_list(COMMA, def_param) RPAREN return_type=option(parsed_type) DO body=block END
  {
    {
      def_name = ident;
      def_params = params;
      def_return_type = Option.value return_type ~default:{ type_desc = Type_name "unit"; type_loc = Location.none };
      def_body = body;
      def_loc = Location.make_loc $loc;
    }
  }

def_param:
| name=IDENT COLON param_type=parsed_type
  {
    {
      param_name = name;
      param_type = param_type;
      param_loc = Location.make_loc $loc;
    }
  }

record: REC rec_name=TIDENT DO rec_fields=list(rec_field) END
  {
    {
      rec_name;
      rec_fields;
      rec_loc = Location.make_loc $loc;
    }
  }

rec_field:
| field_name=IDENT COLON field_type=parsed_type
  {
    {
      field_name;
      field_type;
      field_loc = Location.make_loc $loc;
    }
  }

block: list(statement)
  {
    {
      block_stmts = $1;
      block_loc = Location.make_loc $loc;
    }
  }

statement: statement_desc { { stmt_desc = $1; stmt_loc = Location.make_loc $loc; } }
statement_desc:
| expr { Stmt_expr $1 }
| VAR var_name=IDENT COLON var_type=parsed_type EQ var_value=expr
  {
    Stmt_var {
      var_name;
      var_type;
      var_value;
    }
  }
| RET ret_value=expr { Stmt_return ret_value }

expr: expr_desc { { expr_desc = $1; expr_loc = Location.make_loc $loc; } }
expr_desc:
| literal { Expr_literal $1 }
| IDENT { Expr_ident $1 }
| postfix_expr { $1 }
| rec_type_name=TIDENT LPAREN rec_fields=separated_list(COMMA, rec_field_expr) RPAREN
  { Expr_rec { rec_type_name; rec_fields; } }
| expr binary_operator expr
  {
    Expr_binary_op {
      binop_left = $1;
      binop_operator = $2;
      binop_right = $3;
    }
  }

postfix_expr:
| call_expr { Expr_call $1 }
| member_object=expr DOT member_name=IDENT
  {
    Expr_member_access {
      member_object;
      member_name;
    }
  }

call_expr:
| def_expr=expr LPAREN args=separated_list(COMMA, expr) RPAREN
  {
    {
      call_def = def_expr;
      call_args = args;
    }
  }

rec_field_expr:
| field_name=IDENT COLON field_value=expr
  {
    (field_name, field_value)
  }

%inline binary_operator:
| PLUS { Binop_add }
| MINUS { Binop_sub }
| STAR { Binop_mul }
| SLASH { Binop_div }
| EQEQ { Binop_eq }

literal:
| INT_LITERAL { Lit_int $1 }
| STRING_LITERAL { Lit_string $1 }
| TRUE { Lit_bool true }
| FALSE { Lit_bool false }

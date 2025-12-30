%{
open Parsed_ast
%}

%token <string> IDENT
%token <string> STRING_LITERAL
%token <int> INT_LITERAL
%token LPAREN RPAREN COMMA COLON
%token EQ
%token TRUE FALSE
%token DEF DO END EXTERN
%token EOF

%start <toplevel list> toplevels
%%

toplevels: list(toplevel) EOF { $1 }

toplevel:
| def { Toplevel_def $1 }
| extern { Toplevel_extern $1 }

parsed_type: type_desc { { type_desc = $1; type_loc = Location.make_loc $loc; } }
type_desc:
| IDENT { Type_name $1 }

extern:
| EXTERN DEF ident=IDENT LPAREN params=separated_list(COMMA, parsed_type) RPAREN return_type=parsed_type EQ foreign_name=STRING_LITERAL
  {
    {
      extern_name = ident;
      extern_params = params;
      extern_return_type = return_type;
      extern_foreign_name = foreign_name;
      extern_loc = Location.make_loc $loc;
    }
  }

def:
| DEF ident=IDENT LPAREN params=separated_list(COMMA, def_param) RPAREN DO body=block END
  {
    {
      def_name = ident;
      def_params = params;
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

expr: expr_desc { { expr_desc = $1; expr_loc = Location.make_loc $loc; } }
expr_desc:
| literal { Expr_literal $1 }
| IDENT { Expr_ident $1 }
| call_expr { Expr_call $1 }

call_expr:
| def_expr=expr LPAREN args=separated_list(COMMA, expr) RPAREN
  {
    {
      call_def = def_expr;
      call_args = args;
    }
  }

literal:
| INT_LITERAL { Lit_int $1 }
| STRING_LITERAL { Lit_string $1 }
| TRUE { Lit_bool true }
| FALSE { Lit_bool false }

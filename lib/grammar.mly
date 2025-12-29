%{
open Parsed_ast
%}

%token <string> IDENT
%token <string> STRING_LITERAL
%token <int> INT_LITERAL
%token LPAREN RPAREN COMMA
%token TRUE FALSE
%token DEF DO END
%token EOF

%start <parsed_def list> module_defs
%%

module_defs:
| list(def) EOF { $1 }

def:
| DEF ident=IDENT LPAREN RPAREN DO body=block END
  {
    {
      def_name = ident;
      def_params = [];
      def_body = body;
      def_loc = Location.make_loc $loc;
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

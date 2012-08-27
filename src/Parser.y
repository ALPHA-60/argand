{
module Parser where
import Lexer
import Core
import Control.Monad.State
}


%name argparser
%tokentype { Token }
%error { parseError }

%token
  box 	 { Token {tv = Box} }
  put 	 { Token {tv = Put} }
  construct 	 { Token {tv = Construct} }
  conn 	 { Token {tv = Conn } }
  using	 { Token {tv = Using } }
  to 	 { Token {tv = To } }
  at 	 { Token {tv = At } }
  draw 	 { Token {tv = Draw } }
  left 	 { Token {tv = LeftJ } }
  right	 { Token {tv = RightJ } }
  center { Token {tv = CenterJ } }
  string { Token {tv = String $$ } }
  '{'    { Token {tv = LBrace} }
  '}'    { Token {tv = RBrace} }
  '='    { Token {tv = Sym "=" } }
  '~'    { Token {tv = Sym "~" } }
  '-'    { Token {tv = Sym "-" } }
  '+'    { Token {tv = Sym "+" } }
  '*'    { Token {tv = Sym "*" } }
  '/'    { Token {tv = Sym "/" } }
  '^'    { Token {tv = Sym "^" } }
  ':'    { Token {tv = Sym ":" } }
  '('    { Token {tv = Sym "(" } }
  ')'    { Token {tv = Sym ")" } }
  '['    { Token {tv = Sym "[" } }
  ']'    { Token {tv = Sym "]" } }
  const  { Token {tv = Num $$ } }
  name	 { Token {tv = Id $$ } }
  ';'    { Token {tv = Sym ";"} }
  ','    { Token {tv = Sym ","} }
  '.'    { Token {tv = Sym "."} }
  '<'    { Token {tv = Sym "<"} }
  '>'    { Token {tv = Sym ">"} }
  var	 { Token {tv = Lexer.Var } }
%%

FigSpec  : BoxDef { [$1] }
         | FigSpec BoxDef { $1 ++ [$2] } 

BoxDef   : Body { $1 }
	 | box Body { $2 }

Body	 : name '{' '}'  { BoxDef $1 [] }
         | name '{' Statements '}'  { BoxDef $1 $3 }

Statements : Stmt	{ [$1] }
	   | Statements Stmt  { $1 ++ [$2] }

Stmt	: VarStmt ';'	{ $1 } 
        | EqnStmt ';' { EqnStmt $1 }
        | PutStmt  { $1 }
        | PenStmt ';' { $1 }
        | ConnStmt ';' { $1 }
        | DrawStmt ';' { $1 }
        | StrStmt ';' { $1 }


VarStmt : var VarList { VarStmt $2 }

VarList :: { [String] }
VarList : name	{ [$1] }
	| VarList ',' name { $1 ++ [$3] }

EqnStmt : Expr '=' Expr { EqnNode (EqnLeaf $1) (EqnLeaf $3) False }
  | Expr '~' Expr { EqnNode (EqnLeaf $1) (EqnLeaf $3) True }
  | EqnStmt '=' Expr { EqnNode $1 (EqnLeaf $3) False }
  | EqnStmt '~' Expr { EqnNode $1 (EqnLeaf $3) True }
 
PutStmt : PutWord name ':' Body { PutStmt (Just $2) $4 }
        | PutWord Body  { PutStmt (Nothing) $2 }
        | name ':' PutWord Body { PutStmt (Just $1) $4 }

PutWord : put { $1 }
        | construct { $1 }
  
PenStmt : conn KnotList using Expr Body '<' Expr ',' Expr '>' { PenStmt $2 $4 $5 $7 $9 }

ConnStmt : conn KnotList { ConnStmt $2 }


KnotList : Expr { [$1] }
         | KnotList to Expr { $1 ++ [$3] }
    
DrawStmt : draw name { DrawStmt $2 }

StrStmt : left string at Expr { StrStmt (JustLeft) $2 $4 }
        | right string at Expr { StrStmt (JustRight) $2 $4 }
        | center string at Expr { StrStmt (JustCenter) $2 $4 }
        | string at Expr { StrStmt (JustCenter) $1 $3 }

ExprList : Expr { [$1] }
  | ExprList ',' Expr { $1 ++ [$3] }
  
Expr :  '-' Expr { Neg $2 }
  | Expr '+' Expr { Add $1 $3 }
  | Expr '-' Expr { Sub $1 $3 }
  | Expr '*' Expr { Mul $1 $3 }
  | Expr '/' Expr { Div $1 $3 }
  | '^' Expr      { Conj $2 }
  | PathName      { Path $1 }
  | const { Const $1 }
  | '(' Expr ')' { $2 }
  | '(' Expr ',' Expr ')' { Comma $2 $4 }
  | Expr '[' Expr ',' Expr ']' { Bracket $1 $3 $5 }
  | name '(' ExprList ')' { App $1 $3 }

PathName : name { [$1] }
  | name '.' PathName { [$1] ++ $3 } 

{

parseError :: [Token] -> a
parseError l =
  let (fileName, line) = tp(head l)
      Just errtok = str(head l)
  in
  error $ fileName ++ ":" ++ (show line) ++ ":" 
           ++ " error on input `" ++ errtok ++ "'"
}

module Core where

type Figure = [BoxDef]

type Number = Float

data BoxDef =
  BoxDef {
    boxName :: String,
    boxStmts :: Statements
    } deriving Show

type Statements = [Stmt]

data Stmt =
  Stmt Int
  | VarStmt [VarName]
  | EqnStmt EqnTree
  | PutStmt (Maybe Name) BoxDef
  | ConnStmt [Expr]
  | PenStmt [Expr] Expr BoxDef Expr Expr
  | DrawStmt Name
  | StrStmt StrJust String Expr
  deriving Show

data StrJust =
  JustLeft
  | JustRight
  | JustCenter
  deriving Show

data EqnTree =
  EqnLeaf Expr
  | EqnNode EqnTree EqnTree Bool
  deriving Show

data Expr = Const Float
          | Neg Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Conj Expr
          | Comma Expr Expr
          | Bracket Expr Expr Expr
          | Path [Name]
          | App Name [Expr]
          deriving Show

type Name = String
type VarName = String

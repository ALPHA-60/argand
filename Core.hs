module Core where

type Figure = [BoxDef]

type Number = Float

data BoxDef =
  BoxDef {
    boxName :: String,
    boxStmts :: Statements
    }

type Statements = [Stmt]

data Stmt =
  Stmt Int
  | VarStmt [VarName]
  | EqnStmt Eqn
  | PutStmt (Maybe Name) BoxDef
  | ConnStmt [Expr]
  | PenStmt [Expr] Expr BoxDef Expr Expr
  | DrawStmt Name
  | StrStmt StrJust String Expr

data StrJust =
  JustLeft
  | JustRight
  | JustCenter

data Eqn =
  EqnLeaf Expr
  | EqnNode Eqn Eqn Bool

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

type Name = String
type VarName = String

data VarType = Real
             | Imag
             deriving (Eq, Ord)

type CplxVarId = Int

data Var = Var CplxVarId VarType
         deriving (Eq, Ord)

data VarRef = VarRef{
  refName :: Name,
  refId :: Int
}

data PutNode = PutNode {
  name :: Maybe Name,
  box  :: BoxDef
  }

varNames (VarStmt v) = v

putStmtToPutNode (PutStmt n b) = PutNode {name = n, box = b}


epsilon :: Float
epsilon = 0.0001


onlyVarStmts :: Statements -> Statements
onlyVarStmts s = [x | x@(VarStmt _) <- s]

onlyEqnStmts :: Statements -> Statements
onlyEqnStmts s = [x | x@(EqnStmt _) <- s]

onlyPutStmts :: Statements -> Statements
onlyPutStmts s = [x | x@(PutStmt _ _) <- s]


onlyPenStmts :: Statements -> Statements
onlyPenStmts s = [x | x@(PenStmt _ _ _ _ _) <- s]

onlyConnStmts :: Statements -> Statements
onlyConnStmts s = [x | x@(ConnStmt _) <- s]
module ArgState where

import Control.Monad.State
import Core
import LinComb
import Noad
import Data.Map (Map, empty, insert, (!))
import Data.List (find)

type ArgState a = State ArgM a


data ArgM = ArgM {
  figure :: Figure,
  varMap :: Map Var LinComb,
  maxVarID :: Int,
  depVarList :: [Var],
  nlFail :: Bool,
  nlEqns :: [NlEqn],
  context :: NoadTrail
}

data NlEqn =
  NlEqn {
    eqn :: Eqn,
    ctx :: NoadTrail
    }

getCurrNoadInfo :: ArgState ([Noad], Statements)
getCurrNoadInfo = do
  currNoad <- getCurrNoad
  let BoxDef name stmts = box $ defNode currNoad
  paradigmBox <- lookupBox name
  return (children currNoad, stmts ++ boxStmts paradigmBox)

initState :: Figure -> ArgM
initState figure =
  ArgM {
    figure = figure,
    varMap = empty,
    maxVarID = 1 ,
    depVarList = [],
    nlFail = False,
    nlEqns = [],
    context = []
    }

descendAnd :: ArgState a -> Noad -> ArgState a
descendAnd action n = do
  descend n
  res <- action
  ascend
  return res
  where descend n = do
          s <- get;
          put s {context  = n:(context s) }
        ascend = do
          s <- get;
          put s {context  = tail $ context s }


wasNonLinear :: ArgState Bool
wasNonLinear = do
  s <- get
  return $ nlFail s

pushNonLinEqn :: Eqn -> ArgState ()
pushNonLinEqn e = do
  s <- get
  put s {nlEqns = NlEqn{eqn = e, ctx = (context s)}:(nlEqns s)}


getCurrNoad :: ArgState Noad
getCurrNoad = do
  s <- get
  return $ head $ context s

lookupBox :: Name -> ArgState BoxDef
lookupBox name = do
  s <- get
  case find (\box -> name == boxName box) (figure s) of
    Nothing -> return $ BoxDef {boxName = "", boxStmts = []}
    Just box -> return box


getVar :: Var -> ArgState LinComb
getVar var = do
  s <- get
  return $ (varMap s) ! var

getDepVars :: ArgState [Var]
getDepVars = do
  s <- get
  return $ depVarList s

pushDepVar :: Var -> ArgState ()
pushDepVar var = do
  s <- get
  put s {depVarList = (var:(depVarList s))}

setVar :: Var -> LinComb -> ArgState ()
setVar var linComb = do
  s <- get
  put s {varMap = insert var linComb (varMap s) }


raiseNlFlag :: ArgState ()
raiseNlFlag = do
  s <- get
  put s {nlFail = True}

getNlEqns :: ArgState [NlEqn]
getNlEqns = do
  s <- get
  return $ nlEqns s

resetNlFlag :: ArgState ()
resetNlFlag = do
  s <- get
  put s {nlFail = False}

setContext :: NoadTrail -> ArgState ()
setContext trail = do
  s <- get
  put s { context = trail }

getContext :: ArgState NoadTrail
getContext = do
  s <- get
  return $ context s

getVarMap :: ArgState (Map Var LinComb)
getVarMap = liftM varMap get

makeNoadTree :: PutNode -> ArgState Noad
makeNoadTree putNode = do
  let name = boxName $ box putNode
      stmts = boxStmts $ box putNode
  paradigmBox <- lookupBox name
  let paradigmStmts =  boxStmts paradigmBox
  ev <- mapM allocNewVar (declaredVars stmts)
  pc <- mapM makeNoadTree (putNodes stmts)
  bv <- mapM allocNewVar (declaredVars paradigmStmts)
  bc <- mapM makeNoadTree (putNodes paradigmStmts)
  return (Noad{
             defNode = putNode,
             boxVars = bv,
             edgeVars = ev,
             children = pc ++ bc
         })
  where
    declaredVars stmts = concatMap varNames (onlyVarStmts stmts)
    putNodes stmts = map putStmtToPutNode (onlyPutStmts stmts)
    allocNewVar name = do
      s <- get
      let  varRef = VarRef {refName = name, refId = maxVarID s}
      makeFreshVar Real
      makeFreshVar Imag
      incrCplxVarCount
      return varRef
    makeFreshVar varType = do
      s <- get
      let nextCplxVarId = maxVarID s
      let newComb = [1.0 :*: Just (Var nextCplxVarId varType)]
      put s {varMap = insert (Var nextCplxVarId varType) newComb (varMap s) }
    incrCplxVarCount = do
      s <- get
      put s{maxVarID = maxVarID s + 1}

module Argand where
import Core

import Data.Map (Map, empty, insert, (!))
import Control.Monad.State
import Data.Maybe (catMaybes)
import Data.List (find)
import System.Environment (getArgs)
import LinComb
import qualified Parser
import qualified Lexer


dtor x = x * (pi/ 180)

lookupBox figure name =
  case find (\box -> name == boxName box) figure of
    Nothing -> BoxDef {boxName = "", boxStmts = []}
    Just box -> box


data NoadTree = Noad {
  defNode :: PutNode,
  boxVars ::  [VarRef],
  edgeVars ::  [VarRef],
  children :: [NoadTree]
  }

type NoadPath = [NoadTree]

justConnStmts :: Statements -> [Stmt]
justConnStmts stmts =
  [ s | s <- stmts, case s of ConnStmt _ -> True; _ -> False ]

data PutNode = PutNode {
  name :: Maybe Name,
  box  :: BoxDef
  }

putStmtToPutNode (PutStmt n b) = PutNode {name = n, box = b}

data NoadState = NoadState {
  varMap :: Map Var LinComb,
  maxVarID :: Int,
  depVarList :: [Var],
  nlFail :: Bool,
  nlEqns :: [(EqnTree, NoadPath)]
}

getVar var = do
  s <- get
  return $ (varMap s) ! var

setVar var linComb = do
  s <- get
  put s {varMap = insert var linComb (varMap s) }


raiseNlFlag = do
  s <- get
  put s {nlFail = True}

resetNlFlag :: State NoadState ()
resetNlFlag = do
  s <- get
  put s {nlFail = False}

data VarRef = VarRef{
  refName :: Name,
  refId :: Int
}

varNames (VarStmt v) = v

makeNoadTree :: Figure -> PutNode -> State NoadState NoadTree
makeNoadTree figure putNode = do
  let name = boxName $ box putNode
      stmts = boxStmts $ box putNode
      paradigmBox = lookupBox figure name
      paradigmStmts =  boxStmts paradigmBox
  ev <- mapM allocNewVar (declaredVars stmts)
  pc <- mapM (makeNoadTree figure) (putNodes stmts)
  bv <- mapM allocNewVar (declaredVars paradigmStmts)
  bc <- mapM (makeNoadTree figure) (putNodes paradigmStmts)
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
      let newComb = mkLinComb (Var nextCplxVarId varType) 1.0
      put s {varMap = insert (Var nextCplxVarId varType) newComb (varMap s) }
    incrCplxVarCount = do
      s <- get
      put s{maxVarID = maxVarID s + 1}

initState = NoadState {
  varMap = empty,
  maxVarID = 1 ,
  depVarList = [],
  nlFail = False,
  nlEqns = []
  }

data DepPair = DepPair {
  rePart :: LinComb,
  imPart :: LinComb
  }

varFind name [] =
  return $ mkConst 0.0

varFind name (currNoad:ancestry) = do
  let refVarList = (edgeVars currNoad) ++ (boxVars currNoad)
  case find (\vr -> refName vr == name) refVarList of
    Nothing -> varFind name ancestry
    Just refvar -> do
      s <- get
      let r = (varMap s) ! (Var (refId refvar) Real)
          i = (varMap s) ! (Var (refId refvar) Imag)
      return $ mkDep (r, i)

pathFind [name] np =
  varFind name np

pathFind (n:names) npath@(currNoad:ancestry) =
  let noadList = children currNoad in
  case find (\nd -> name (defNode nd) == Just n) noadList of
    Nothing ->
      case ancestry of
        [] -> return $ mkConst 0.0
        parent:_ ->
          let noadList = children parent in
          case find (\nd -> name (defNode nd) == Just n) noadList of
            Nothing -> return $ mkConst 0.0
            Just noad ->
              pathFind names (noad:ancestry)
    Just noad ->
      pathFind names (noad:npath)

isKnown dp =
  isKnownComb (rePart dp) && isKnownComb (imPart dp)

re = getValue . rePart
im = getValue . imPart

mkDep (re, im) =
  DepPair {
    rePart = re,
    imPart = im
    }

mkConst c = mkDep (mkConstComb c, mkConstComb 0.0)

evalExpr expr noadTrail =
  eval expr where
    eval e =
      case e of
        Const c ->
          return $ mkConst c
        Path p -> pathFind p noadTrail
        Neg e -> do
          e' <- eval e
          return $ mkDep ((-1) |*| rePart e', (-1) |*| imPart e')
        Add ex ey -> do
          x <- eval ex
          y <- eval ey
          return $ mkDep (rePart x |+| rePart y, imPart x |+| imPart y)
        Sub ex ey -> do
          x <- eval ex
          y <- eval ey
          return $ mkDep (rePart x |-| rePart y, imPart x |-| imPart y)
        Mul ex ey -> do
          x <- eval ex
          y <- eval ey
          let mult knownExpr x =
                let (r, i) = (re knownExpr, im knownExpr)
                in return $ mkDep (r |*| rePart x  |-| i |*| imPart x,
                                   i |*| rePart x  |+| r |*| imPart x)
          case (isKnown x, isKnown y) of
            (True, _) -> mult x y
            (_, True) -> mult y x
            (_, _ ) -> do
              raiseNlFlag
              return $ mkConst 1.0
        Div ex ey -> do
          x <- eval ex
          y <- eval ey
          if isKnown y
            then
            let r = re y
                i = - (im y)
                modulus = r * r + i * i
            in
             if (modulus < epsilon * epsilon)
             then return $ mkConst 1.0 -- division by zero
             else return $ mkDep ((r / modulus) |*| rePart x  |-|  (i / modulus) |*| imPart x,
                                  (i / modulus) |*| rePart x  |+|  (r / modulus) |*| imPart x)
            else do
              raiseNlFlag
              return $ mkConst 1.0
        Bracket coeff start end -> eval (Add start (Mul coeff (Sub end start)))
        Comma ex ey -> do
          x <- eval ex
          y <- eval ey
          return $ mkDep (rePart x, rePart y)
        App fun exprs -> do
          case fun of
            "cis" -> do
              x <- eval (head exprs)
              if isKnown x
                then return $ mkDep (mkConstComb (cos (dtor (re x))),
                                     mkConstComb (sin (dtor (re x))))
                else
                do
                  raiseNlFlag
                  return $ mkConst 1.0
            "abs" -> do
              x <- eval (head exprs)
              if isKnown x
                then return $ mkConst $ sqrt $ (re x)**2 + (im x)**2
                else
                do
                  raiseNlFlag
                  return $ mkConst 1.0
            _ -> error "unknown function"

        _ -> error "Don't know how to handle this"


eqnEval figure noadTrail =
  let currNoad = head noadTrail
      BoxDef name stmts = box $ defNode currNoad
      paradigmBox = lookupBox figure name
      eqnStmts = onlyEqnStmts stmts
      paradigmEqnStmts = onlyEqnStmts (boxStmts paradigmBox)
  in do
    forM_ (eqnStmts ++ paradigmEqnStmts) trySolve
    --XXX: order?
    forM_ (children currNoad) (\n -> eqnEval figure (n:noadTrail))
    return ()
    where trySolve (EqnStmt e) = do
            rEqnEval e noadTrail
            s <- get
            if nlFail s then
               do
                 pushNlEq e
                 resetNlFlag
              else
              return ()
          pushNlEq eqn = do
            s <- get
            put s {nlEqns = (eqn, noadTrail):(nlEqns s)}

rEqnEval et noadtree =
      case et of
        EqnLeaf expr -> evalExpr expr noadtree
        EqnNode etl etr _ -> do
          xl <- rEqnEval etl noadtree
          xr <- rEqnEval etr noadtree
          s <- get
          if (nlFail s) then
            return xr
            else do
            [rl, il, rr, ir] <- mapM substDepVarsInto
                  [rePart xl, imPart xl, rePart xr, imPart xr]
            eqnDo $ rl |-| rr
            --s <- get
            [il', ir'] <- mapM substDepVarsInto [il, ir]
            {-
             Wyk optimises for the fact that only one var might have
             gotten into the list of dependent vars after first eqnDo:
             case depVarList s of
             [] -> return [il, ir]
             newVarId:_ -> do
               s <- get
               let newVarDeps = (varMap s) ! newVarId
               return $ map (depSubst newVarId newVarDeps) [il, ir]
            -}
            eqnDo $ il' |-| ir'
-- XXX: fix this
            return xr
      where substDepVarsInto linComb = do
              s <- get
              return $ foldl
                (\acc var -> let lc = (varMap s) ! var in depSubst var lc acc)
                linComb
                (depVarList s)

nlDo toSolve haveChance failures = do
  case toSolve of
    (eqn, noadtree):rest -> do
      resetNlFlag
      _ <- rEqnEval eqn noadtree
      s <- get
      if nlFail s then
        nlDo rest haveChance ((eqn, noadtree): failures)
        else
        nlDo rest True failures
    [] -> if haveChance then
            nlDo failures False []
          else do
            _ <- mapM (\(eqn, noadtree) -> rEqnEval eqn noadtree) failures
            return ()


eqnDo linComb
  | isKnownComb linComb = return ()
  | otherwise = do
    let (dv, maxCoeff) = maxTermVarAndCoeff linComb
    var <- getVar dv
    let linComb' = (var |-| (1.0/maxCoeff) |*| linComb)
    setVar dv linComb'
    s <- get
    forM_ (depVarList s) (substInto dv linComb')
    addToDepList dv
  where
    substInto srcVar lc var = do
      lcv <- getVar var
      setVar var (depSubst srcVar lc lcv)
    addToDepList var = do
      s <- get
      put s {depVarList = (var:(depVarList s))}


data Shape =
  Line {
    x0 :: Float,
    y0 :: Float,
    x1 :: Float,
    y1 :: Float
    }

type ShapeList = [Shape]

connStmtToLineList noadPath (ConnStmt exprs)  = do
  zipWithM makeLine exprs (tail exprs)
  where makeLine e e' = do
          e0 <- evalExpr e noadPath
          e1 <- evalExpr e' noadPath
          if isKnown e0 && isKnown e1 then
            return (Just (Line {
                            x0 = re e0,
                            y0 = im e0,
                            x1 = re e1,
                            y1 = im e1
                            }))
            else
            return Nothing


{-
  conn a to b using n box {...} <x, y>;

  is a shorthand for

  for i = 1 to n
     put box {
       x = ((i-1)/n) [a, b];
   y = (i/n) [a, b];
   ...
 };
-}
evalPenStmt :: Figure -> NoadPath -> Stmt -> State NoadState [Maybe Shape]
evalPenStmt fig noadTrail (PenStmt (a:b:_) nExpr (BoxDef name stmts) x y) = do
  nVal <- evalExpr nExpr noadTrail
  if not $ isKnown nVal
    then return []
    else do
    let n = re nVal
        mkEqn var i =
          EqnStmt (EqnNode
                   (EqnLeaf var)
                   (EqnLeaf (Bracket (Div i (Const n)) a b))
                   False)
        xEqn i = mkEqn x (Sub (Const i) (Const 1.0))
        yEqn i = mkEqn y (Const i)
        mkPut i = PutNode {
          name = Nothing,
          box = BoxDef name (xEqn i:yEqn i:stmts)
          }
        newPuts = map mkPut [1.0 .. n]
    noads <- mapM (makeNoadTree fig) newPuts
    forM_ noads (\n -> eqnEval fig (n:noadTrail))
    l <- mapM (\n -> collectShapes fig (n:noadTrail)) noads
    return $ concat l


collectShapes figure noadPath@(currNoad:ancestry) = do
  let BoxDef name stmts = box $ defNode currNoad
      paradigmBox = lookupBox figure name
      paradigmStmts = boxStmts paradigmBox
      connStmts = onlyConnStmts $ stmts ++ paradigmStmts
      penStmts = onlyPenStmts $ stmts ++ paradigmStmts
  x <- mapM (connStmtToLineList noadPath) connStmts
  l <- mapM (\n -> collectShapes figure (n:noadPath)) (children currNoad)
  y <- mapM (evalPenStmt figure noadPath) penStmts
  return $ concat (x ++ y ++ l)


data Extent = Extent {
  maxX :: Float,
  maxY :: Float,
  minX :: Float,
  minY :: Float
  }

mergeExtents ext1 ext2 = Extent {
  maxX = max (maxX ext1) (maxX ext2),
  maxY = max (maxY ext1) (maxY ext2),
  minX = min (minX ext1) (minX ext2),
  minY = min (minY ext1) (minY ext2)
  }

extent shape =
  case shape of
    l@(Line {}) -> Extent {
      maxX = max (x0 l) (x1 l),
      maxY = max (y0 l) (y1 l),
      minX = min (x0 l) (x1 l),
      minY = min (y0 l) (y1 l)
      }

idMain parser fig = do
  let (a, s) = runState
               (makeNoadTree (parser fig) (PutNode (Just "main") (BoxDef "main" [])))
               initState
      (a', s') = runState (do {
                              eqnEval (parser fig) [a];
                              s <- get;
                              nlDo (nlEqns s) False [];
                              shps <- collectShapes  (parser fig) [a];
                              return shps
                              }) s
  putStr "%!\n"
  putStr "/inch {72 mul} def\n"
  psConv $ catMaybes a'

psConv [] = do
  putStr "showpage\n"

psConv (s:ss) =
  case s of
    Line {} -> do
      putStr $ (show $ x0 s) ++ " inch " ++ (show $ y0 s) ++ " inch moveto\n"
      putStr $ (show $ x1 s) ++ " inch " ++ (show $ y1 s) ++ " inch lineto\n"
      putStr "stroke\n"
      psConv ss
    _ -> return ()

main = do
 args <- getArgs
 z <- Lexer.arglex (args !! 0)
 case z of
   Left x -> putStr "Error"
   Right fig -> idMain Parser.argparser fig

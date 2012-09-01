module Evaluator where
import Core
import Noad
import ArgState
import LinComb
import Control.Monad.State
import Control.Monad.Error
import Data.Map (Map, empty, insert, (!))
import Data.List (find)

data Cplx = Cplx {
  re :: LinComb,
  im :: LinComb
  }


knownRe = getValue . re

knownIm = getValue . im


mkCplx :: (LinComb, LinComb) -> Cplx
mkCplx (r, i) = Cplx {re = r, im = i}


dtor x = x * (pi/ 180)


mkRealConst :: Float -> Cplx
mkRealConst c = mkCplx (mkConstComb c, mkConstComb 0.0)


isKnown :: Cplx -> Bool
isKnown x =
  isKnownComb (re x) && isKnownComb (im x)


eval :: Expr -> ErrorT ArgError (State PrgState) Cplx
eval (Const c) = return $ mkRealConst c

eval (Path p) = do
  breadcrumbs <- getContext
  pathFind p breadcrumbs


eval (Neg e) = do
  e' <- eval e
  return $ mkCplx ((-1) |*| re e', (-1) |*| im e')


eval (Add ex ey) = do
  x <- eval ex
  y <- eval ey
  return $ mkCplx (re x |+| re y, im x |+| im y)


eval (Sub ex ey) = do
  x <- eval ex
  y <- eval ey
  return $ mkCplx (re x |-| re y, im x |-| im y)

eval (Mul ex ey) = do
  x <- eval ex
  y <- eval ey
  let mult knownExpr x =
        let (r, i) = (knownRe knownExpr, knownIm knownExpr)
        in return $ mkCplx (r |*| re x  |-| i |*| im x,
                            i |*| re x  |+| r |*| im x)
  case (isKnown x, isKnown y) of
    (True, _) -> mult x y
    (_, True) -> mult y x
    (_, _ ) ->  throwError NonLinear


eval (Div ex ey) = do
  x <- eval ex
  y <- eval ey
  if isKnown y
    then let r = knownRe y
             i = - (knownIm y)
             modulus = r * r + i * i
         in
          if (modulus < epsilon * epsilon)
          then return $ mkRealConst 1.0 -- division by zero
          else do
            let r' = (r / modulus) |*| re x |-| (i / modulus) |*| im x
                i' = (i / modulus) |*| re x |+| (r / modulus) |*| im x
            return $ mkCplx (r', i')
    else throwError NonLinear


eval (Bracket coeff start end) = eval (Add start (Mul coeff (Sub end start)))


eval (Comma ex ey) = do
  x <- eval ex
  y <- eval ey
  return $ mkCplx (re x, re y)


eval (App f es) = evalFun f es


eval _ = error "Don't know how to handle this expr"


evalFun :: Name -> [Expr] -> ErrorT ArgError (State PrgState) Cplx
evalFun "cis" (e:_) = do
  x <- eval e
  if isKnown x
    then return $ mkCplx (mkConstComb (cos (dtor (knownRe x))),
                          mkConstComb (sin (dtor (knownRe x))))
    else throwError NonLinear

evalFun "abs" (e:_) = do
  x <- eval e
  if isKnown x
    then return $ mkRealConst $ sqrt $ (knownRe x)**2 + (knownIm x)**2
    else throwError NonLinear

evalFun _ _ = error "unknown function"


varFind :: Name -> NoadTrail -> ErrorT ArgError (State PrgState) Cplx
varFind name [] =
  -- TODO: warn or throw error
  return $ mkRealConst 0.0


varFind name (currNoad:ancestry) = do
  let refVarList = (edgeVars currNoad) ++ (boxVars currNoad)
  case find (\vr -> refName vr == name) refVarList of
    Nothing -> varFind name ancestry
    Just refvar -> do
      r <- getVar (Var (refId refvar) Real)
      i <- getVar (Var (refId refvar) Imag)
      return $ mkCplx (r, i)


pathFind [name] np =
  varFind name np


pathFind (n:names) npath@(currNoad:ancestry) =
  let noadList = children currNoad in
  case find (\nd -> name (defNode nd) == Just n) noadList of
    Nothing ->
      case ancestry of
        [] -> return $ mkRealConst 0.0
        parent:_ ->
          let noadList = children parent in
          case find (\nd -> name (defNode nd) == Just n) noadList of
            Nothing -> return $ mkRealConst 0.0
            Just noad ->
              pathFind names (noad:ancestry)
    Just noad ->
      pathFind names (noad:npath)
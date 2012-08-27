module Evaluator where
import Core
import Noad
import ArgState
import LinComb
import Control.Monad.State
import Data.Map (Map, empty, insert, (!))
import Data.List (find)

data Cplx = Cplx {
  rePart :: LinComb,
  imPart :: LinComb
  }


re = getValue . rePart


im = getValue . imPart


mkCplx :: (LinComb, LinComb) -> Cplx
mkCplx (re, im) = Cplx {rePart = re, imPart = im}


dtor x = x * (pi/ 180)


mkRealConst :: Float -> Cplx
mkRealConst c = mkCplx (mkConstComb c, mkConstComb 0.0)


isKnown :: Cplx -> Bool
isKnown x =
  isKnownComb (rePart x) && isKnownComb (imPart x)


eval :: Expr -> ArgState Cplx
eval (Const c) = return $ mkRealConst c

eval (Path p) = do
  breadcrumbs <- getContext
  pathFind p breadcrumbs


eval (Neg e) = do
  e' <- eval e
  return $ mkCplx ((-1) |*| rePart e', (-1) |*| imPart e')


eval (Add ex ey) = do
  x <- eval ex
  y <- eval ey
  return $ mkCplx (rePart x |+| rePart y, imPart x |+| imPart y)


eval (Sub ex ey) = do
  x <- eval ex
  y <- eval ey
  return $ mkCplx (rePart x |-| rePart y, imPart x |-| imPart y)

eval (Mul ex ey) = do
  x <- eval ex
  y <- eval ey
  let mult knownExpr x =
        let (r, i) = (re knownExpr, im knownExpr)
        in return $ mkCplx (r |*| rePart x  |-| i |*| imPart x,
                            i |*| rePart x  |+| r |*| imPart x)
  case (isKnown x, isKnown y) of
    (True, _) -> mult x y
    (_, True) -> mult y x
    (_, _ ) -> do
      raiseNlFlag
      return $ mkRealConst 1.0


eval (Div ex ey) = do
  x <- eval ex
  y <- eval ey
  if isKnown y
    then let r = re y
             i = - (im y)
             modulus = r * r + i * i
         in
          if (modulus < epsilon * epsilon)
          then return $ mkRealConst 1.0 -- division by zero
          else do
            let re = (r / modulus) |*| rePart x |-| (i / modulus) |*| imPart x
                im = (i / modulus) |*| rePart x |+| (r / modulus) |*| imPart x
            return $ mkCplx (re, im)
    else do
    raiseNlFlag
    return $ mkRealConst 1.0


eval (Bracket coeff start end) = eval (Add start (Mul coeff (Sub end start)))


eval (Comma ex ey) = do
  x <- eval ex
  y <- eval ey
  return $ mkCplx (rePart x, rePart y)


eval (App f es) = evalFun f es


eval _ = error "Don't know how to handle this expr"


evalFun :: Name -> [Expr] -> ArgState Cplx
evalFun "cis" (e:_) = do
  x <- eval e
  if isKnown x
    then return $ mkCplx (mkConstComb (cos (dtor (re x))),
                          mkConstComb (sin (dtor (re x))))
    else do
    raiseNlFlag
    return $ mkRealConst 1.0

evalFun "abs" (e:_) = do
  x <- eval e
  if isKnown x
    then return $ mkRealConst $ sqrt $ (re x)**2 + (im x)**2
    else do
    raiseNlFlag
    return $ mkRealConst 1.0

evalFun _ _ = error "unknown function"


varFind :: Name -> NoadTrail -> ArgState Cplx
varFind name [] =
  return $ mkRealConst 0.0


varFind name (currNoad:ancestry) = do
  let refVarList = (edgeVars currNoad) ++ (boxVars currNoad)
  case find (\vr -> refName vr == name) refVarList of
    Nothing -> varFind name ancestry
    Just refvar -> do
      s <- get
      let r = (varMap s) ! (Var (refId refvar) Real)
          i = (varMap s) ! (Var (refId refvar) Imag)
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
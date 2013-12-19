module Evaluator where
import Prelude hiding (div, abs)
import Core
import Noad
import ArgState (ArgState, getContext, getVarMap)
import Complex
import LinComb
import Control.Monad.State
import Data.Map (Map, (!))
import Data.List (find)
import Data.Maybe (fromJust)
import Control.Monad.Trans.Either
import Control.Monad.Reader (asks)
import Control.Applicative hiding (Const)

data Env = Env {context :: NoadTrail, varMap :: Map Var LinComb }

evaluate :: Expr -> ArgState (Either String Complex)
evaluate e = runEitherT (eval e) <$> (Env <$> getContext <*> getVarMap)


type EvalResult a = EitherT String ((->) Env) a


evalToRealOr :: Expr -> String -> EvalResult Complex
evalToRealOr e err = eval e >>= \x -> if isConst x then return x else left err


eval :: Expr -> EvalResult Complex
eval (Path p) = pathFind p =<< (asks context)

eval (Const c) = return $ scalar c

eval (Neg e) = neg <$> eval e

eval (Add ex ey) = add <$> eval ex <*> eval ey

eval (Sub ex ey) = sub <$> eval ex <*> eval ey

eval (Mul ex ey) = do
  x <- eval ex; y <- eval ey
  maybe (left "non linear") return $ (mul x y) `mplus` (mul y x)

eval (Div ex ey) = div <$> eval ex <*>  ey `evalToRealOr` "non linear"

eval (Bracket coeff start end) = eval (Add start (Mul coeff (Sub end start)))

eval (Comma ex ey) = realParts <$> eval ex <*> eval ey

eval (App f es) = evalFun f es



evalFun :: Name -> [Expr] -> EvalResult Complex
evalFun "cis" (e:_) = cis <$> e `evalToRealOr` "non linear"

evalFun "abs" (e:_) = abs <$> e `evalToRealOr` "non linear"

evalFun _ _ = error "unknown function"



varFind :: Name -> NoadTrail -> EvalResult Complex
varFind name [] = return $ scalar 0.0

varFind name (currNoad:ancestry) =
  case find ((name ==) . refName) $ (edgeVars currNoad) ++ (boxVars currNoad) of
    Nothing -> varFind name ancestry
    Just refvar -> (:+:) <$> getVar Real refvar <*> getVar Imag refvar
  where getVar t v = (! (Var (refId v) t)) <$> asks varMap 



pathFind :: [Name] -> NoadTrail -> EvalResult Complex
pathFind [name] np = varFind name np

pathFind (n:names) ancestry =
  fromJust $ search ancestry `mplus` search (tail ancestry) `mplus` (Just $ return $ scalar 0.0)
  where search [] = Nothing
        search ctx = (\x -> pathFind names (x:ctx)) <$> findChild (head ctx)
        findChild = (find ((Just n ==) . name . defNode)) . children

module Solver (solveEquations, solveNonLinearEqns) where

import Control.Monad.State
import Control.Monad.Trans.Either (runEitherT)
import Complex (scalar, Complex((:+:)))
import Core
import Evaluator (evaluate)
import LinComb
import Noad
import ArgState


solveEquations = do
  (children, stmts) <- getCurrNoadInfo
  let eqns = onlyEqnStmts stmts
  forM_ eqns tryEqn
  forM_ children (descendAnd solveEquations)
  where tryEqn (EqnStmt e) = do
          handleEqn e
          nl <- wasNonLinear
          if nl
            then do pushNonLinEqn e; resetNlFlag
            else return ()


handleEqn :: Eqn -> ArgState Complex
handleEqn et =
  case et of
    EqnLeaf expr -> do
      res <- evaluate expr
      case res of
        Right c -> return c
        Left _ -> do { raiseNlFlag ; return (scalar 0.0) }
    EqnNode x y _ -> do
      xr :+: xi <- handleEqn x
      yr :+: yi <- handleEqn y
      nl <- wasNonLinear
      if nl
        then return (xr :+: xi)
        else do
        [lhsRe, lhsIm, rhsRe, rhsIm] <- mapM substDepVarsInto
              [xr, xi, yr, yi]
        eqnDo $ lhsRe |-| rhsRe
        [lhsIm', rhsIm'] <- mapM substDepVarsInto [lhsIm, rhsIm]
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
        eqnDo $ lhsIm' |-| rhsIm'
-- XXX: fix this
        return (yr :+: yi)
  where substDepVarsInto linComb = do
          depVars <- getDepVars
          foldM
            (\acc var -> do
                lc <- getVar var
                return $ depSubst var lc acc)
            linComb
            depVars


eqnDo linComb
  | isKnownComb linComb = return ()
  | otherwise = do
    let (dv, maxCoeff) = maxTermVarAndCoeff linComb
    var <- getVar dv
    let linComb' = (var |-| (1.0/maxCoeff) |*| linComb)
    setVar dv linComb'
    depVars <- getDepVars
    forM_ depVars (substInto dv linComb')
    pushDepVar dv
  where
    substInto srcVar lc destVar = do
      destComb <- getVar destVar
      setVar destVar (depSubst srcVar lc destComb)


solveNonLinearEqns = do
  nleqns <- getNlEqns
  solveNls nleqns [] False
  where
  solveNls :: [NlEqn] -> [NlEqn] -> Bool -> ArgState ()
  solveNls toSolve failures haveNewData = do
    case toSolve of
      nl:rest -> do
        resetNlFlag
        setContext (ctx nl)
        handleEqn (eqn nl)
        nonLin <- wasNonLinear
        if nonLin
          then solveNls rest (nl:failures) haveNewData
          else solveNls rest failures True
      [] -> if haveNewData
            then solveNls failures [] False
            else do
              mapM_ (\nl -> do
                        setContext (ctx nl)
                        handleEqn (eqn nl)
                    ) failures

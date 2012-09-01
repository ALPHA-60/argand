module Main where

import Core

import Control.Monad.State
import Control.Monad.Error
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import ArgState
import LinComb
import Evaluator
import Solver
import qualified Parser
import qualified Lexer



data Shape =
  Line {
    x0 :: Float,
    y0 :: Float,
    x1 :: Float,
    y1 :: Float
    }

evaluate :: Expr -> ErrorT ArgError (State PrgState) Cplx
evaluate e = do
  result <- eval e
  if isKnown result
    then return result
    else throwError UndeterminedVar


connStmtToLineList (ConnStmt exprs)  = do
  lines <- zipWithM (\el er -> tryComputing $ makeLine el er) exprs (tail exprs)
  return $ [line | Right line <- lines]
  where makeLine e e' = do
          e0 <- evaluate e
          e1 <- evaluate e'
          return (Line {
              x0 = knownRe e0,
              y0 = knownIm e0,
              x1 = knownRe e1,
              y1 = knownIm e1
              })


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
evalPenStmt :: Stmt -> ArgState [Shape]
evalPenStmt (PenStmt (a:b:_) nExpr (BoxDef name stmts) x y) = do
  let getNewPutStmts = do
        v <- evaluate nExpr
        let n = knownRe v
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
        return $ map mkPut [1.0 .. n]
  newPutStmts <- tryComputing getNewPutStmts
  case newPutStmts of
    Left UndeterminedVar -> return []
    Right newPuts -> do
      noads <- mapM makeNoadTree newPuts
      forM_ noads (descendAnd solveEquations)
      l <- mapM (descendAnd collectShapes) noads
      return $ concat l


collectShapes  = do
  (children, stmts) <- getCurrNoadInfo
  let connStmts = onlyConnStmts stmts
      penStmts = onlyPenStmts stmts
  s1 <- mapM connStmtToLineList connStmts
  s2 <- mapM evalPenStmt penStmts
  s3 <- mapM (descendAnd collectShapes) children
  return $ concat (s1 ++ s2 ++ s3)


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

process = do
  n <- makeNoadTree (PutNode (Just "main") (BoxDef "main" []))
  setContext [n]
  solveEquations
  solveNonLinearEqns -- this destroys the context
  setContext [n]
  collectShapes

idMain figure = do
  let shapes = evalState process (initState figure)
  putStr "%!\n"
  putStr "/inch {72 mul} def\n"
  psConv shapes

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
   Right fig -> idMain (Parser.argparser fig)

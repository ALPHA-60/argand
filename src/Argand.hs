module Main where

import Core

import Control.Monad.State
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import ArgState
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


type ShapeList = [Shape]


connStmtToLineList (ConnStmt exprs)  = do
  zipWithM makeLine exprs (tail exprs)
  where makeLine e e' = do
          e0 <- eval e
          e1 <- eval e'
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
evalPenStmt :: Stmt -> ArgState [Maybe Shape]
evalPenStmt (PenStmt (a:b:_) nExpr (BoxDef name stmts) x y) = do
  nVal <- eval nExpr
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
    noads <- mapM makeNoadTree newPuts
    forM_ noads (\n -> doInside n solveEquations)
    l <- mapM (\n -> doInside n collectShapes) noads
    return $ concat l


collectShapes  = do
  (currNoad, children, stmts, paradigmStmts) <- getCurrNoadInfo
  let connStmts = onlyConnStmts $ stmts ++ paradigmStmts
      penStmts = onlyPenStmts $ stmts ++ paradigmStmts
  x <- mapM connStmtToLineList connStmts
  l <- mapM (\n -> doInside n collectShapes) children
  y <- mapM evalPenStmt penStmts
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

process = do
  n <- makeNoadTree (PutNode (Just "main") (BoxDef "main" []))
  setContext [n]
  solveEquations
  s <- get
  solveNonLinearEqns (nlEqns s) [] False -- this destroys the context
  setContext [n];
  shps <- collectShapes;
  return shps

idMain figure = do
  let shapes = evalState process (initState figure)
  putStr "%!\n"
  putStr "/inch {72 mul} def\n"
  psConv $ catMaybes shapes

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

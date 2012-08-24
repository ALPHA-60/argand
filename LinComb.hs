{-
  Linear combinations are of the form "a*x + b*y + c + ...".
-}

module LinComb ((|+|), (|-|), (|*|), LinComb (..),
                mkLinComb, isKnownComb, getValue,
                mkConstComb, depSubst, maxTermVarAndCoeff) where

import Core (Var, epsilon)
import Data.Maybe (isJust, isNothing)
import Data.List (nubBy, partition, maximumBy)

data LinTerm = LinTerm {
  varId :: Maybe Var, -- constant terms have Nothing in this field
  coeff :: Float
}

type LinComb = [LinTerm]

infixl 6 |+|, |-|
infixl 7 |*|


(|+|) :: LinComb -> LinComb -> LinComb
x |+| y =
  let termGroups = groupByVar (x ++ y)
      sum = [t | t <- map sumCoeffs termGroups, abs (coeff t) > epsilon]
  in
   if null sum then mkConstComb 0.0 else sum
  where
    sumCoeffs = foldr1 (\t1 t2 -> t1 { coeff = coeff t1 + coeff t2 })
    groupByVar lcomb = map
                       (\t -> [t' | t' <- lcomb, haveSameVar t t'])
                       (nubBy haveSameVar lcomb)
    haveSameVar t1 t2 = (varId t1) == (varId t2)


(|*|) :: Float -> LinComb -> LinComb
a |*| x = map (\t -> t {coeff = coeff t * a} ) x


(|-|) :: LinComb -> LinComb -> LinComb
x |-| y = x |+| ((-1.0) |*| y)


-- Creates a linear combination of one term
mkLinComb :: Var -> Float -> LinComb
mkLinComb v c = [LinTerm {varId = Just v, coeff = c}]


mkConstComb :: Float -> LinComb
mkConstComb c = [LinTerm {varId = Nothing, coeff = c}]


-- solver will reduce linear combinations with
-- determined vars to singleton lists of constant
-- terms
isKnownComb :: LinComb -> Bool
isKnownComb x =
  hasOneTerm x && knownTerm (head x)
  where
    hasOneTerm x = length x == 1
    knownTerm = isNothing . varId


-- get value of a linear combination whose value has been
-- determined by solver
getValue :: LinComb-> Float
getValue = coeff . head


-- substitutes every a*x term in 'to' linear combination with
-- a*from
depSubst :: Var -> LinComb -> LinComb -> LinComb
depSubst x from to =
  let (xTerms, nonXterms) = partition (\t -> varId t == Just x) to
      replacements = concatMap (\t -> (coeff t) |*| from) xTerms
  in
    nonXterms |+| replacements


maxTermVarAndCoeff :: LinComb -> (Var, Float)
maxTermVarAndCoeff x =
  let nonConsts = filter (isJust . varId) x
      LinTerm { varId = Just dv, coeff = c } = maximumBy compareCoeffs nonConsts
  in
  (dv, c)
  where compareCoeffs t1 t2 = compare (abs $ coeff t1) (abs $ coeff t2)
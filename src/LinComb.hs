{-
  Linear combinations are of the form "a*x + b*y + c + ...".
-}

module LinComb where

import Core (Var, epsilon)
import Data.Maybe (isJust, isNothing)
import Data.List (nubBy, partition, maximumBy)

data LinTerm = Float :*: Maybe Var -- constant terms have `Nothing` here

type LinComb = [LinTerm]

infixl 6 |+|, |-|
infixl 7 |*|

(|+|) :: LinComb -> LinComb -> LinComb
x |+| y =
  let termGroups = groupByVar (x ++ y)
      sum = [t | t@(a :*: x) <- map sumCoeffs termGroups, abs a > epsilon]
  in
   if null sum then mkConstComb 0.0 else sum
  where
    sumCoeffs = foldr1 (\(a :*: x) (b :*: _) -> (a + b) :*: x)
    groupByVar lcomb = map
                       (\t -> [t' | t' <- lcomb, haveSameVar t t'])
                       (nubBy haveSameVar lcomb)
    haveSameVar (_ :*: x) (_ :*: y) = x == y


(|*|) :: Float -> LinComb -> LinComb
a |*| l = map (\(b :*: x) -> (a * b) :*: x) l


(|-|) :: LinComb -> LinComb -> LinComb
x |-| y = x |+| ((-1.0) |*| y)

negate :: LinComb -> LinComb
negate = ((-1) |*|)



mkConstComb :: Float -> LinComb
mkConstComb c = [c :*: Nothing]


-- solver will reduce linear combinations with
-- determined vars to singleton lists of constant
-- terms
isKnownComb :: LinComb -> Bool
isKnownComb [_ :*: Nothing] = True
isKnownComb _ = False


-- get value of a linear combination whose value has been
-- determined by solver
getValue :: LinComb-> Float
getValue [a :*: _] = a


-- substitutes every a*x term in 'to' linear combination with
-- a*from
depSubst :: Var -> LinComb -> LinComb -> LinComb
depSubst x from to =
  let (xTerms, nonXterms) = partition (\(_ :*: v) -> v == Just x) to
      replacements = concatMap (\(a :*: _) -> a |*| from) xTerms
  in
    nonXterms |+| replacements


maxTermVarAndCoeff :: LinComb -> (Var, Float)
maxTermVarAndCoeff x =
  let nonConsts = filter nonConst x
      a :*: Just dv  = maximumBy compareCoeffs nonConsts
  in (dv, a)
  where compareCoeffs (a :*: _) (b :*: _) = compare (abs a) (abs b)
        nonConst (_ :*: x) = isJust x

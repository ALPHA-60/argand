module Noad where

import Core
  
data Noad = Noad {
  defNode :: PutNode,
  boxVars ::  [VarRef],
  edgeVars ::  [VarRef],
  children :: [Noad]
  }

type NoadTrail = [Noad]
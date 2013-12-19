module Complex where
import Core (epsilon)
import Prelude hiding (negate)
import LinComb

data Complex = LinComb :+: LinComb

dtor x = x * (pi/ 180)

isConst :: Complex -> Bool
isConst (r :+: i) = isKnownComb r && isKnownComb i

constant :: Float -> Float -> Complex
constant r i = (mkConstComb r) :+: (mkConstComb i)

scalar :: Float -> Complex
scalar x = constant x 0.0

neg :: Complex -> Complex
neg (r :+: i) = (negate r :+: negate i)

add :: Complex -> Complex -> Complex
add (xr :+: xi) (yr :+: yi) = (xr |+| yr) :+: (xi |+| yi)

sub :: Complex -> Complex -> Complex
sub (xr :+: xi) (yr :+: yi) = (xr |-| yr) :+: (xi |-| yi)

div :: Complex -> Complex -> Complex
div (xr :+: xi) ([yr :*: Nothing] :+: [yi :*: Nothing]) =
  let modulus = yr * yr + yi * yi
  in if (modulus < epsilon * epsilon)
     then scalar 1.0 -- div by zero
     else (:+:) ((yr / modulus) |*| xr |-| ((-yi) / modulus) |*| xi)
                (((-yi) / modulus) |*| xr |+| (yr / modulus) |*| xi)

mul :: Complex -> Complex -> Maybe Complex
mul ([xr :*: Nothing] :+: [xi :*: Nothing]) (yr :+: yi) =
  Just $ (xr |*| yr  |-| xi |*| yi) :+: (xi |*| yr  |+| xr |*| yi)
mul _ _ = Nothing

realParts :: Complex -> Complex -> Complex
realParts (xr :+: _) (yr :+: _) = xr :+: yr

cis :: Complex -> Complex
cis ([a :*: Nothing] :+: _) = constant (cos . dtor $ a) (sin . dtor $ a)

abs :: Complex -> Complex
abs ([r :*: Nothing] :+: [i :*: Nothing]) =  scalar $ sqrt $ r**2 + i**2

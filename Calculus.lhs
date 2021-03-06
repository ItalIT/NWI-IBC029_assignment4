> {-# LANGUAGE UnicodeSyntax #-}
> module Calculus
> where
> import Unicode

> data Primitive
>   =  Sin  -- trigonometric: sine
>   |  Exp  -- exponential
>   deriving (Show)
>
> infixl 6 :+:
>
> data Function
>   =  Const Rational         -- constant function
>   |  Id                     -- identity
>   |  Prim Primitive         -- primitive function
>   |  Function :+: Function  -- addition of functions
>   deriving (Show)

infixl 7 :*:
infixr 9 :.:

apply    ∷ Function → (Double → Double)
derive   ∷ Function → Function
simplify ∷ Function → Function

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Jasper Haasdijk s4449754

exercise 6.1
============

exercise 6.2
============

exercise 6.3
============

exercise 6.4
============

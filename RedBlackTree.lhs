> {-# LANGUAGE UnicodeSyntax #-}
> module RedBlackTree
> where
> import Unicode
> import QuickTest

> data RedBlackTree elem
>   =  Leaf
>   |  Red    (RedBlackTree elem) elem (RedBlackTree elem)
>   |  Black  (RedBlackTree elem) elem (RedBlackTree elem)
>   deriving (Show)

> instance Functor RedBlackTree where
>   fmap _f (Leaf)         =  Leaf
>   fmap f  (Red l a r)    =  Red (fmap f l) (f a) (fmap f r)
>   fmap f  (Black l a r)  =  Black (fmap f l) (f a) (fmap f r)

member ∷ (Ord elem) ⇒ elem → RedBlackTree elem → Bool
insert ∷ (Ord elem) ⇒ elem → RedBlackTree elem → RedBlackTree elem
black ∷ RedBlackTree elem → elem → RedBlackTree elem → RedBlackTree elem
isRedBlackTree ∷ RedBlackTree elem → Bool
redBlackTrees ∷ [elem] → Probes (RedBlackTree elem)

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Jasper Haasdijk s4449754

exercise 5.1
============

> member :: (Ord elem) => elem -> RedBlackTree elem -> Bool
> member _e Leaf = False
> member e (Red l k r)
>     | e < k = member e l
>     | e == k = True
>     | e > k = member e r
> member e (Black l k r)
>     | e < k = member e l
>     | e == k = True
>     | e > k = member e r

exercise 5.2
============

I really, really do not get why this is not working but I used almost all 4
hours of the computer practicum and my whole afternoon to get to this point.

I asked several student assistants multiple times and noone could help me
resolve this.

I was told Red Black Trees in Haskell very easy. This exercise determines that
was a lie.

I used smart constructors as requested an went over everything 1330 times. I
even went to Ralf but he had no time to help me. It works up to a certain point
but breaks for higher depths.

> insert :: (Ord elem) => elem -> RedBlackTree elem -> RedBlackTree elem
> insert e = blacken . ins e
>     where ins e Leaf = Red Leaf e Leaf
>           ins e n@(Red l k r)
>               | e < k = Red (ins e l) k r
>               | e == k = n
>               | e > k = Red l k (ins e r)
>           ins e n@(Black l k r)
>               | e < k = black (ins e l) k r
>               | e == k = n
>               | e > k = black l k (ins e r)
>           blacken (Red l k r) = Black l k r
>           blacken t = t

> black :: RedBlackTree elem -> elem -> RedBlackTree elem -> RedBlackTree elem
> black (Red (Red v c w) b u) a t = Black (Red v c w) b (Red u a t)
> black (Red u b (Red v c w)) a t = Black (Red u b v) c (Red w a t)
> black t a (Red u b (Red v c w)) = Black (Red t a u) b (Red v c w)
> black t a (Red (Red v c w) b u) = Black (Red t a v) c (Red w b u)
> black l k r = Black l k r

exercise 5.3
============

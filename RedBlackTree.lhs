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

exercise 5.3
============

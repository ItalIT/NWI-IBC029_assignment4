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

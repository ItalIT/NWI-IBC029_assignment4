> {-# LANGUAGE UnicodeSyntax #-}
> module BinarySearchTree
> where
> import Unicode
> import BinaryTree hiding (member)
> import QuickTest

> registry  ∷  Tree String
> registry  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

member ∷ (Ord elem) ⇒ elem → Tree elem → Bool
insert ∷ (Ord elem) ⇒ elem → Tree elem → Tree elem
delete ∷ (Ord elem) ⇒ elem → Tree elem → Tree elem

isSearchTree ∷ (Ord elem) ⇒ Tree elem → Bool
trees ∷ [elem] → Probes (Tree elem)  -- should be defined in BinaryTree

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Jasper Haasdijk s4449754

exercise 4.1
============

> member :: (Ord elem) => elem -> Tree elem -> Bool
> member _e Empty = False
> member e (Node l k r) = case compare e k of
>                         EQ -> True
>                         LT -> member e l
>                         GT -> member e r

The difference between this exercise and 4.1.6 is that here we have an ordering
in the tree which potentially has a much higher runtime efficiency. At each step
we can decide whether to go left or right and don't have to do both.

exercise 4.2
============

> insert :: (Ord elem) => elem -> Tree elem -> Tree elem
> insert e Empty = Node Empty e Empty
> insert e (Node l k r)
>     | e < k || e == k = Node (insert e l) k r
>     | otherwise = Node l k (insert e r)

exercise 4.3
============

exercise 4.4
============

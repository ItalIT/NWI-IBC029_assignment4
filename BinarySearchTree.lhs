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
> member e Empty = False
> member e (Node l k r) = case compare e k of
>                         EQ -> True
>                         LT -> member e l
>                         GT -> member e r

exercise 4.2
============

exercise 4.3
============

exercise 4.4
============

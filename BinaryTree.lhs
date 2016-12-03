> {-# LANGUAGE UnicodeSyntax #-}
> module BinaryTree
> where
> import Unicode

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

> instance Functor Tree where
>   fmap _f Empty         =  Empty
>   fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

> ex1  ∷  Tree Integer
> ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
> ex2  ∷  Tree String
> ex2  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
> ex3  ∷  Tree Char
> ex3  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

size ∷ Tree elem → Int
minHeight, maxHeight ∷ Tree elem → Int
member ∷ (Eq elem) ⇒ elem → Tree elem → Bool
preorder, inorder, postorder ∷ Tree elem → [elem]
layout ∷ (Show elem) => Tree elem → String
build ∷ [elem] → Tree elem
balanced ∷ [elem] → Tree elem
create ∷ Int → Tree ()

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Jasper Haasdijk s4449754

exercise 1.1
============

This is the binary tree from figure 1.

> tree :: Tree Char
> tree = Node (
>            Node Empty 'a' (
>                Node Empty 'b' Empty
>            )
>        ) 'c' (
>            Node (
>                Node Empty 'd' Empty
>            ) 'f' (
>                Node Empty 'g' Empty
>            )
>        )

exercise 1.2
============

exercise 1.3
============

Tree design pattern

f :: Tree elem -> S
f Empty = ...
f (Node l k r) = ... l ... k ... r

> size :: Tree elem -> Int
> size Empty = 0
> size (Node l k r) = size l + 1 + size r

exercise 1.4
============

exercise 1.5
============

exercise 1.6
============

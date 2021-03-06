> {-# LANGUAGE UnicodeSyntax #-}
> module BinaryTree
> where
> import Unicode
> import Data.List

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

Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))

        / 42
    / 815
-4711

Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

-"Ralf"
    \ "Peter"
        \ "Frits"

Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

    / 'z'
-'k'
    \ 'a'

exercise 1.3
============

Tree design pattern

f :: Tree elem -> S
f Empty = ...
f (Node l k r) = ... l ... k ... r

> size :: Tree elem -> Int
> size Empty = 0
> size (Node l _k r) = size l + 1 + size r

exercise 1.4
============

> minHeight :: Tree elem -> Int
> minHeight Empty = 0
> minHeight (Node l _k r) = 1 + min (minHeight l) (minHeight r)

> maxHeight :: Tree elem -> Int
> maxHeight Empty = 0
> maxHeight (Node l _k r) = 1 + max (maxHeight l) (maxHeight r)

exercise 1.5
============

The size of the tree is greater or equal to the maximal height of a tree, which
is in turn greater or equal to the minimal height of a tree:
minHeight <= maxHeight <= size

exercise 1.6
============

> member :: (Eq elem) => elem -> Tree elem -> Bool
> member _e Empty = False
> member e (Node l k r) = e == k || member e l || member e r

exercise 2.1
============

> preorder :: Tree elem -> [elem]
> preorder Empty = []
> preorder (Node l k r) = k:preorder l ++ preorder r

> inorder :: Tree elem -> [elem]
> inorder Empty = []
> inorder (Node l k r) = inorder l ++ k:inorder r

> postorder :: Tree elem -> [elem]
> postorder Empty = []
> postorder (Node l k r) = postorder l ++ postorder r ++ [k]

exercise 2.2
============

> layout :: (Show elem) => Tree elem -> String  
> layout x = intercalate "\n" formattedLines
>     where formattedLines = map buildLine nodeInfos
>           nodeInfos = reverse $ inorderInfo x 0 Root
>           buildLine (i, d, c) = indentation d ++ childIndicator c ++ show i
>           indentation d = replicate (d * 4 + 1) ' '
>           childIndicator s = case s of
>                    RightChild -> "/ "
>                    LeftChild -> "\\ "
>                    Root -> "-"

> data ChildStatus = Root | LeftChild | RightChild
>     deriving (Show)

> type NodeInfos elem = [(elem, Int, ChildStatus)]

> inorderInfo :: Tree elem -> Int -> ChildStatus -> NodeInfos elem
> inorderInfo Empty _depth _child = []
> inorderInfo (Node l k r) depth child = leftInfo ++ (k, depth, child):rightInfo
>       where leftInfo = inorderInfo l (depth + 1) LeftChild
>             rightInfo = inorderInfo r (depth + 1) RightChild

exercise 3.1
============

This function builds really unbalanced trees but "inorder . build = id".

> build :: [elem] -> Tree elem
> build [] = Empty
> build (e:es) = Node Empty e (build es)

exercise 3.2
============

> balanced :: [elem] -> Tree elem
> balanced [] = Empty
> balanced elems = Node (balanced firstHalf) k (balanced secondHalf)
>                 where halfLength = (length elems) `div` 2
>                       firstHalf = take halfLength elems
>                       secondHalf = drop (halfLength + 1) elems
>                       k = head $ drop halfLength elems

exercise 3.3
============

Harry the Hacker is a quacksalver. It is literally impossible to construct a
tree more efficiently than O(n) since you need to visit each node (n nodes in
total) at least once and do O(1) work at each node.

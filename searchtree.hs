data SearchTree = Nil | SearchTree Int SearchTree SearchTree deriving Show

insert :: Int -> SearchTree -> SearchTree
insert n Nil = SearchTree n Nil Nil
insert n (SearchTree node left right) =
    if n < node then SearchTree node (insert n left) right
    else if n > node then SearchTree node left (insert n right)
    else SearchTree node left right

isElem :: Int -> SearchTree -> Bool
isElem _ Nil = False
isElem n (SearchTree node left right) =
    n == node || n < node && isElem n left || n > node && isElem n right


delete :: Int -> SearchTree -> SearchTree
delete _ Nil = Nil
delete n (SearchTree node Nil right) = right
delete n (SearchTree node (SearchTree lnode lleft lright) right) =
    if n < node then SearchTree node (delete n (SearchTree lnode lleft lright)) right
    else if n > node then SearchTree node (SearchTree lnode lleft lright) (delete n right)
    else SearchTree lnode lleft (delete lnode (SearchTree lnode lright right))
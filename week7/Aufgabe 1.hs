data Tree n l = Leaf l | Node (Tree n l) n (Tree n l)
  deriving Show

--Definition der Instanzen
instance Functor (Tree n) where
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l n r ) = Node (fmap f l) n (fmap f r)

instance Applicative (Tree n) where
  pure x = Leaf x
  (Leaf f) <*> t      = fmap f t
  (Node l n r) <*> t  = Node (l <*> t) n (r <*> t)

instance Monad (Tree n) where
  (Leaf x) >>= f = f x
  (Node l n r) >>= f = Node (l >>= f) n (r >>= f)

module Exercises.Week6 where

data Child = Naughty | Nice
--Aufgabe 2: Show-Instanzen müssen hinzugefügt werden 
    deriving Show

--muss mit Großbuchstaben starten und Konstruktor muss einen Namen haben.
data Wish a = Wish a
--Aufgabe 2: Show-Instanzen müssen hinzugefügt werden 
    deriving Show


--Present hat ein Argument a, welches fehlte. Deshalb wurde es ergänzt
data Present a = Box a | WrappingPaper (Present a)
--Aufgabe 2: Show-Instanzen müssen hinzugefügt werden 
    deriving Show


--In Functor wird ein Typkontruktor erwartet. Also keine Instanz vom Typ Present.
instance Functor Present where 
  fmap f (Box a)           = Box (f a)
  fmap f (WrappingPaper p) = WrappingPaper (fmap f p)

--Aufgabe 3: Es entsteht eine Endlosschleife, weil die Eq-Instanz durch x == Naughty auf sich selbst zugreift. Der folgende Code hat
-- dieses Problem nicht.
-- Es wird die Ausgabe [ geschrieben, weil Haskell in der Funktion santa die Liste c':cs durch den Konstruktor (:) zusammengefüg wird 
-- und dies wird zurückgegeben und angezeigt, bevor die Elemente der Liste berechnet werden. Danach folgt eine Endlosschleife und
-- das Programm terminiert nicht.  
instance Eq Child where
  Naughty == Naughty = True
  Nice == Nice = True
  _ == _ = False
--  x == y = if x == Naughty
--           then if y == Naughty then True else False
--           else if y == Naughty then False else True

--Falsche Syntax. Das Komma darf bei einem Funktionenaufruf dort nicht hin.
wrap :: Int -> a -> Present a
wrap 0 x = Box x
wrap n x = WrappingPaper (wrap(n - 1)  x)

--In der Definiton von santasLittleHelper wurde die Reihenfolge der Komponenten in der letzten Zeile vertauscht.
santa :: [(Child, Wish a)] -> [(Child, Maybe (Present a))]
santa xs = snd (foldr santasLittleHelper (0, []) xs)
  where santasLittleHelper = \(c, Wish x) (n,cs) ->
          let c' = if c == Naughty then (c, Nothing) else (c, Just (wrap n x))
          in (n + 1, c':cs)

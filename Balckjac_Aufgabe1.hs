data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving (Show)

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Show)

data Card = Card Suit Rank
  deriving (Show)

type Hand = [Card]

fullDeck :: Hand
fullDeck = 
    (getAllCardsForSuit Clubs) ++ (getAllCardsForSuit Spades) ++ (getAllCardsForSuit Hearts) ++ (getAllCardsForSuit Diamonds)
    where
        getAllCardsForSuit :: Suit -> Hand
        getAllCardsForSuit suit =
            (getAllNumberCards 10) ++ [ Card suit Jack, Card suit Queen, Card suit King, Card suit Ace]
            where 
                getAllNumberCards :: Int -> Hand
                getAllNumberCards n = if n < 2 then [] else (getAllNumberCards (n - 1)) ++ [ Card suit (Numeric n)]

numOfAces :: Hand -> Int
numOfAces [] = 0
numOfAces ((Card _ Ace): xs) = 1 + (numOfAces xs)
numOfAces (_: xs) = numOfAces xs

getCardValue :: Card -> Int
getCardValue (Card _ rank) =
    getRankValue rank
    where 
        getRankValue (Numeric n) = n
        getRankValue Ace = 11
        getRankValue _ = 10

getValue :: Hand -> Int
getValue hand = 
    let valueWith11 = go hand
    in if valueWith11 > 21 
        then valueWith11 - (numOfAces hand) * 10
        else valueWith11
    where 
        go [] = 0
        go (x: xs) = getCardValue x + go xs

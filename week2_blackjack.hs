data Suit = Diamond | Heart | Spades | Club deriving Show
data Rank = Number Int | Jack | Queen | King | Ace deriving Show
data Card = Card Suit Rank deriving Show
data Hand = Nil | Hand Card Hand deriving Show

getCardValue :: Card -> Int
getCardValue (Card _ rank) =
    getRankValue rank
    where 
        getRankValue (Number n) = n
        getRankValue Ace = 11
        getRankValue _ = 10

(<+>) :: Hand -> Hand -> Hand
(<+>) h1 Nil = h1
(<+>) Nil h2 = h2
(<+>) (Hand card resthand) h2 = Hand card (resthand <+> h2)

(<+>) :: Hand -> Card -> Hand
(<+>) :: hand card = hand <+> (Hand card Nil)

fullDeck :: Hand
fullDeck =
    getAllCardsWithSuit Diamond <+> g
    where getAllCardsWithSuit suit =
        getAllNumberCards 2 <+> (Card suit Jack) <+> (Card suit Queen) <+> (Card suit King) <+> (Card suit Ace)
        
import Data.Ratio
--Aufgabe 1
--infinite list of all fibonacci numbers
fibonacci :: [Integer]
fibonacci = fibgen 0 1
   where 
      fibgen :: Integer -> Integer -> [Integer]
      fibgen n1 n2 = n1 : fibgen n2 (n1 + n2)

--golden Ratio
goldenRatio :: [Rational]
goldenRatio = goldenRatioGen 0 
   where
      goldenRatioGen :: Int -> [Rational]
      goldenRatioGen n = 1 + ((fibonacci !! n) % (fibonacci !! (n+1))) : goldenRatioGen (n+1) 

-- returns an approximation for a given distance
approx :: Rational -> [Rational] -> Rational 
approx _ [] = 0
approx _ [_] = 0
approx eps (x:y:xs) = 
   if abs (x - y) <= eps 
   then y
   else approx eps (y:xs)

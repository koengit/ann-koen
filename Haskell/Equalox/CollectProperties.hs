module Equalox.CollectProperties where
 import Properties
 import Conjecture
 
getAxioms :: IO [String]
 getAxioms = do
	axioms <- filter (not.isComment) $ filter (/= null) (unlines $ readFile "properties.p")
	
isComment xs = head xs == '%'

getUnsatSubsets :: [String] -> [String]
getUnsatSubsets 

toConjecture :: String -> String
toConjecture [] = []
toConjecture ('a':'x':'i':'o':'m':xs) = "conjecture" ++ xs
toConjecture (x:xs) = x:(toConjecture xs)

chooseConjecture :: Int ->  [String] -> [String]
chooseConjecture k as = let (bs,(c:cs)) = splitAt k as in bs ++ (toConjecture c : cs)

falseConj :: String
falseConj = "fof(falseconj,conjecture, $false)."

getName :: String -> String
getName ('f':'o':'f':'(':xs) = takeWhile (/= ',') xs 




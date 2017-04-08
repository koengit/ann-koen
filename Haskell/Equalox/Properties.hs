module Equalox.Properties where

import Data.Maybe
import Data.List
import Form
import Debug.Trace
import Equalox.Implications

import Control.Monad

data Property = 
  Reflexive |  Transitive | Symmetric | Total | Irreflexive | Coreflexive | 
  AntiSymmetric | StrictlyTotal  | StrictlyAntiSymmetric | Functional | Serial2 | Impl RelationLit
  deriving (Eq, Ord)
  
allProperties = [Reflexive,Transitive,Symmetric,Total,
 Coreflexive,AntiSymmetric,Functional,Serial2]  

instance Show Property where
 show Reflexive     = "reflexive"
 show Transitive    = "transitive"
 show Symmetric     = "symmetric"
 show Total         = "total"
 show AntiSymmetric = "antisymmetric"
 show Functional    = "right_euclidean"
 show Serial2       = "serial"
--	show Irreflexive   = "reflexive_neg"
 show Irreflexive   = "irreflexive"
 show Coreflexive   = "coreflexive"
 show StrictlyTotal = "strictlytotal"
--        show StrictlyTotal = "antisymmetric_neg"
 show StrictlyAntiSymmetric = "strictlyantisymmetric"
 show (Impl r) =     "=> " ++ show r


read' "reflexive"         = Reflexive
read' "transitive"        = Transitive
read' "symmetric"         = Symmetric
read' "total"             = Total
read' "antisymmetric"     = AntiSymmetric
read' "right_euclidean"   = Functional
read' "serial"            = Serial2
read' "reflexive_neg"     = Irreflexive
read' "coreflexive"       = Coreflexive
read' "antisymmetric_neg" = StrictlyTotal 
read' "total_neg"         = StrictlyAntiSymmetric
read' x                   = error (show x)


convert2prop p 
   | isSuffixOf "_flip" p =
       if (isSuffixOf "_neg" (drop 5 p)) then (read' (dropLast 9 p),False,True)
        else (read' (drop 5 p),True,True)
   | otherwise =
       if (isSuffixOf "_neg" p) then (read' (dropLast 4 p),False,False)
        else (read' p,True,False)

dropLast n xs = (iterate init xs) !! n

showList' :: Show a => [a] -> String
showList' [] = ""
showList' [p] = show p
showList' [p1,p2] = show p1 ++ " and " ++ show p2
showList' (p1:ps) = show p1 ++ ", " ++ showList' ps

data RelationLit = Rel {rsymbol :: Symbol, rsign :: (Maybe Bool), dir :: (Maybe Bool)}  
-- Name, positive or negative/either, arguments flipped yes/no/either
  deriving (Eq)
  
instance Show RelationLit where
  show (Rel s mb1 mb2) = show s ++ show' mb1 ++ show'' mb2
show' (Just True)   = "+"
show' (Just False)  = "-"
show' Nothing       = "+-"
show'' (Just True)  = "<"
show'' (Just False) = ">"
show'' Nothing      = "><"

instance Ord RelationLit where
   (<=) r1 r2 = (show (rsymbol r1)) <= (show (rsymbol r2))

negr :: RelationLit -> RelationLit
negr rel = 
  case rel of 
    (Rel r Nothing mb)  ->  rel
    (Rel r (Just b) mb) ->  Rel r (Just (not b)) mb

flipr :: RelationLit -> RelationLit
flipr rel = 
  case rel of
   (Rel r mb Nothing)    -> rel
   (Rel r mb (Just b))   -> Rel r mb (Just (not b))

withProperty :: Property -> Clause -> Maybe RelationLit
withProperty Reflexive     = isReflexive
withProperty Irreflexive   = isIrreflexive
withProperty Functional    = isFunctional
withProperty Symmetric     = isSymmetric
withProperty Transitive    = isTransitive
withProperty Total         = isTotal
withProperty AntiSymmetric = isAntiSymmetric
withProperty Serial2       = isSerial
withProperty StrictlyAntiSymmetric = 
               \c -> case isTotal c of 
                       Just r  -> Just (negr r)
                       Nothing -> Nothing
withProperty StrictlyTotal = \c -> case isAntiSymmetric c of 
                                     Just r  -> Just (negr r)
                                     Nothing -> Nothing
withProperty Coreflexive = isCoreflexive

impliedBy :: Clause -> Maybe (RelationLit,RelationLit)
impliedBy [(Neg (Fun r [Var x,Var y] :=: t1)), (Pos (Fun s [Var x',Var y'] :=: t2))] = 
  if r /= s && x == x' && y == y' && t1 == t2 && t2 == truth then return (Rel r (Just True) (Just False), Rel s (Just True) (Just False))    
    else if (x == y' && y == x' && t1 == t2 && t2 == truth ) then return (Rel r (Just True) (Just False),Rel s (Just True) (Just True)) 
      else Nothing

impliedBy [Pos f, Neg f2] = impliedBy [Neg f2, Pos f]

impliedBy [(Neg (Fun r [Var x,Var y] :=: t1)), (Neg (Fun s [Var x',Var y'] :=: t2))] = 
  if r /= s && x == x' && y == y' && t1 == t2 && t2 == truth then return (Rel r (Just True) (Just False), Rel s (Just False) (Just False))
          else if (x == y' && y == x' && t1 == t2 && t2 == truth ) then return (Rel r (Just True) (Just False),Rel s (Just False) (Just True)) 
           else Nothing
 
impliedBy [(Pos (Fun r [Var x,Var y] :=: t1)), (Pos (Fun s [Var x',Var y'] :=: t2))] = 
  if r /= s && x == x' && y == y' && t1 == t2 && t2 == truth then return (Rel r (Just False) (Just False), Rel s (Just True) (Just False))
      else if (x == y' && y == x' && t1 == t2 && t2 == truth ) then return (Rel r (Just False) (Just True),Rel s (Just True) (Just True)) 
       else Nothing

impliedBy _ = Nothing

isReflexive :: Clause -> Maybe RelationLit
isReflexive [(Pos (Fun r [Var x,Var y] :=: t))] = 
 if x == y && t == truth then return (Rel r (Just True) Nothing) else Nothing
 
isReflexive [(Neg (Fun r [Var x,Var y] :=: t))] = 
 if x == y && t == truth then return (Rel r (Just False) Nothing) else Nothing
 
isReflexive [Neg (Var x :=: Var y), Pos ((Fun r [Var x', Var y']) :=: t)] = 
 if (t == truth && (x == x' && y == y') || (x == y' && y == x') ) && (x' /= y' && x /= y) 
         then  return $ (Rel r (Just True) Nothing) else Nothing
 
isReflexive [Neg (Var x :=: Var y), Neg ((Fun r [Var x', Var y']) :=: t)] = 
 if (t == truth && (x == x' && y == y') || (x == y' && y == x') ) && (x' /= y' && x /= y)
       then return $ (Rel r (Just False) Nothing) else Nothing

isReflexive [Pos a, Neg b] = isReflexive [Neg b, Pos a]

isReflexive _ = Nothing


isCoreflexive :: Clause -> Maybe RelationLit
isCoreflexive [(Neg (Fun r [Var x,Var y] :=: t)), Pos (Var x' :=: Var y') ] =
  if (t == truth && (x == x' && y == y') || (x == y' && y == x') ) && (x' /= y' && x /= y)
    then Just (Rel r (Just True) Nothing) else Nothing
  
isCoreflexive [(Pos (Fun r [Var x,Var y] :=: t)), Pos (Var x' :=: Var y') ] =
   if (t == truth && (x == x' && y == y') || (x == y' && y == x') ) && (x' /= y' && x /= y)
     then Just (Rel r (Just False) Nothing) else Nothing

isCoreflexive [Pos a, Neg a'] = isCoreflexive [Neg a',Pos a]
isCoreflexive _ = Nothing 


isIrreflexive c = case isReflexive c of
   Nothing -> Nothing
   Just r  -> Just (negr r) 

isSymmetric :: Clause -> Maybe RelationLit
isSymmetric [Pos (Fun r [Var x,Var y] :=: t), Neg (Fun r' [Var y',Var x'] :=: t')] =
 if (r == r' && t == truth && t' == truth && x == x' && y == y' && (x /= y && y' /= x')) 
  then Just (Rel r Nothing Nothing) else Nothing
  
isSymmetric [Neg a, Pos a'] = isSymmetric [Pos a', Neg a]
isSymmetric _ = Nothing

---

isFunctional :: Clause -> Maybe RelationLit
isFunctional [a,b,c] = listToMaybe $ mapMaybe isFunctional' (permutations [a,b,c])
isFunctional _ = Nothing

-- pos flipped and not flipped
isFunctional'
 [Pos (Fun r1 [Var y,Var z] :=: t1), 
 Neg (Fun r2 [Var x,Var y'] :=: t2) , Neg (Fun r3  [Var x',Var z'] :=: t3)] = 
     if r1 == r2 && r2 == r3 && t2 == truth && t3 == truth && t1 == truth then 
      (if x == x' && y == y' && z == z' && (x /= y && x' /= z') then 
         Just (Rel r2 (Just True) (Just False)) 
       else 
        (if y' == z' && z == x && x' == y && (x /= y && x' /= z') then 
          Just (Rel r1 (Just True) (Just True)) 
         else Nothing))
       else Nothing
    
-- negative flipped and not flipped   
isFunctional' [Neg (Fun r1 [Var y,Var z] :=: t1), Pos (Fun r2 [Var x,Var y'] :=: t2) , Pos (Fun r3  [Var x',Var z'] :=: t3)] = 
   if r1 == r2 && r2 == r3 && t2 == truth && t3 == truth && t1 == truth then 
     if x == x' && y == y' && z == z' && (y/=z && x /= y' && x' /= z') then 
       Just (Rel r2 (Just False) (Just False)) 
     else 
      if y' == z' && x == z && x' == y && (y/=z && x /= y' && x' /= z')then 
        Just (Rel r2 (Just False) (Just True)) 
       else Nothing
   else Nothing    

isFunctional' _ = Nothing

--Still total with flipped arguments
isTotal :: Clause -> Maybe RelationLit
isTotal [Pos (Fun r [Var x,Var y] :=: t), Pos (Fun r' [Var y',Var x'] :=: t')] =
  if (r == r' && t == truth && t' == truth) then  
    if (x == x' && y == y') && (x /=y && y' /= x') then (Just (Rel r (Just True) Nothing)) else Nothing
   else Nothing
    
isTotal [Neg (Fun r [Var x,Var y] :=: t), Neg (Fun r' [Var y',Var x'] :=: t')] =
  if (r == r' && t == truth && t' == truth) then 
    if (x == x' && y == y') && (x/=y && y'/=x') then (Just (Rel r (Just False) Nothing)) else Nothing
   else Nothing

isTotal _ = Nothing

---

isAntiSymmetric :: Clause -> Maybe RelationLit
isAntiSymmetric [a,b,c] = listToMaybe $ catMaybes $ map isAntiSymmetric' (permutations [a,b,c])
isAntiSymmetric _ = Nothing

-- ~R(x,y) | ~R(y,x) | x=y
isAntiSymmetric' :: Clause -> Maybe RelationLit
isAntiSymmetric' [Pos (Var x :=: Var y), Neg (Fun r [Var x',Var y'] :=: t),Neg (Fun r' [Var y'',Var x''] :=: t')] =
   if r == r' && t == t' && t' == truth then 
     if x' == x'' && y' == y'' && x == x' && y == y' &&
      (x /= y && x' /= y' && y'' /= x'') then (Just  (Rel r (Just True) Nothing))
      else Nothing
    else Nothing

-- r(X,Y) | r(Y,X) | X = Y
isAntiSymmetric' [Pos (Var x :=: Var y), Pos (Fun r [Var x',Var y'] :=: t),Pos (Fun r' [Var y'',Var x''] :=: t')] =
  if r == r' && t == t' && t' == truth then 
    if x' == x'' && y' == y'' && x == x' && y == y' then (Just (Rel r (Just False) Nothing))
     else Nothing
   else Nothing

isAntiSymmetric' _ = Nothing

---

isTransitive :: Clause -> Maybe RelationLit
isTransitive [a,b,c] = listToMaybe $ mapMaybe isTransitive' (permutations [a,b,c])
isTransitive _ = Nothing
isTransitive' [Pos (Fun r1 [Var x,Var z] :=: t1), Neg (Fun r2 [Var x',Var y] :=: t2) , Neg (Fun r3  [Var y',Var z'] :=: t3)] = 
 if r1 == r2 && r2 == r3 && t1 == truth && t2 == truth && t3 == truth then
   if x == x' && y == y' && z == z' &&
      (x /= z && x' /= y && y' /= z') then (Just (Rel r1 (Just True) Nothing)) else 
       if z == y && z' == x' && x == y' &&
         (x /= z && x' /= y && y' /= z') then (Just (Rel r1 (Just True) Nothing))
        else Nothing
    else Nothing

isTransitive' [Neg (Fun r1 [Var x,Var z] :=: t1), Pos (Fun r2 [Var x',Var y] :=: t2) , Pos (Fun r3  [Var y',Var z'] :=: t3)] = 
 if r1 == r2 && r2 == r3 && t1 == truth && t2 == truth && t3 == truth then
   if x == x' && y == y' && z == z' &&
    (x /= z && x' /= y && y' /= z') then (Just (Rel r1 (Just False) Nothing)) else 
      if z == y && z' == x' && x == y' && (x /= z && x' /= y && y' /= z') then (Just (Rel r1 (Just False) Nothing)) 
      else Nothing
    else Nothing
isTransitive' _ = Nothing

---

isSerial :: Clause -> Maybe RelationLit
isSerial [  Pos ((Fun r [Var x, Fun f [Var x']]) :=: t) ] = 
  if x == x' && t == truth then (Just (Rel r (Just True) (Just False))) else Nothing

isSerial [  Pos ((Fun r [Fun f [Var x'], Var x]) :=: t) ] = 
  if x == x' && t == truth then (Just (Rel r (Just True) (Just True))) else Nothing

isSerial [  Neg ((Fun r [Var x, Fun f [Var x']]) :=: t) ] = 
  if x == x' && t == truth then (Just (Rel r (Just True) (Just False))) else Nothing

isSerial [  Neg ((Fun r [Fun f [Var x'], Var x]) :=: t) ] = 
  if x == x' && t == truth then (Just (Rel r (Just False) (Just True))) else Nothing

isSerial _ = Nothing

collectProperties' :: Clause -> [(RelationLit,Property)]
collectProperties' c = case collectProperties c of
   [] -> []
   [(Rel r mb1 mb2,p)] -> [((Rel r a b),p) | a <- boolcases mb1, b <- boolcases mb2]
 where boolcases (Just b) = [(Just b)]
       boolcases Nothing  = [Just True,Just False]
                        

collectProperties :: Clause -> [(RelationLit,Property)]
collectProperties c = fromMaybe [] $ 
       
   do {r <- isReflexive c; 
     return [(r,Reflexive) ]} 
      `mplus`
      do {r <- isCoreflexive c; 
         return [(r,Coreflexive) ] }
        `mplus`
        do {r <- isSymmetric c; 
         return [(r, Symmetric)]} 
        `mplus`
        do {r <- isTransitive c; 
        return [(r, Transitive)]} 
        `mplus`
        do {r <- isTotal c;  
        return [(r, Total)]} 
        `mplus`
        do {r <- isAntiSymmetric c; 
        return [(r, AntiSymmetric)]} 
        `mplus`
        do {r <- isFunctional c; 
        return [(r, Functional)]} 
        `mplus`
        do {r <- isSerial c; 
        return [(r,Serial2)]} 
        `mplus` 
        do {(r1,r2) <- impliedBy c;
        return [(r1, Impl r2)]}

-- Creates a map of properties for each relationlit				
sortByFirst :: Eq a => [(a,b)] -> [(a,[b])]
--[(RelationLit,Property)] -> [(RelationLit,[Property])]
sortByFirst [] = []
sortByFirst ((rel,p):xs) = 
  let 
    rs  = (rel,p):[(rel',prop) | (rel',prop) <- xs, rel' == rel]
    rs2 = (rel, [prop | (_,prop) <- rs])
  in (rs2 : sortByFirst [(s,p') | (s,p') <- xs, s /= rel])
 

sameR :: RelationLit -> RelationLit -> Bool
sameR (Rel r _ _) (Rel r1 _ _) = r == r1

---

isNegatedP :: String -> Bool
isNegatedP s = "_neg" `isInfixOf` s

isFlippedP :: String -> Bool
isFlippedP s = "_flip" `isInfixOf` s

isNegated :: RelationLit -> Bool
isNegated (Rel _ (Just False) _) = True
isNegated _ = False

isFlipped :: RelationLit -> Bool
isFlipped (Rel _ _ (Just True)) = True
isFlipped _ = False


hasProperty :: RelationLit -> Property -> [(RelationLit,[Property])]  -> Bool

hasProperty r StrictlyTotal ps          = hasProperty (negr r) AntiSymmetric ps 
hasProperty r StrictlyAntiSymmetric ps  = hasProperty (negr r) Total ps 
hasProperty r Irreflexive ps            = hasProperty (negr r) Reflexive ps

hasProperty r prop rps = 
  trace ("hasProperty " ++ show r ++ "  " ++ show prop ++ " in " ++ show [  impls | (p,impls) <- implications, p == show prop] ++ "\n\n" ++ "?" ++ "\n" ++
    "isGiven: " ++ show (isGiven r prop rps) ++ "\n" ++ "given from implication table: " ++ show  (or $ map (areGiven r rps)
         [  impls | (p,impls) <- implications, p == show prop]) ++ "\n\n" ++ unlines  (map show [  (impls, areGiven r rps impls) | (p,impls) <- implications, p == show prop]))
	     $
  isGiven r prop rps || 
 -- trace ("checking if " ++ show prop ++ " is implied for " ++ show r)
	 (or $ map (areGiven r rps) [  impls | (p,impls) <- implications, p == show prop])


isGiven r prop rps = case lookup' r rps of
            Just ps -> --trace ("looked up " ++ show r ++ "  " ++ show prop ++ "  " ++ show ps ++ " " ++ show (elem prop ps)) $ 
	                elem prop ps
            Nothing -> False

lookup' r [] = Nothing
lookup' r ((r',xs):rs) = 
   if r =**= r' then Just (xs ++ case lookup' r rs of 
                    Just ys -> ys
                    Nothing -> [])
                 else lookup' r rs

--todo should not be symmetric.			   
(=**=) :: RelationLit -> RelationLit -> Bool 
-- checks if the name of the relation is the same, and the directions and signs are not opposite.
(Rel r1 mb1 mb2) =**= (Rel r2 mb1' mb2') = r1 == r2 && (mb1 == mb1' || mb1' == Nothing || mb1 == Nothing )-- || mb1' == Nothing || mb1 == Nothing)
     && (mb2 == mb2' ||  mb2' == Nothing || mb2 == Nothing)

      
areGiven r rps ss = -- trace ("areGiven: " ++ show r ++ "  " ++ show ss ++ "\n\n") $
  let f s = if isNegatedP s then negr else id
      f' s = if isFlippedP s then flipR else id 
      toProp = read'.dropNeg.dropFlip in 
       and [ --  (trace ( (show ( ((f' s) . (f s)) r)) ++ "    " ++ (show (toProp s)) ++ "     " ++ show (isGiven ( ((f' s) . (f s)) r) (toProp s) rps)) 
               (isGiven ( ((f' s) . (f s)) r) (toProp s) rps) | s <- ss]

negaR :: RelationLit -> RelationLit
negaR (Rel r (Just b1) b2) =  Rel r (Just (not b1)) b2
negaR r = r

flipR :: RelationLit -> RelationLit
flipR (Rel r b1 (Just b2))  = (Rel r b1 (Just (not b2)))
flipR r = r  

 
dropNeg ('_':'n':'e':'g':xs) = xs
dropNeg (x:xs) = (x:dropNeg xs)
dropNeg [] = []

dropFlip ('_':'f':'l':'i':'p':xs) = xs
dropFlip (x:xs) = (x:dropFlip xs)
dropFlip [] = []


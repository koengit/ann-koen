module Equalox.Properties where
	
import Data.Maybe
import Data.List
import Form	
import Debug.Trace

import Control.Monad
	
data Property = 
  Reflexive |  Transitive | Symmetric | Total | Irreflexive | Coreflexive | AntiSymmetric | 
  StrictlyTotal  | StrictlyAntiSymmetric | Functional | Serial2
  deriving (Eq)
  
issymmetric :: Property -> Bool
-- what properties are preserved when flipped
issymmetric p = 
   let symmetrylist = [Transitive,Reflexive,Coreflexive,Symmetric,
                       Irreflexive,Total,AntiSymmetric] 
   in elem p symmetrylist

--what properties are preserved when negated
isneutral Symmetric = True
isneutral _ = False
  
instance Show Property where
	show Reflexive     = "reflexive"
	show Transitive    = "transitive"
	show Symmetric     = "symmetric"
	show Total         = "total"
	show AntiSymmetric = "antisymmetric"
	show Functional    = "right_euclidean"
	show Serial2       = "serial"
	show Irreflexive   = "irreflexive"
	show Coreflexive   = "coreflexive"
	show StrictlyTotal = "strictlytotal"
	show StrictlyAntiSymmetric = "strictlyantisymmetric"
  
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

negr :: RelationLit -> RelationLit
negr rel = 
	case rel of 
		(Rel r Nothing mb)  ->  rel
		(Rel r (Just b) mb) ->  Rel r (Just (not b)) mb	
		
flipr :: RelationLit -> RelationLit
flipr rel = 
	case rel of
	  (Rel r mb Nothing)    -> rel
	  (Rel r mb (Just b))	-> Rel r mb (Just (not b))

withProperty :: Property -> Clause -> Maybe RelationLit
withProperty Reflexive     = isReflexive
withProperty Irreflexive   = isIrreflexive
withProperty Functional    = isFunctional
withProperty Symmetric     = isSymmetric
withProperty Transitive	   = isTransitive
withProperty Total         = isTotal
withProperty AntiSymmetric = isAntiSymmetric
withProperty Serial2       = isSerial
withProperty StrictlyAntiSymmetric = \c -> case isTotal c of 
	                                Just r  -> Just (negr r)
					Nothing -> Nothing
withProperty StrictlyTotal = \c -> case isAntiSymmetric c of 
	                                Just r  -> Just (negr r)
					Nothing -> Nothing
withProperty Coreflexive = isCoreflexive
--Coreflexive
--StrictlyAntiSymmetric
--StrictlyTotal
	
---
	
isReflexive :: Clause -> Maybe RelationLit
isReflexive [(Pos (Fun r [Var x,Var y] :=: t))] = 
 if x == y && t == truth then return (Rel r (Just True) Nothing) else Nothing
 
isReflexive [(Neg (Fun r [Var x,Var y] :=: t))] = 
 if x == y && t == truth then return (Rel r (Just False) Nothing) else Nothing
isReflexive _ = Nothing

---

isCoreflexive :: Clause -> Maybe RelationLit
isCoreflexive [(Neg (Fun r [Var x,Var y] :=: t)), Pos (Var x' :=: Var y') ] =
  if (t == truth && (x == x' && y == y') || (x == y' && y == x') ) 
    then Just (Rel r (Just True) Nothing) else Nothing
  
isCoreflexive [(Pos (Fun r [Var x,Var y] :=: t)), Pos (Var x' :=: Var y') ] =
   if (t == truth && (x == x' && y == y') || (x == y' && y == x') ) 
     then Just (Rel r (Just False) Nothing) else Nothing

isCoreflexive [Pos a, Neg a'] = isCoreflexive [Neg a',Pos a]
isCoreflexive _ = Nothing 

---

isIrreflexive :: Clause -> Maybe RelationLit
isIrreflexive c = case isReflexive c of
	 Nothing -> Nothing
         Just r  -> Just (negr r) 

---
	 
isSymmetric :: Clause -> Maybe RelationLit
isSymmetric [Pos (Fun r [Var x,Var y] :=: t), Neg (Fun r' [Var y',Var x'] :=: t')] =
 if (r == r' && t == truth && t' == truth && x == x' && y == y') 
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
	   (if x == x' && y == y' && z == z' then 
	       Just (Rel r2 (Just True) (Just False)) 
	     else 
		 (if y' == z' && z == x && x' == y then 
		   Just (Rel r1 (Just True) (Just True)) 
		   else Nothing))
	    else Nothing
	    
-- negative flipped and not flipped   
isFunctional' [Neg (Fun r1 [Var y,Var z] :=: t1), Pos (Fun r2 [Var x,Var y'] :=: t2) , Pos (Fun r3  [Var x',Var z'] :=: t3)] = 
	 if r1 == r2 && r2 == r3 && t2 == truth && t3 == truth && t1 == truth then 
	   if x == x' && y == y' && z == z' then 
	       Just (Rel r2 (Just False) (Just False)) 
	     else 
		if y' == z' && x == z && x' == y then 
		  Just (Rel r2 (Just False) (Just True)) 
		    else Nothing
	  else Nothing    
	    	    
isFunctional' _ = Nothing

--Still total with flipped arguments
isTotal :: Clause -> Maybe RelationLit
isTotal [Pos (Fun r [Var x,Var y] :=: t), Pos (Fun r' [Var y',Var x'] :=: t')] =
  if (r == r' && t == truth && t' == truth) then  
    if (x == x' && y == y') then (Just (Rel r (Just True) Nothing)) else Nothing
   else Nothing
    
isTotal [Neg (Fun r [Var x,Var y] :=: t), Neg (Fun r' [Var y',Var x'] :=: t')] =
  if (r == r' && t == truth && t' == truth) then 
    if (x == x' && y == y') then (Just (Rel r (Just False) Nothing)) else Nothing
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
	  if x' == x'' && y' == y'' && x == x' && y == y' then (Just  (Rel r (Just True) Nothing))
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
	 if x == x' && y == y' && z == z' then (Just (Rel r1 (Just True) Nothing)) else 
		 if z == y && z' == x' && x == y' then (Just (Rel r1 (Just True) Nothing	)) 
		 	else Nothing
	 else Nothing

isTransitive' [Neg (Fun r1 [Var x,Var z] :=: t1), Pos (Fun r2 [Var x',Var y] :=: t2) , Pos (Fun r3  [Var y',Var z'] :=: t3)] = 
 if r1 == r2 && r2 == r3 && t1 == truth && t2 == truth && t3 == truth then
	 if x == x' && y == y' && z == z' then (Just (Rel r1 (Just False) Nothing)) else 
		 if z == y && z' == x' && x == y' then (Just (Rel r1 (Just False) Nothing)) 
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

---

--collects pairs of relations and properties from a given clause						
collectProperties :: Clause -> [(RelationLit,Property)]
collectProperties c = fromMaybe [] $ 
	do {r <- isReflexive c; 
	     return [(r,Reflexive) ]} 
	`mplus`
	do {r <- isCoreflexive c; 
	    return [(r,Coreflexive) ] }
	`mplus`
	do {r <- isSymmetric c; 
	    return [(r,Symmetric)]} 
	`mplus`
	do {r <- isTransitive c; 
	    return [(r,Transitive)]} 
	    `mplus`
	do {r <- isTotal c;  
	     return [(r,Total)]} 
        `mplus`
	do {r <- isAntiSymmetric c; 
	    return [(r,AntiSymmetric)]} 
	`mplus`
	do {r <- isFunctional c; 
	   return [(r,Functional)]} 
	`mplus`
	do {r <- isSerial c; 
	   return [(r,Serial2)]}
	   
-- Creates a map of properties for each relationlit				
sortByFirst :: Eq a => [(a,b)] -> [(a,[b])]
--[(RelationLit,Property)] -> [(RelationLit,[Property])]
sortByFirst [] = []
sortByFirst ((rel,p):xs) = 
  let 
    rs 	= (rel,p):[(rel',prop) | (rel',prop) <- xs, rel' == rel]
    rs2 = (rel, [prop | (_,prop) <- rs])
  in (rs2 : sortByFirst [(s,p') | (s,p') <- xs, s /= rel])
 

sameR :: RelationLit -> RelationLit -> Bool
sameR (Rel r _ _) (Rel r1 _ _) = r == r1

areGiven :: RelationLit -> [(RelationLit,[Property])] -> [String] -> Bool
areGiven r rps props = let ans = all (isGiven r (filter ((sameR r).fst) rps)) props in
  ans

isGiven :: RelationLit -> [(RelationLit,[Property])] -> String -> Bool
isGiven r rps prop = 
	--TODO if the property is symmetric we want to consider both (r and flipr r) ?
		
  let (newr,fixProp) = case (isNegatedP prop,isFlippedP prop) of
	(False,False) -> (r,id)
	(True,False)  -> (negr r, dropNeg)    
	(False,True)  -> (flipr r,dropFlip)
	(True,True)   -> (flipr (negr r), dropNeg.dropFlip) in	
   case lookup' newr rps of
          Just ps -> (elem (fixProp prop) $ map show ps)
          Nothing -> False


dropNeg ('_':'n':'e':'g':xs) = xs
dropNeg (x:xs) = (x:dropNeg xs)
dropNeg [] = []

dropFlip ('_':'f':'l':'i':'p':xs) = xs
dropFlip (x:xs) = (x:dropFlip xs)
dropFlip [] = []
	
lookup' r [] = Nothing
lookup' r ((r',xs):rs) = 
   if r =**= r' then Just (xs ++ case lookup' r rs of 
	                            Just ys -> ys
				    Nothing -> [])
	                   else lookup' r rs
			   
(=**=) :: RelationLit -> RelationLit -> Bool 
-- checks if the name of the relation is the same, and the directions and signs are not opposite.
(Rel r1 mb1 mb2) =**= (Rel r2 mb1' mb2') = r1 == r2 && (mb1 == mb1' || mb1' == Nothing || mb1 == Nothing)
     && (mb2 == mb2' || mb2 == Nothing || mb2' == Nothing)

isNegatedP :: String -> Bool
isNegatedP s = "_neg" `isInfixOf` s

isFlippedP :: String -> Bool
isFlippedP s = "_flip" `isInfixOf` s

isNegated :: RelationLit -> Bool
isNegated (Rel _ (Just True) _) = False
isNegated _ = True

isFlipped :: RelationLit -> Bool
isFlipped (Rel _ _ (Just False)) = False
isFlipped _ = True

											
hasProperty :: RelationLit -> Property -> [(RelationLit,[Property])] -> Bool
hasProperty r StrictlyTotal ps         = hasProperty (negr r) AntiSymmetric ps
hasProperty r StrictlyAntiSymmetric ps = hasProperty (negr r) Total ps
hasProperty r Irreflexive ps           = hasProperty (negr r) Reflexive ps
hasProperty r prop ps = case lookup (show prop) (sortByFirst implications) of
	Just impls -> let allimpls = [(show prop)] : impls in  or $ map (areGiven r ps) allimpls
			
isReflTrans :: [(RelationLit,[Property])] -> RelationLit -> Bool
isReflTrans cs s = hasProperty s Reflexive cs && hasProperty s Transitive cs


isEquivalenceRelation :: [(RelationLit,[Property])] -> RelationLit -> Bool
isEquivalenceRelation sps s = hasProperty s Reflexive sps && 	  
			      (hasProperty s Transitive sps &&
			      hasProperty s Symmetric sps)


isPartialEquivalence :: [(RelationLit,[Property])] -> RelationLit -> Bool
isPartialEquivalence sps s = 
			 (hasProperty s Transitive sps &&
			 hasProperty s Symmetric sps) && (not (hasProperty s Reflexive sps))

isTotalOrder :: [(RelationLit,[Property])] -> RelationLit -> Bool
isTotalOrder sps r@(Rel s (Just b) mb) = (hasProperty r Total sps &&
					 hasProperty r Transitive sps &&
					 hasProperty r AntiSymmetric sps)


isStrictTotalOrder :: [(RelationLit,[Property])] -> RelationLit -> Bool
isStrictTotalOrder sps r =  hasProperty r StrictlyTotal sps &&
			    hasProperty r Transitive sps &&
			    hasProperty r StrictlyAntiSymmetric sps
			    

implications =  [
        ("transitive",["total","symmetric"]),
  
        ("transitive",["symmetric","total_neg"]),
  
        ("transitive",["antisymmetric","coreflexive_neg"]),
  
        ("transitive",["coreflexive"]),
  
        ("transitive",["transitive_neg","right_euclidean"]),
  
        ("transitive",["reflexive","right_euclidean_neg"]),
  
        ("transitive",["reflexive_neg","right_euclidean"]),
  
        ("transitive",["antisymmetric","transitive_neg"]),
  
        ("transitive",["total","right_euclidean_neg"]),
  
        ("transitive",["reflexive","coreflexive_neg"]),
  
        ("transitive",["total_neg","coreflexive_neg"]),
  
        ("transitive",["total_neg","right_euclidean"]),
  
        ("transitive",["symmetric","antisymmetric"]),
  
        ("transitive",["symmetric","right_euclidean"]),
  
        ("transitive",["reflexive","symmetric","antisymmetric_neg"]),
  
        ("transitive",["right_euclidean","right_euclidean_neg"]),
  
        ("transitive",["total","right_euclidean"]),
  
        ("transitive",["total","coreflexive_neg"]),
  
        ("transitive",["total_neg","right_euclidean_neg"]),
  
        ("transitive",["total_neg","transitive_neg"]),
  
        ("transitive",["reflexive","right_euclidean_neg_flip"]),
  
        ("transitive",["symmetric","right_euclidean_flip"]),
  
        ("transitive",["reflexive","right_euclidean"]),
  
        ("transitive",["antisymmetric","right_euclidean_neg"]),
  
        ("transitive",["antisymmetric","right_euclidean"]),
  
        ("transitive",["total_neg","right_euclidean_flip"]),
  
        ("transitive",["coreflexive_neg","right_euclidean_flip"]),
  
        ("transitive",["reflexive","right_euclidean_flip"]),
  
        ("transitive",["total","right_euclidean_flip"]),
  
        ("transitive",["serial_flip","right_euclidean"]),
  
        ("transitive",["total","right_euclidean_neg_flip"]),
  
        ("transitive",["total_neg","right_euclidean_neg_flip"]),
  
        ("transitive",["coreflexive_neg","right_euclidean"]),
  
        ("transitive",["right_euclidean","antisymmetric_neg"]),
  
        ("transitive",["right_euclidean","right_euclidean_flip"]),
  
        ("transitive",["reflexive","symmetric","transitive_neg"]),
  
        ("transitive",["antisymmetric","right_euclidean_neg_flip"]),
  
        ("transitive",["transitive_neg","right_euclidean_flip"]),
  
        ("transitive",["antisymmetric","right_euclidean_flip"]),
  
        ("transitive",["serial","right_euclidean_flip"]),
  
        ("transitive",["antisymmetric_neg","right_euclidean_flip"]),
  
        ("transitive",["right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("transitive",["reflexive_neg","right_euclidean_flip"]),
  
        ("transitive",["right_euclidean","right_euclidean_neg_flip"]),
  
        ("transitive",["right_euclidean_neg","right_euclidean_flip"]),
  
        ("reflexive",["serial","antisymmetric","coreflexive_neg"]),
  
        ("reflexive",["antisymmetric","serial_flip","coreflexive_neg"]),
  
        ("reflexive",["total"]),
  
        ("reflexive",["transitive","serial_flip","right_euclidean_neg"]),
  
        ("reflexive",["serial","coreflexive"]),
  
        ("reflexive",["serial_flip","right_euclidean"]),
  
        ("reflexive",["symmetric","transitive","serial_flip"]),
  
        ("reflexive",["symmetric","serial_flip","right_euclidean_flip"]),
  
        ("reflexive",["serial","symmetric","right_euclidean"]),
  
        ("reflexive",["serial","right_euclidean_flip"]),
  
        ("reflexive",["serial","symmetric","transitive"]),
  
        ("reflexive",["transitive","serial_flip","coreflexive_neg"]),
  
        ("reflexive",["coreflexive","serial_flip"]),
  
        ("reflexive",["serial","symmetric","antisymmetric"]),
  
        ("reflexive",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("reflexive",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("reflexive",["symmetric","antisymmetric","serial_flip"]),
  
        ("reflexive",["serial_flip","right_euclidean_neg","right_euclidean_flip"]),
  
        ("reflexive",["antisymmetric","serial_flip","right_euclidean_neg"]),
  
        ("reflexive",["serial","transitive","coreflexive_neg"]),
  
        ("reflexive",["serial","right_euclidean","right_euclidean_neg_flip"]),
  
        ("reflexive",["serial","coreflexive_neg","right_euclidean"]),
  
        ("reflexive",["serial_flip","coreflexive_neg","right_euclidean_flip"]),
  
        ("symmetric",["transitive","serial_flip","right_euclidean_neg"]),
  
        ("symmetric",["coreflexive"]),
  
        ("symmetric",["reflexive_neg","right_euclidean"]),
  
        ("symmetric",["reflexive_neg","right_euclidean_flip"]),
  
        ("symmetric",["coreflexive_neg"]),
  
        ("symmetric",["reflexive","right_euclidean_neg"]),
  
        ("symmetric",["reflexive","right_euclidean_neg_flip"]),
  
        ("symmetric",["reflexive","right_euclidean"]),
  
        ("symmetric",["total_neg","right_euclidean"]),
  
        ("symmetric",["transitive_neg","right_euclidean","serial_neg_flip"]),
  
        ("symmetric",["reflexive_neg","right_euclidean_neg"]),
  
        ("symmetric",["total_neg","right_euclidean_neg"]),
  
        ("symmetric",["total","right_euclidean_neg"]),
  
        ("symmetric",["total","right_euclidean"]),
  
        ("symmetric",["total","right_euclidean_flip"]),
  
        ("symmetric",["total","right_euclidean_neg_flip"]),
  
        ("symmetric",["serial_flip","right_euclidean"]),
  
        ("symmetric",["total_neg","right_euclidean_flip"]),
  
        ("symmetric",["reflexive_neg","right_euclidean_neg_flip"]),
  
        ("symmetric",["right_euclidean","right_euclidean_neg_flip"]),
  
        ("symmetric",["reflexive","right_euclidean_flip"]),
  
        ("symmetric",["right_euclidean_neg","right_euclidean_flip"]),
  
        ("symmetric",["right_euclidean","right_euclidean_flip"]),
  
        ("symmetric",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("symmetric",["serial_neg","right_euclidean_neg_flip"]),
  
        ("symmetric",["right_euclidean_neg","right_euclidean_neg_flip"]),
  
        ("symmetric",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("symmetric",["antisymmetric","serial_flip","right_euclidean_neg"]),
  
        ("symmetric",["serial_neg_flip","right_euclidean_neg"]),
  
        ("symmetric",["serial","right_euclidean_flip"]),
  
        ("symmetric",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("symmetric",["total_neg","right_euclidean_neg_flip"]),
  
        ("symmetric",["right_euclidean","antisymmetric_neg","serial_neg_flip"]),
  
        ("symmetric",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["symmetric","total_neg"]),
  
        ("right_euclidean",["total","symmetric"]),
  
        ("right_euclidean",["antisymmetric","coreflexive_neg"]),
  
        ("right_euclidean",["transitive","right_euclidean_neg"]),
  
        ("right_euclidean",["reflexive","right_euclidean_neg"]),
  
        ("right_euclidean",["reflexive","coreflexive_neg"]),
  
        ("right_euclidean",["total_neg","right_euclidean_neg"]),
  
        ("right_euclidean",["coreflexive"]),
  
        ("right_euclidean",["reflexive_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["total","coreflexive_neg"]),
  
        ("right_euclidean",["total","right_euclidean_neg"]),
  
        ("right_euclidean",["transitive","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["reflexive","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["transitive","coreflexive_neg"]),
  
        ("right_euclidean",["total_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["serial_neg","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["symmetric","antisymmetric"]),
  
        ("right_euclidean",["total","right_euclidean_flip"]),
  
        ("right_euclidean",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["transitive","serial_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["total_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["antisymmetric","right_euclidean_neg"]),
  
        ("right_euclidean",["symmetric","transitive"]),
  
        ("right_euclidean",["total","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["reflexive","symmetric","antisymmetric_neg"]),
  
        ("right_euclidean",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["reflexive","right_euclidean_flip"]),
  
        ("right_euclidean",["right_euclidean_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["total_neg","coreflexive_neg"]),
  
        ("right_euclidean",["antisymmetric","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["antisymmetric","serial_neg","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("right_euclidean",["reflexive","symmetric","transitive_neg"]),
  
        ("right_euclidean",["serial","right_euclidean_flip"]),
  
        ("right_euclidean",["symmetric","right_euclidean_flip"]),
  
        ("right_euclidean",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("right_euclidean",["coreflexive_neg","right_euclidean_flip"]),
  
        ("coreflexive",["antisymmetric","coreflexive_neg"]),
  
        ("coreflexive",["symmetric","total_neg"]),
  
        ("coreflexive",["transitive","reflexive_neg","right_euclidean_neg"]),
  
        ("coreflexive",["reflexive","antisymmetric","right_euclidean_neg"]),
  
        ("coreflexive",["symmetric","antisymmetric"]),
  
        ("coreflexive",["reflexive_neg","right_euclidean"]),
  
        ("coreflexive",["reflexive_neg","right_euclidean_flip"]),
  
        ("coreflexive",["total","antisymmetric","right_euclidean"]),
  
        ("coreflexive",["reflexive","antisymmetric","right_euclidean"]),
  
        ("coreflexive",["reflexive","antisymmetric","right_euclidean_flip"]),
  
        ("coreflexive",["transitive_neg","right_euclidean","serial_neg_flip"]),
  
        ("coreflexive",["total_neg","right_euclidean"]),
  
        ("coreflexive",["total","antisymmetric","right_euclidean_neg"]),
  
        ("coreflexive",["total","antisymmetric","right_euclidean_flip"]),
  
        ("coreflexive",["reflexive","antisymmetric","right_euclidean_neg_flip"]),
  
        ("coreflexive",["serial_neg","coreflexive_neg","right_euclidean"]),
  
        ("coreflexive",["serial_neg","coreflexive_neg","right_euclidean_flip"]),
  
        ("coreflexive",["coreflexive_neg","right_euclidean","serial_neg_flip"]),
  
        ("coreflexive",["transitive","serial_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive",["coreflexive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive",["total_neg","coreflexive_neg"]),
  
        ("coreflexive",["antisymmetric","right_euclidean","right_euclidean_neg_flip"]),
  
        ("coreflexive",["antisymmetric","serial_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive",["antisymmetric","serial_flip","right_euclidean"]),
  
        ("coreflexive",["total_neg","right_euclidean_flip"]),
  
        ("coreflexive",["total","antisymmetric","right_euclidean_neg_flip"]),
  
        ("coreflexive",["antisymmetric","right_euclidean_neg","right_euclidean_flip"]),
  
        ("coreflexive",["serial_neg","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","antisymmetric_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive",["transitive","serial_neg","coreflexive_neg"]),
  
        ("coreflexive",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("coreflexive",["antisymmetric","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","reflexive_neg"]),
  
        ("coreflexive",["antisymmetric","right_euclidean","right_euclidean_flip"]),
  
        ("coreflexive",["symmetric","transitive","serial_neg","right_euclidean_neg"]),
  
        ("coreflexive",["transitive","reflexive_neg","coreflexive_neg"]),
  
        ("coreflexive",["transitive","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["antisymmetric","reflexive_neg","right_euclidean_neg"]),
  
        ("coreflexive",["total_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["total_neg","right_euclidean_neg"]),
  
        ("coreflexive",["antisymmetric","right_euclidean_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("coreflexive",["serial","antisymmetric","right_euclidean_flip"]),
  
        ("coreflexive",["serial_neg_flip","right_euclidean_neg","right_euclidean_flip"]),
  
        ("coreflexive",["transitive","coreflexive_neg","serial_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","serial_neg","antisymmetric_neg"]),
  
        ("coreflexive",["serial_neg","right_euclidean","right_euclidean_neg_flip"]),
  
        ("coreflexive",["right_euclidean","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive",["right_euclidean","antisymmetric_neg","serial_neg_flip"]),
  
        ("coreflexive",["symmetric","serial_neg","transitive_neg","right_euclidean"]),
  
        ("coreflexive",["symmetric","serial_neg","right_euclidean","antisymmetric_neg"]),
  
        ("coreflexive",["symmetric","serial_neg","right_euclidean","right_euclidean_neg"]),
  
        ("coreflexive",["transitive","serial_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","serial_neg","transitive_neg"]),
  
        ("coreflexive",["symmetric","transitive","antisymmetric_neg","serial_neg_flip"]),
  
        ("coreflexive",["symmetric","serial_neg_flip","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("coreflexive",["symmetric","transitive","transitive_neg","serial_neg_flip"]),
  
        ("coreflexive",["antisymmetric","serial_neg","right_euclidean_neg_flip"]),
  
        ("coreflexive",["antisymmetric","serial_flip","right_euclidean_neg"]),
  
        ("coreflexive",["symmetric","transitive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("coreflexive",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("coreflexive",["right_euclidean","serial_neg_flip","right_euclidean_neg"]),
  
        ("coreflexive",["serial_neg","right_euclidean_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["total_neg"]),
  
        ("antisymmetric",["coreflexive"]),
  
        ("antisymmetric",["reflexive_neg","right_euclidean"]),
  
        ("antisymmetric",["coreflexive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric",["symmetric","transitive","serial_neg","right_euclidean_neg"]),
  
        ("antisymmetric",["transitive","serial_neg_flip","right_euclidean_neg"]),
  
        ("antisymmetric",["transitive","reflexive_neg"]),
  
        ("antisymmetric",["reflexive_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["serial_neg","coreflexive_neg","right_euclidean"]),
  
        ("antisymmetric",["serial_neg","coreflexive_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["transitive_neg","right_euclidean","serial_neg_flip"]),
  
        ("antisymmetric",["transitive","serial_neg","coreflexive_neg"]),
  
        ("antisymmetric",["symmetric","transitive","serial_neg","antisymmetric_neg"]),
  
        ("antisymmetric",["coreflexive_neg","right_euclidean","serial_neg_flip"]),
  
        ("antisymmetric",["transitive","coreflexive_neg","serial_neg_flip"]),
  
        ("antisymmetric",["symmetric","serial_neg","right_euclidean","antisymmetric_neg"]),
  
        ("antisymmetric",["symmetric","serial_neg","transitive_neg","right_euclidean"]),
  
        ("antisymmetric",["serial_neg","right_euclidean","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["transitive","serial_neg","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["serial_neg_flip","right_euclidean_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["symmetric","transitive","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["symmetric","serial_neg_flip","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["serial_neg","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["right_euclidean","antisymmetric_neg","serial_neg_flip"]),
  
        ("antisymmetric",["symmetric","transitive","antisymmetric_neg","serial_neg_flip"]),
  
        ("antisymmetric",["symmetric","antisymmetric_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric",["right_euclidean","serial_neg_flip","right_euclidean_neg_flip"]),
  
        ("antisymmetric",["symmetric","transitive","transitive_neg","serial_neg_flip"]),
  
        ("antisymmetric",["symmetric","transitive","serial_neg","transitive_neg"]),
  
        ("antisymmetric",["serial_neg","right_euclidean_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["symmetric","serial_neg","right_euclidean","right_euclidean_neg"]),
  
        ("antisymmetric",["right_euclidean","serial_neg_flip","right_euclidean_neg"]),
  
        ("antisymmetric",["symmetric","transitive_neg","serial_neg_flip","right_euclidean_flip"]),
  
        ("antisymmetric",["serial_neg","antisymmetric_neg","right_euclidean_flip"]),
  
        ("antisymmetric",["serial_neg","transitive_neg","right_euclidean_flip"]),
  
        ("total",["serial","coreflexive","coreflexive_neg"]),
  
        ("total",["serial","antisymmetric","coreflexive_neg"]),
  
        ("total",["coreflexive","serial_flip","right_euclidean_neg_flip"]),
  
        ("total",["reflexive","coreflexive_neg"]),
  
        ("total",["coreflexive","serial_flip","coreflexive_neg"]),
  
        ("total",["serial","coreflexive","antisymmetric_neg"]),
  
        ("total",["serial","symmetric","antisymmetric","antisymmetric_neg"]),
  
        ("total",["antisymmetric","serial_flip","coreflexive_neg"]),
  
        ("total",["reflexive","antisymmetric_neg"]),
  
        ("total",["reflexive","right_euclidean_neg"]),
  
        ("total",["serial_flip","coreflexive_neg","right_euclidean"]),
  
        ("total",["symmetric","antisymmetric","serial_flip","right_euclidean_neg_flip"]),
  
        ("total",["transitive","serial_flip","right_euclidean_neg"]),
  
        ("total",["reflexive","right_euclidean_neg_flip"]),
  
        ("total",["reflexive","transitive_neg"]),
  
        ("total",["serial","coreflexive","right_euclidean_neg"]),
  
        ("total",["serial","coreflexive","right_euclidean_neg_flip"]),
  
        ("total",["coreflexive","serial_flip","antisymmetric_neg"]),
  
        ("total",["serial_flip","transitive_neg","right_euclidean"]),
  
        ("total",["serial","coreflexive_neg","right_euclidean_flip"]),
  
        ("total",["serial","antisymmetric_neg","right_euclidean_flip"]),
  
        ("total",["symmetric","antisymmetric","serial_flip","antisymmetric_neg"]),
  
        ("total",["symmetric","transitive","serial_flip","antisymmetric_neg"]),
  
        ("total",["serial","symmetric","antisymmetric","transitive_neg"]),
  
        ("total",["symmetric","serial_flip","antisymmetric_neg","right_euclidean_flip"]),
  
        ("total",["serial","symmetric","transitive","right_euclidean_neg"]),
  
        ("total",["serial","symmetric","transitive_neg","right_euclidean"]),
  
        ("total",["serial_flip","right_euclidean","right_euclidean_neg"]),
  
        ("total",["transitive","serial_flip","coreflexive_neg"]),
  
        ("total",["serial","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("total",["symmetric","transitive","serial_flip","transitive_neg"]),
  
        ("total",["serial","coreflexive","transitive_neg"]),
  
        ("total",["coreflexive","serial_flip","transitive_neg"]),
  
        ("total",["serial","symmetric","antisymmetric","right_euclidean_neg"]),
  
        ("total",["serial_flip","right_euclidean","right_euclidean_neg_flip"]),
  
        ("total",["antisymmetric","serial_flip","right_euclidean_neg"]),
  
        ("total",["serial","symmetric","right_euclidean","antisymmetric_neg"]),
  
        ("total",["serial","symmetric","transitive","antisymmetric_neg"]),
  
        ("total",["serial","transitive","coreflexive_neg"]),
  
        ("total",["serial_flip","right_euclidean","antisymmetric_neg"]),
  
        ("total",["serial","antisymmetric","right_euclidean_neg_flip"]),
  
        ("total",["serial","transitive_neg","right_euclidean_flip"]),
  
        ("total",["serial","right_euclidean_neg","right_euclidean_flip"]),
  
        ("total",["serial_flip","right_euclidean_neg","right_euclidean_flip"]),
  
        ("total",["symmetric","antisymmetric","serial_flip","transitive_neg"]),
  
        ("total",["symmetric","serial_flip","right_euclidean_flip","right_euclidean_neg_flip"]),
  
        ("total",["serial","symmetric","right_euclidean","right_euclidean_neg"]),
  
        ("total",["serial","transitive","right_euclidean_neg_flip"]),
  
        ("total",["coreflexive","serial_flip","right_euclidean_neg"]),
  
        ("total",["symmetric","transitive","serial_flip","right_euclidean_neg_flip"]),
  
        ("total",["serial","symmetric","transitive","transitive_neg"]),
  
        ("total",["serial","coreflexive_neg","right_euclidean"]),
  
        ("total",["serial_flip","coreflexive_neg","right_euclidean_flip"]),
  
        ("total",["symmetric","serial_flip","transitive_neg","right_euclidean_flip"]),
  
        ("total",["serial","right_euclidean","right_euclidean_neg_flip"]),
  
        ("serial",["reflexive"]),
  
        ("serial",["total"]),
  
        ("serial",["serial_flip","coreflexive_neg"]),
  
        ("serial",["coreflexive","serial_flip"]),
  
        ("serial",["symmetric","serial_flip"]),
  
        ("serial",["serial_flip","right_euclidean"]),
  
        ("serial",["serial_flip","right_euclidean_neg"]),
  
        ("serial",["serial_flip","reflexive_neg","right_euclidean_neg_flip"]),
  
        ("serial",["serial_neg","serial_flip","right_euclidean_neg_flip"])]
	
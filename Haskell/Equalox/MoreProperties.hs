module Equalox.MoreProperties where
	
import Equalox.Properties
import qualified Data.Set as S
import Name
import Form
import Data.Ord
import Data.List (sort)


testa :: Clause -> Clause
testa c = sort c

litOrder :: Atom -> Atom -> Ordering
litOrder l1@(t1 :=: t2) l2@(t3 :=: t4) = 
	if l1 == l2 || isEquality l1 && isEquality l2 then EQ
	  else if isEquality l1 then EQ else if isEquality l2 then GT 
	   else case (t1,t2) of
		   (Fun f xs, Fun f2 xs2) -> compare (length xs) (length xs2)
		   _                      -> error "litOrder"
		   
--toNormalForm - sortera pos och neg literaler, ordna minst -> störst
-- räkna unika variabelförekomster

isEquality (t1 :=: t2) = t2 /= truth

relevantClause :: Clause -> Bool
relevantClause cs = has2VarRelation cs && S.size (free cs) == 2 || S.size (free cs) == 3 

has2VarRelation [] = False
has2VarRelation (l:xs) = case the l of 
  f@(Fun r xs' :=: t)  -> if S.size (free f) == 2 && t == truth then True else has2VarRelation xs
  _              -> has2VarRelation xs

isTransitive' [Pos (Fun r1 [Var x,Var z] :=: t1), Neg (Fun r2 [Var x',Var y] :=: t2) , Neg (Fun r3  [Var y',Var z'] :=: t3)] = 
 if r1 == r2 && r2 == r3 && t1 == truth && t2 == truth && t3 == truth then
	 if x == x' && y == y' && z == z' then (Just (Rel r1 (Just True) Nothing)) else 
		 if z == y && z' == x' && x == y' then (Just (Rel r1 (Just True) Nothing	)) 
		 	else Nothing
	 else Nothing
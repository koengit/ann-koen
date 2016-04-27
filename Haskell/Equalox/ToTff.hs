module Equalox.ToTff where
	
import Form
import Infinox.Conjecture
import qualified Data.Set as S


clauses2tff :: [Clause] -> Bool -> String
clauses2tff cs b = clauses2tff' cs 0 b


clauses2tff'  [] _  _ = ""

clauses2tff' (x:xs) n b  = clause2tff  x n b ++ "\n" ++ clauses2tff' xs  (n+1) b

clause2tff ::  Clause -> Int -> Bool ->  String
clause2tff f n _ = 
	"tff(" ++ "a_" ++ (show n) ++ "," ++ "axiom," ++ showVars f ++ showClause f ++ ")."
	
showVars :: Clause -> String
showVars c = let xs =  S.toList $ free c  in
  if xs == [] then "" else "![" ++ showVars' xs ++ "]: "
  
showVars' [x] = show x ++":$i"  
showVars' (x:xs) = show x ++ ":$i, " ++ showVars' xs
	
			
--clauses2cnfs :: [Clause] -> String
--clauses2cnfs fs = clauses2cnfs' fs 0
	
--clauses2cnfs' [] n = ""
--clauses2cnfs' (x:xs) n = clause2cnf x n ++ "\n" ++  clauses2cnfs' xs (n+1)



	
--showClause :: Clause -> String
--showClause [] s = "$false"
--showClause c s = show (foldr1 (\/) ([ if not (sameSymb a s) then (Atom a) else showLit (Atom a) | Pos a <- c] ++ [ Not (Atom a) | Neg a <- c ]))
-- where sameSymb ((Fun (s' ::: _) ts) :=: _) s = show s' == s
  --     sameSymb _ _ = False
    --   showLit ((Fun (s' ::: _) ts) :=: _) =  (show (ts!!0)) ++ show s' ++ (show (ts!!1))
	
{-showTFF ls = let vars = free ls in
  quantify (S.toList vars) ++ "(" ++ showClause' ls ">=" ++ ")"

showClause' :: Clause -> String -> String
showClause' [] s = "$false"
showClause' c s = show (foldr1 (\/) ([ if not (sameSymb a s) then (Atom a) else showLit (Atom a) | Pos a <- c] ++ [ Not (Atom a) | Neg a <- c ]))
 where sameSymb ((Fun (s' ::: _) ts) :=: _) s = show s' == s
       sameSymb _ _ = False
       showLit ((Fun (s' ::: _) ts) :=: _) =  (show (ts!!0)) ++ show s' ++ (show (ts!!1))
 -}

--quantify [] = ""
--quantify (x:xs) = "![" ++ show x ++ " :  $i"  ++ quantify' xs ++ "] : "

--quantify' [] = ""
--quantify' ((nam ::: _):xs) = ", " ++ show nam ++ " : " ++ "$i" 

typeDecl :: Symbol -> String
typeDecl (nam ::: typ) | head (show nam) == '$' = ""
                       | otherwise = "tff(" ++ show nam ++ "_type, type, " ++ show nam ++ " : " ++ showtyp_ nam typ ++ ")."

showtyp_ nam t@([t1] :-> t2) | take 4 (show nam) == "rep_" = "$i > $real"
                             | otherwise = showtyp t
showtyp_ nam t = showtyp t 


showtyp :: Typing -> String
showtyp (V t) = "$i"
 
showtyp ([] :-> t2) = if (t2 == bool) then "$o" else "$i"
showtyp (ts :-> t2)  = "(" ++ showtyp' ts ++ ") > " ++ if (t2 == bool) then "$o" else "$i"

showtyp' [] = ""
showtyp' (x:xs) = case xs of 
  [] -> "$i"
  _  -> "$i" ++ " * " ++  showtyp' xs 



--isPredSymbol :: Symbol -> Bool
--isPredSymbol s@(_ ::: (_ :-> t)) = t == bool
--isPredSymbol _                   = False
  
	

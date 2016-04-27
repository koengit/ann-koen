{-# LANGUAGE BangPatterns #-}
module Equalox.Equalify where

import System.IO

import Equalox.Properties hiding (show')
--import Equalox.MoreProperties
import Equalox.ToTff
import Form 
import Flags
import Data.Maybe
import Data.Either

import Name
import qualified Data.Set as S
import Data.List
import Infinox.Conjecture (noClashString,clause2cnf)
import Debug.Trace
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import System.FilePath
import Control.Monad
import System.Directory

------- Data type definitions -------------------------------------------------


hasRelitC :: RelationLit -> Clause ->  Int
hasRelitC r c  =  length $ (filter ( == True)) ([hasRelit r l | l <- c] )
 

hasRelit :: RelationLit -> Signed Atom ->   Bool
hasRelit r (Pos ((Fun r2 _) :=: _)) = 
  if rsymbol r /= r2 then False else fromJust $ rsign r
  
hasRelit r (Neg ((Fun r2 _) :=: _)) = 
  if rsymbol r /= r2 then False else rsign r == Just False
  
hasRelit _ _ = False
  
flipIfContainsR :: RelationLit -> Signed Atom -> Signed Atom
flipIfContainsR r sa = case the sa of
	((Fun r2 _) :=: _) -> if rsymbol r == r2 then negat sa else sa
	_          -> sa



data RelationTransformation = 
  Equivalence {relit :: RelationLit, newsymbol :: Symbol} | 
  PartialEquivalence {relit :: RelationLit, partial_pred :: Symbol, newsymbol :: Symbol} | 
  TransRef {relit :: RelationLit, newsymbol :: Symbol, newvar :: String} |
  TotalOrder {relit :: RelationLit, newsymbol ::  Symbol, ordersymbol :: Symbol} |
  StrictTotalOrder {relit :: RelationLit, newsymbol :: Symbol, ordersymbol :: Symbol}
  deriving (Eq,Show)
  
data PropertyMap =  
  PMap {eqrels            :: [RelationLit],
        peqrels           :: [RelationLit],
	transrels         :: [RelationLit],
	totalorders       :: [RelationLit],
	stricttotalorders :: [RelationLit],
	allproperties     :: [(RelationLit,[Property])]
	}

data ClauseOrComment = Comment String | Cl Clause
	deriving (Eq)
	
instance Show ClauseOrComment where
	show (Comment s) = "%% " ++ s
	show (Cl c)      = showClause c
	
type ClausesOrComments = [ClauseOrComment]

clausesOrComments2format :: Ify -> ClausesOrComments -> Int -> [String]
clausesOrComments2format _ [] _  =  []
clausesOrComments2format f ((Cl c):cs) n = 
   (if f == Ordify || f == Tordify then clause2tff else clause2cnf)  c n : clausesOrComments2format f cs (n+1)
clausesOrComments2format f (c:cs) n = (show c) : (clausesOrComments2format f cs n)
		
-------------------------------------------------------------------------------
	

equalify :: (?flags :: Flags) => [Clause] -> [Clause] -> IO ClauseAnswer
equalify theory oblig = do
	
 let 
  tf      = (Flags.thisFile ?flags)
  ify     = (Flags.ify ?flags)
  outdir' = (Flags.dir ?flags)
  outdir  = case outdir' of
	     Nothing -> ""
	     Just d  -> d ++ "/"
  outfile = outdir ++(takeBaseName tf) ++ "_" ++ show ify ++ (takeExtension tf) 
  cs      = theory ++ oblig 
  fs      = map toForm cs
  noClash = noClashString fs  
  pmap    = concatMap collectProperties cs
  wmap    = filter ((== Functional).snd) $ nub pmap
  tmap    = filter ((== Transitive).snd) $ nub pmap
  
 createDirectoryIfMissing False outdir
 let propmap  = (findEquivalenceRelations cs)   
    
 let 
 
  (method,rels) = (case ify of
    Flags.Equalify  -> (equalifyClauses,transformRels ify noClash 0 ((eqrels propmap)))
    Flags.PEqualify -> (equalifyClauses,transformRels ify noClash 0 ((peqrels propmap)))
    Flags.Transify  -> (transifyClauses, transformRels ify noClash 0 ((transrels propmap)))
    Flags.Ordify    -> (ordify, transformRels ify noClash 0 (totalorders propmap))
    Flags.Tordify   -> (tordify, transformRels ify noClash 0 (stricttotalorders propmap))) 
  wonkyrs = [r | (r,p) <- wmap, elem'' r (eqrels propmap)]
  trs     = [r | (r,p) <- tmap, elem'' r (eqrels propmap)]
  
--  trs2    = [r | (r,p) <- tmap]
--  wonkyrs2 = [r | (r,p) <- wmap]
--  wonkrs  = wonkyrs \\ trs
  sameSign r1 r2 = case (relit r1,relit r2) of
	 ((Rel s mb1 mb2),(Rel s2 mb1' mb2')) ->  s == s2 && mb1 == mb1' || mb1 == Nothing || mb1' == Nothing
	
  nubbed xs = nubBy sameSign xs -- this is OK because for equalify,pequalify, the order does not matter.
  in                            -- for transify, ordify, tordify, the two directions should not occur in the same list in the first place.
  do 
 -- putStrLn (show rels)
  
  if rels /= [] then do
	  
     let --cs' = map (map (flipIfContainsR (head (totalorders propmap)))) cs
         es = method (map Cl cs) (nubbed rels)
         ss = transformationComments (nubbed rels)  
--	 tffs = clauses2tff  (map toClause (filter isClause es))
	 symbs = filter (not.isVarSymbol) $ S.toList $ symbols cs
	 newsymbs = [(name ("rep_" ++ noClash ++ show k) ::: ([top]:-> top)) 
	              | k <- [0..((length (nubbed rels)) -1)]]
	 decls = if ify == Ordify || ify == Tordify then Just (map typeDecl (symbs++newsymbs)) else Nothing
    -- putStrLn $ show 
   --  writeProblemToFile outfile Nothing (map Cl cs') Equalify []	 
      
--         ans = [(r,sum (map (hasRelitC r) cs)) | r <- totalorders propmap]
--	 ans2 = [((negr r),sum (map (hasRelitC (negr r)) cs)) | r <- totalorders propmap]
--	 ans3 = if length ans > 2 then error "?" else snd $ head ans
--	 ans4 = snd $ head ans2
 --    putStrLn $ show (map length cs)
 --    putStrLn $ show (length cs)
--     putStrLn $ show tf
--     putStrLn $ (show ans3) 
--     putStrLn $ (show ans4)
--     addToFile tf ans3 ans4
--     putStrLn $ unlines decls
--     putStrLn $ tffs
  --   writeProblemToFile outfile decls (map Cl cs) Ordify ss  
     writeProblemToFile outfile decls es ify ss  
     let --a = sortByFirst $ concatMap collectProperties cs 
         --b = [(r',prop')| (r',prop') <- a, elem'' r' (eqrels propmap) ]
    -- appendFile "hej" (tf ++ "   " ++ show (length wonkyrs) ++ "   " ++ show (length trs) ++ "\n")
   --  putStrLn $ show pmap
     putStrLn $ tf ++ "   " ++ show (length  (eqrels propmap) `div` 2) ++ 
      "   " ++ show (length wmap) ++ "   " ++ show (length tmap)  ++ "   " ++ show (length wonkyrs) ++ "   " ++ show (length trs) ++ "\n"
      -- ++
      -- show (eqrels propmap) ++ "\n\n" ++ show wmap ++ "\n\n " ++ show tmap ++ "\n\n" ++ show wonkyrs ++ "\n\n" ++ show trs 
     
     
     --appendFile "propmaps" $ unlines $ map (++"\n") (map show (map collectProperties cs))
   --  appendFile "test_" (show pmap)
  else return ()
 return $ NoAnswerClause NA
  
elem' r [] = False  
elem' r (x:xs) = r =*= x || elem' r  xs

elem'' r [] = False
elem'' rel@(Rel r m1 m2) ((Rel r2 m1' m2'):xs) = r == r2 || elem'' rel xs
  
  
  
--addToFile tf ans1 ans2 = do
--  appendFile "statistics" (tf ++ "\n" ++ show ans1 ++ "\n" ++ show ans2 ++ "\n")
  
  
--printStatistics :: String -> Int -> Int
--printStatistics file tos stos = putStrLn $  

printPropMap :: PropertyMap -> IO ()
printPropMap pm = do 
  mPutStrLn "Equivalence Relations: "  (eqrels pm)
  mPutStrLn "Partial Equivalence Relations: " (peqrels pm)
  mPutStrLn "Total Orders: " (totalorders pm)
  mPutStrLn "Strict Total Orders: " (stricttotalorders pm)
  mPutStrLn "Transitive+Reflexive: " (transrels pm)
  mPutStrLn "All Properties: " (allproperties pm)
  
mPutStrLn :: (Show a) => String -> [a] -> IO () 
mPutStrLn s1 s2 = if null s2 then return () else putStrLn $ s1 ++ show s2

isClause :: ClauseOrComment -> Bool
isClause (Cl c ) = True
isClause _ = False

toClause (Cl c) = c
 
 
writeProblemToFile :: FilePath -> Maybe [String] -> ClausesOrComments ->  Ify -> String -> IO ()
writeProblemToFile f decls cs ify ss = 
 do
   putStrLn ("Writing to file " ++ show f)
   h <- openFile f WriteMode
   hSetBuffering h LineBuffering
   hPutStrLn h $ ss
   case decls of 
	 Nothing -> return ()
	 Just s  -> hPutStrLn h $ unlines s
   hPutStr h (unlines $ clausesOrComments2format ify cs 1)
   hClose h
	
transformationComments = unlines.(map transformationComment)

transformationComment :: RelationTransformation -> String
transformationComment (Equivalence r ns) =
 -- let hs = case rsign r of 
	  --  Just False -> "Negated"
	  --  _          -> ""
-- in
 "%%  equivalence relation " ++ show (rsymbol r) ++
  " replaced by rep-function " ++ (show ns)
  
transformationComment (PartialEquivalence r p ns) =  
 --let hs = --case rsign r of 
	  --  Just False -> "Negated"
	  --  _          -> ""
-- in
 "%%  partial equivalence relation " ++ show (rsymbol r) ++
  " replaced by rep-function " ++ (show ns) ++ " and predicate " ++ show p
  
transformationComment t@(TransRef r ns nv) = 
--   let hs = case rsign r of
--	     Just False -> "Negated"
--	     _          -> ""
  -- in 
   "%%  transitive and reflexive relation " ++ show (rsymbol r) ++
    " replaced by new relation " ++ (show ns)
    
transformationComment (TotalOrder r newr ord) = 
   "%% Total order " ++ show (rsymbol r) ++ " replaced by rep function " ++ show newr
   
transformationComment (StrictTotalOrder r newr ord) = 
      "%% Strict Total order " ++ show (rsymbol r) ++ " replaced by rep function " ++ show newr
  
------------------------------------------------------------------------------- 

-- Syntactically identify what symbols represent equivalence relations or 
-- trans/reflexive relations 
-- returns:     1. equivalence relations, 
--              2. Transitive+symmetric but not reflexive relations,
--              3. Transitive+reflexive but not symmetric relations, 
--              4. total orders, 
--              5. strict total orders, 
--              6. The list of all pairs of found symbols and properties.
findEquivalenceRelations :: [Clause] -> PropertyMap

findEquivalenceRelations cs = do
  let ss  = S.toList $ symbols cs 
      ss' = filter isRelation ss
      relits s = [Rel s b1 b2 | 
                  b1 <- [Just True, Just False], b2 <- [Just True, Just False]]      
      rls    = concatMap relits ss'
    
      cs'    = sortByFirst $ concatMap collectProperties cs 
      eqrls     = filter (isEquivalenceRelation cs')  rls
      transrels = filter (isReflTrans cs') ((rls) \\ eqrls)
      tos       = filter (isTotalOrder cs') rls
      stos      = filter (isStrictTotalOrder cs') rls
      peqrls    = filter (isPartialEquivalence cs') (((rls)\\eqrls))
      
      cs'' = sortByFirst [ (r,prop) | (r,ps) <- cs' , prop <- allprops, hasProperty r prop cs' ]
   

  

   in  PMap eqrls peqrls transrels tos stos cs''
   
 
allprops = [Coreflexive,Reflexive,Transitive,Symmetric,Total,Irreflexive,AntiSymmetric,
            StrictlyTotal,StrictlyAntiSymmetric,Functional,Serial2]


isRelation s = isPredSymbol s && arity s == 2

transformRels :: Flags.Ify -> String -> Int -> [RelationLit] -> [RelationTransformation]
transformRels ify noClash k [] = []
transformRels ify noClash k (r:rs) = 
	(transformRel ify noClash k r) : (transformRels ify noClash (k+1) rs)

transformRel :: Flags.Ify -> String -> Int -> RelationLit -> RelationTransformation
transformRel ify noClash k r@(Rel s mb1 mb2)  =   
  let newsymbol = (name ("rep_" ++ noClash ++ show k)) ::: ([top]:-> top)
      newp      = (name ("p" ++ noClash ++ show k)) ::: ([top]:-> top)
      newvar    = ("X" ++ noClash ++ show k)
      ordersymbol1 = (name "<=" ::: ([top,top]:-> bool))
      ordersymbol2 = (name ">" :::  ([top,top]:-> bool))
  in
   case ify of 
       Flags.Equalify  -> Equivalence r newsymbol
       Flags.PEqualify -> PartialEquivalence r newp newsymbol  
       Flags.Transify  -> TransRef r newsymbol newvar
       Flags.Ordify    -> TotalOrder r newsymbol ordersymbol1
       Flags.Tordify   -> StrictTotalOrder r newsymbol ordersymbol2


----------  "equalifying" all clauses with the given relation symbols  ----------

--equalifyClauses' cs rels = equalifyClauses cs (nubBy sameRel rels)

equalifyClauses :: ClausesOrComments -> [RelationTransformation] -> ClausesOrComments
equalifyClauses cs [] = cs 
equalifyClauses cs (r:ss) = equalifyClauses cs' ss
 where cs' = (onClauses (equalifyOrRemoveClause r) cs) 


onClauses :: (Clause -> Either [Clause] String) -> ClausesOrComments -> ClausesOrComments
onClauses f [] = []
onClauses f ((Cl c):xs) = case f c of
                            Left c' ->(map Cl c') ++ (onClauses f xs)
			    Right s ->  (Comment s) : (onClauses f xs)
onClauses f ((Comment s):xs) = (Comment s) : (onClauses f xs) 


notInteresting :: Clause -> Bool
notInteresting = null.collectProperties
    
-- if the clause states that the equivalence relation rel has one of the below properties,
-- the clause is removed and replaced by a comment. Otherwise the clause is equalified
equalifyOrRemoveClause :: RelationTransformation  -> Clause -> Either [Clause] String 
equalifyOrRemoveClause r c = if (notInteresting c) then Left (equalifyClause r c) else 
  removePropertyOr r Transitive c equalifyClause `mplus_` 
  removePropertyOr r Functional c equalifyClause `mplus_` 
  removePropertyOr r Reflexive  c equalifyClause `mplus_` 
  removePropertyOr r Symmetric c equalifyClause
  
mplus_ :: Either a b -> Either a b -> Either a b
Left _  `mplus_` n = n
Right x `mplus_` _ = Right x
  
removePropertyOr :: RelationTransformation -> Property ->  Clause -> 
  (RelationTransformation  -> Clause -> [Clause]) ->  Either [Clause] String
removePropertyOr r p c fun = case withProperty p c of
  Just r' -> if (relit r) =*= r' then Right $ "Removed " ++ isnegatedS r' ++ (show p) ++ " axiom of " ++ show (rsymbol r')
	       else Left $  (fun r c)
  Nothing -> Left $  (fun r c)
  where isnegatedS (Rel _ (Just False) _) = "negated "
        isnegatedS _ = ""
  
replacePropertyOr :: RelationTransformation -> Property -> Clause -> Clause ->
  (RelationTransformation  -> Clause -> [Clause]) ->  Either [Clause] [Clause]
replacePropertyOr r p c newc fun = case withProperty p c of
  Just r' -> if (relit r) =*= r' then Right [newc]
	                           else Left $ fun r c
  Nothing -> Left $ fun r c


(=*=) (Rel r mb1 mb2) (Rel r' mb1' mb2') = r == r' && (mb1 == mb1'  || mb1 == Nothing || mb1' == Nothing)

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a						
				
{-
			

rules for equalification: 
				
If r is an equivalence relation	
				
r(X,Y) becomes rep_r(X) = rep_r(Y)
~r(X,Y) becomes rep_r(X) != rep_r(Y)
				
~r equivalence relation:
				
r(X,Y) becomes rep_r(X) != rep_r(Y)
~r(X,Y) becomes rep_r(X) = rep_r(Y)
				
If r is a partial equivalence relation:
				
Each occurence of r(X,Y) becomes
				
p(X) & p(Y) & rep(r) = rep(Y)	
				
so l1 | ... | ln | r(X,Y) becomes
				
l1 | ... | ln | r(X,Y)
l1 | ... | ln | p(X)
l1 | ... | ln | p(Y)	


l1 | ... | ln | r(X,Y) | r2(Z,Y)


l1 | ... | ln | rep(X) = rep(Y) | rep2(Z) = rep2(Y)
l1 | ... | ln | p(X) | rep2(Z) = rep2(Y)
l1 | ... | ln | p(Y) |rep2(Z) = rep2(Y)

l1 | ... | ln | rep(X) = rep(Y) | rep2(Z) = rep2(Y)
l1 | ... | ln | p(X) | rep2(Z) = rep2(Y)
l1 | ... | ln | p(Y) |rep2(Z) = rep2(Y)

l1 | ... | ln | rep(X) = rep(Y) | p2(Z)
l1 | ... | ln | p(X) | p2(Z)
l1 | ... | ln | p(Y) p2(Z)

l1 | ... | ln |  rep(X) = rep(Y) | p2(Y)
l1 | ... | ln |  p(X) | p2(Y)
l1 | ... | ln |  p(Y) | p2(Y) 





~r(X,Y) becomes

~(p(X) & p(Y) & rep(r) = rep(Y)	)

which is

~p(X) | ~p(Y) | rep(X) != rep(Y)


			
-}

torepr (Pos t) newr = Pos (torepr' t newr)
torepr (Neg t) newr = Neg (torepr' t newr)	
torepr' ((Fun r [t1,t2]) :=: _) newr = (Fun newr [t1] :=: Fun newr [t2])

topred :: Symbol -> Term -> Signed Atom
topred p x = Pos (Fun p [x] :=: truth)  

equalifyClause :: RelationTransformation  -> Clause -> [Clause]
equalifyClause _  [] = [[]]
equalifyClause r  ls = 
 case (occursR (rsymbol (relit r)) ls) of 
   Nothing 	   -> [ls] -- no occurence of s in the clause -> no change
   Just (t,rest)   -> 
     let eqrest = equalifyClause r rest 
         newr   = newsymbol r
	 [x,y]  = case the t of
       	           ((Fun f ts) :=: truth) -> ts		         
                   _ -> error "?"
     in    
     case (rsign (relit r)) of 
       Nothing -> error "this should not happen"
       Just b -> let cl1 = ((if sign t == b then Pos (the (torepr t newr)) 
	                       else Neg (the (torepr t newr)))) in
         case r of 
           Equivalence _ _ -> map ((:) cl1) eqrest
           PartialEquivalence _ p _ -> 
             if sign t == b then
                (map ((:) cl1) eqrest) ++ 
                (map ((:) (topred p x))  eqrest) ++
                (map ((:) (topred p y))  eqrest) 
               else map ((++) [negat (topred p x), negat (topred p y), Neg (the (torepr t newr))])  eqrest 
			   

occursR :: Symbol -> Clause -> Maybe (Signed Atom,Clause)
-- * picks out the first occurrence of r in the clause if there is one, and
-- * also returns the remaining clause
occursR r [] = Nothing
occursR r (a:ls) = case the a of
 ((Fun r' _) :=: _) ->  
  if r == r' then Just (a,ls) else 
   case occursR r ls of 
     Nothing 	   -> Nothing
     Just (a',ls') -> Just (a',a:ls') 
 _ -> Nothing

-----------------------------------------------------------------------------------

-- transify relations that are transitive and reflexive

transifyClauses :: ClausesOrComments
                   -> [RelationTransformation] -> ClausesOrComments
transifyClauses cs [] = cs 
transifyClauses cs (r:ss) = 
	   let cs' = (onClauses (transifyOrRemoveClause r) cs) 
	       reflclause = let varx = makeNewVar r 0 in [Pos (prd (newsymbol r) [varx,varx])]
           in (Cl reflclause) : (transifyClauses cs' ss)
	   
transifyOrRemoveClause :: RelationTransformation  -> Clause -> Either [Clause] String 
transifyOrRemoveClause r c = -- if c == [] then error "dfgdfg" else 
  removePropertyOr r Transitive c transifyClause `mplus_`
  removePropertyOr r Reflexive c transifyClause
{-  
transifyOrReplaceClause :: RelationTransformation -> Clause -> [Clause]
transifyOrReplaceClause _ [] = [[]]
transifyOrReplaceClause r ls = fromEither $ replacePropertyOr r Reflexive ls reflclause transifyClause
  where reflclause = let varx = makeNewVar r 0 in 
                      [Pos (prd (newsymbol r) [varx,varx])]
-}
  
 	      
makeNewVar r k = Var (name ((newvar r)++(show k)) ::: (V top))

transifyClause :: RelationTransformation -> Clause -> [Clause]
transifyClause _ [] = [[]]
transifyClause r ls = [transifyLiterals r 0 ls]
	
transifyLiterals :: RelationTransformation -> Int -> Clause  -> Clause
transifyLiterals _ _ [] = []
transifyLiterals r n (l:ls) = transifyLiteral r n l ++ transifyLiterals  r (n+1) ls

transifyLiteral :: RelationTransformation -> Int -> Signed Atom -> Clause
transifyLiteral r k lit = 
 if not (isInteresting lit (rsymbol (relit r))) then [lit] else 
   case rsign (relit r) of
    Just b -> if sign lit /= b then  [changeSymbol lit (newsymbol r)] 
	       else 
		  let varx = makeNewVar r k 
		      newlits = [negat (lit @@ varx), lit @@@ varx]
		  in
                    map ((flip changeSymbol) (newsymbol r)) newlits

(@@) :: Signed Atom -> Term -> Signed Atom	 
l @@ x  = case the l of
	  (Fun f [t1,t2] :=: t)   -> toSigned (sign l) ((Fun f [x,t1]) :=: t)

(@@@) :: Signed Atom -> Term -> Signed Atom	  
l @@@ x = case the l of
	  ((Fun f [t1,t2]) :=: t) -> toSigned (sign l) ((Fun f [x,t2]) :=: t)
	  
toSigned True l   = Pos l
toSigned False l  = Neg l	 

isInteresting :: Signed Atom -> Symbol	-> Bool
isInteresting sa s   = case the sa of
  (Fun f ts :=: _)  -> f == s  
  _                 -> False
	
changeSymbol :: Signed Atom -> Symbol -> Signed Atom
changeSymbol (Pos (Fun f ts :=: t)) s = (Pos (Fun s ts :=: t))
changeSymbol (Neg (Fun f ts :=: t)) s = (Neg (Fun s ts :=: t))

 
---------------------- equalify relations that are a total order ------------------------------

--total,transitiv, antisymmetrisk
--(lessthan)

--r(X,Y) | r(Y,X)                rep(x) <= rep(y)
--r(X,Y) & r(Y,Z) => r(X,Z)      
--r(X,Y) & r(Y,X) => X = Y

-- Strikt total ordning:

--r(X,Y) | r(Y,X) | X = Y
--trans
--r(X,Y) => ~r(Y,X)

--(less)

{-

~lessthaneq <=> greaterthan


lteq(X,Y) & lteq(Y,Z) => lteq(X,Z)

gt

gt(X,Y) & gt(Y,Z) => gt(X,Z)




-}


tordify :: ClausesOrComments -> [RelationTransformation] -> ClausesOrComments
tordify cs [] = cs
tordify cs (r1:ss) = let cs'  = (onClauses (tordifyOrRemoveClause r1) cs) in
  tordify cs' ss

	
-- todo merge code for ordify+tordify	
tordifyOrRemoveClause :: RelationTransformation  -> Clause -> Either [Clause] String 
tordifyOrRemoveClause r c = -- trace (show r) $
  removePropertyOr nr  Total c tordifyClauseNeg `mplus_`
  removePropertyOr nr  Transitive c tordifyClauseNeg `mplus_`
  removePropertyOr nr  AntiSymmetric c tordifyClauseNeg `mplus_`
  removePropertyOr nr  Reflexive c tordifyClauseNeg `mplus_`
  removePropertyOr r Irreflexive c tordifyClause `mplus_` 
  removePropertyOr r StrictlyTotal c tordifyClause `mplus_`
  removePropertyOr r Transitive c tordifyClause `mplus_`
  removePropertyOr r StrictlyAntiSymmetric c tordifyClause
  where nr = negateR r
  
tordifyClauseNeg = tordifyClause.negateR 
  
tordifyClause :: RelationTransformation -> Clause -> [Clause]
tordifyClause _ [] = [[]]
tordifyClause r ls = [tordifyLiterals r ls]

tordifyLiterals :: RelationTransformation -> Clause -> Clause
tordifyLiterals _ [] = []
tordifyLiterals r (l:ls) = tordifyLiteral r l : tordifyLiterals r ls

tordifyLiteral :: RelationTransformation -> Signed Atom -> Signed Atom
tordifyLiteral r lit = 
	if not (isInteresting lit (rsymbol (relit r))) then lit else
	  case the lit of
             ((Fun s ts) :=: _) -> let l =  Fun (( name "$greater" ) ::: ([top,top] :-> bool)) [(Fun (newsymbol r) [ts!!0]), (Fun (newsymbol r) [ts!!1])] in
	                            if sign lit == True then Pos (l :=: truth) else Neg (l :=: truth) 
	                           
	     _	                -> lit 


ordify :: ClausesOrComments -> [RelationTransformation] -> ClausesOrComments
ordify cs [] = cs

ordify cs (r1:ss) = let cs'  = (onClauses (ordifyOrRemoveClause r1) cs) in
  ordify cs' ss
  
-- if relit r1 =*= relit r2 then 
  -- let cs'  = (onClauses (ordifyOrRemoveClause r1) cs)
    --   cs'' = (onClauses (ordifyOrRemoveClause r2) cs)
   --in if countComments cs' > countComments cs'' then ordify cs' ss else ordify cs'' ss
  --else  ordify (onClauses (ordifyOrRemoveClause r1) cs) (r2:ss)
  
--ordify cs [_] = error "?"

--not necessary, as we know that the relevant properties of a total order are detected in both directions (<, >),
--so either relit will yield the same number of comments
--countComments [] = 0
--countComments ((Cl _):xs) = countComments xs
--countComments (_:xs) = 1+countComments xs

--ordify cs (r:ss) = 
-- let cs' = (onClauses (ordifyOrRemoveClause r) cs)
-- in ordify cs' ss

ordifyOrRemoveClause :: RelationTransformation  -> Clause -> Either [Clause] String 
ordifyOrRemoveClause r c = -- trace (show r) $
  removePropertyOr r  Total c ordifyClause `mplus_`
  removePropertyOr r  Transitive c ordifyClause `mplus_`
  removePropertyOr r  AntiSymmetric c ordifyClause `mplus_`
  removePropertyOr r  Reflexive c ordifyClause `mplus_`
  removePropertyOr nr Irreflexive c ordifyClauseNeg `mplus_` 
  removePropertyOr nr StrictlyTotal c ordifyClauseNeg `mplus_`
  removePropertyOr nr Transitive c ordifyClauseNeg `mplus_`
  removePropertyOr nr StrictlyAntiSymmetric c ordifyClauseNeg 
  where nr = negateR r
  
 {- 
  removePropertyOr :: RelationTransformation -> Property ->  Clause -> 
    (RelationTransformation  -> Clause -> [Clause]) ->  Either [Clause] String
  removePropertyOr r p c fun = case withProperty p c of
    Just r' -> if (relit r) =*= r' then Right $"Removed "  ++ (show p) ++ " axiom of " ++ show (rsymbol r')
  	             --TODO add note about if it was negated.
  				   else Left $  (fun r c)
    Nothing -> Left $  (fun r c)
-}  
negateR (TotalOrder r ns os) = (TotalOrder (negr r) ns os)
negateR (StrictTotalOrder r ns os) = (StrictTotalOrder (negr r) ns os)
  
ordifyClauseNeg = ordifyClause.negateR 

ordifyClause :: RelationTransformation -> Clause -> [Clause]
ordifyClause _ [] = [[]]
ordifyClause r ls = [ordifyLiterals r ls]

ordifyLiterals :: RelationTransformation -> Clause -> Clause
ordifyLiterals _ [] = []
ordifyLiterals r (l:ls) = ordifyLiteral r l : ordifyLiterals r ls
{-
ordifyLiteral :: RelationTransformation -> Signed Atom -> Signed Atom
ordifyLiteral r lit = 
	if not (isInteresting lit (rsymbol (relit r))) then lit else
	  case the lit of
             ((Fun s ts) :=: _) -> let l =  Fun (( name "$lesseq" ) ::: ([top,top] :-> bool)) [(Fun (newsymbol r) [ts!!0]), (Fun (newsymbol r) [ts!!1])] in
	                            if sign lit == True then Pos (l :=: truth) else Neg (l :=: truth) 
	                           
	     _	                -> lit 
-}

ordifyLiteral :: RelationTransformation -> Signed Atom -> Signed Atom
ordifyLiteral r lit = 
	if not (isInteresting lit (rsymbol (relit r))) then lit else
	  case the lit of
             ((Fun s ts) :=: _) -> let l =  Fun (( name "$lesseq" ) ::: ([top,top] :-> bool)) [(Fun (newsymbol r) [ts!!0]), (Fun (newsymbol r) [ts!!1])] in
	                            if sign lit == True then Pos (l :=: truth) else Neg (l :=: truth) 
	                           
	     _	                -> lit 
  
 {- 
 
 isInteresting :: Signed Atom -> Symbol -> Bool
transifyClause :: RelationTransformation -> Clause -> [Clause]
transifyClause _ [] = [[]]
transifyClause r ls = [transifyLiterals r 0 ls]
	
transifyLiterals :: RelationTransformation -> Int -> Clause  -> Clause
transifyLiterals _ _ [] = []
transifyLiterals r n (l:ls) = transifyLiteral r n l ++ transifyLiterals  r (n+1) ls

transifyLiteral :: RelationTransformation -> Int -> Signed Atom -> Clause
transifyLiteral r k lit = 
 if not (isInteresting lit (rsymbol (relit r))) then [lit] else 
   case rsign (relit r) of
    Just b -> if sign lit /= b then  [changeSymbol lit (newsymbol r)] 
	       else 
		  let varx = makeNewVar r k 
		      newlits = [negat (lit @@ varx), lit @@@ varx]
		  in
                    map ((flip changeSymbol) (newsymbol r)) newlits  
  
transifyOrReplaceClause :: RelationTransformation -> Clause -> [Clause]
transifyOrReplaceClause _ [] = [[]]
transifyOrReplaceClause r ls = fromEither $ replacePropertyOr r Reflexive ls reflclause transifyClause
  where reflclause = let varx = makeNewVar r 0 in 
                      [Pos (prd (newsymbol r) [varx,varx])]
-}
{-

-- hur göra vid fler än en totala ordningar? Olika maxfunktioner?

equalifyClauses3 :: [Clause] -> [Symbol] -> String -> IO [Clause]
equalifyClauses3 cs [] _ = return cs
equalifyClauses3 cs (s:ss) noClash = do
  cs' <- equalifyClauses3 cs ss noClash
  mcs <- equalifyClausesWithSymbol3 cs' s noClash
  return $ catMaybes mcs 

equalifyClausesWithSymbol3 :: [Clause] -> Symbol -> String -> IO [Maybe Clause]
equalifyClausesWithSymbol3 [] _ _ = return []
equalifyClausesWithSymbol3 cs r noClash = 
  do 
   
    let 
         newsymbol = case r of 
           (n ::: t) ->  (name ("rep_" ++ noClash ++ (normalName noClash n))) ::: ([top,top]:-> bool)
         newvar = Var ((name ("Newvar_" ++ noClash)) ::: (V top))
    return $ map (equalifyClause3 r newsymbol newvar) cs


maxaxioms :: String -> [Maybe Clause]
maxaxioms noClash = [Just total,Just associative,Just commutative]
  where
    maxsymbol = (name ("max_" ++ noClash)) ::: ([top,top] :-> top)
    xvar      = Var ((name "X") ::: (V top))
    yvar      = Var ((name "Y") ::: (V top))
    zvar      = Var ((name "Z") ::: (V top))
    total     =  [Pos (Fun maxsymbol [xvar,yvar] :=: xvar),Pos (Fun maxsymbol [xvar,yvar] :=: yvar) ] 
    associative = [Pos (Fun maxsymbol [xvar, Fun maxsymbol [yvar,zvar]] :=: Fun maxsymbol [Fun maxsymbol [xvar,yvar],zvar])]
    commutative = [Pos ((Fun maxsymbol [xvar, yvar]) :=: (Fun maxsymbol [yvar,xvar]))]

-- 1. tag bort total, transitiv, antisymm
equalifyClause3 :: Symbol -> Symbol -> Term -> Clause -> Maybe Clause
equalifyClause3 r _ _ c | isTransitive c == Just r  || isTotal c == Just r || isAntiSymmetric c == Just r = Nothing                    -- remove transitivity

-}

{-

A1. total: R(x,y) | R(y,x)
A2. transitiv: ...
A3. anti-symmetrisk: ~R(x,y) | ~R(y,x) | x=y

Då kan vi hitta på en ny funktion "max", och lägga till följande axiom:

B1. "total": max(x,y)=x | max(x,y)=y
B2. associativ: ...   max(max(x,y),z) = max(x,max(y,z))
B3. kommutativ: ... max(x,y) = max(y,x)

Dessutom:

1. Vi tar bort alla axiom A1,A2,A3
2. Vi lägger till B1, B2, B3
3. Vi ersätter alla förekomster av R(s,t) med max(s,t)=t



Om vi hittar en strikt total ordning Q, dvs:

C1. strikt total: Q(x,y) | Q(y,x) | x=y
C2. transitiv: ...
C3. strikt anti-symmetrisk: ~Q(x,y) | ~Q(y,x)

Då kan vi hitta på den ny funktion "max", och lägga till följande axiom:

B1. "total": max(x,y)=x | max(x,y)=y
B2. associativ: ...
B3. kommutativ: ...

Dessutom:

1. Vi tar bort alla axiom C1,C2,C3
2. Vi lägger till B1, B2, B3
3. Vi ersätter alla förekomster av Q(s,t) med max(s,t)!=s
-}



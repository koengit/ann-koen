{-# LANGUAGE BangPatterns #-}
module Equalox.Equalify where

import System.IO

import Equalox.Properties hiding (show')
import Equalox.Implications
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

------- Data type definitions -----------------------------------------------------------


data Transformation =
 Trans {
      --  name     :: String,
     trans    ::  Ify,
     format   :: Format,
     names    :: [String],
     propsL   ::  [Property], 
     propsR   ::  Int -> [Clause], 
     posR     ::  (Int -> Int -> Term -> Term -> [Clause]), 
     negR     ::  (Int -> Int -> Term -> Term -> [Clause]), 
     proofLR  ::   Maybe  Clause, 
     proofRL  ::   Maybe  Clause
 }
 
data ClauseOrComment = Comment String | Cl Clause
   deriving (Eq)
   
data Format = TFF | CNF
  deriving (Eq,Show)

instance Show ClauseOrComment where
   show (Comment s) = "%% " ++ s
   show (Cl c)      = showClause c

type ClausesOrComments = [ClauseOrComment]

data WorkingTheory = 
  WTheory {axioms   :: ClausesOrComments, 
           hyps     :: ClausesOrComments, 
           negconjs :: ClausesOrComments}

-- data RelationLit = Rel {rsymbol :: Symbol, rsign :: (Maybe Bool), dir :: (Maybe Bool)}  
-- defined in Properties.hs 
 
 --- ***************************** TRANSFORMATIONS ********************************** ---

equalification :: String ->  Transformation
equalification noClash = 
    Trans {
      trans   = Equalify,
      format  = CNF,
      names   = ["rep_"],
      propsL  = [Transitive,Reflexive,Symmetric], 
      propsR  = \_ -> [],
      posR    = \n _ a1 a2 -> [[Pos ((Fun (rep n) [a1]) :=: (Fun (rep n) [a2]))]], 
      negR    = \n _ a1 a2 -> [[Neg ((Fun (rep n) [a1]) :=: (Fun (rep n) [a2]))]],  
      proofLR = Just $ [Pos ((Fun (rep 1) [arg1]) :=: (Fun (rep 1) [arg2]))],
      proofRL = Just $ undefined} 
   where rep n = (name ("rep_" ++ noClash ++ show n) ::: ([top]:-> top)) 
  
  
equalification_idempotent :: String -> Transformation
-- equalification with added idempotency axiom
equalification_idempotent noClash = (equalification noClash) 
        {propsR = \k -> 
           [[Pos ((Fun (rep k) [Fun (rep k) [repx k]]) :=:  (Fun (rep k) [repx k]))  ]]}
    where   rep n = (name ("rep_" ++ noClash ++ show n) ::: ([top]:-> top)) 
            repx k = Fun (rep k) [varx]

pequalification :: String -> Transformation
pequalification noClash = 
     Trans {
         trans = PEqualify,
         format = CNF,
         names = ["rep_","p_"],
         propsL = [Transitive,Symmetric], 
         propsR = \_ -> [], 
         posR = \n _ a1 a2 -> [[Pos ((Fun (p n) [a1]) :=: truth)], 
                               [Pos ((Fun (p n) [a2]) :=: truth)],
                               [Pos ((Fun (rep n) [a1]) :=: (Fun (rep n) [a2]))]], 
         negR = \n _ a1 a2 -> [[Neg ((Fun (p n) [a1]) :=: truth), 
                                Neg ((Fun (p n) [a2]) :=: truth),
                                Neg ((Fun (rep n) [a1]) :=: (Fun (rep n) [a2]))]],   
         proofLR = Nothing, 
         proofRL = Nothing}   
   where rep n = (name ("rep_" ++ noClash ++ show n) ::: ([top]:-> top)) 
         p n   = (name ("p_" ++ noClash ++ show n) ::: ([top] :-> bool))


transification_with_reflexivity :: String -> Transformation
transification_with_reflexivity noClash = 
   (transification noClash) 
      {propsL = [Transitive,Reflexive], 
       propsR = \k -> [Reflexive `for` (q k)],
       posR = \k n a1 a2 -> [
                         [Neg (qlit k (varx n) a1), 
                         Pos (qlit k (varx n) a2) ]]
       }
   where q k = (name ("q_" ++ noClash ++ show k) ::: ([top,top]:-> bool))
         qlit k a b    = (Fun (q k) [a,b] :=: truth)
         varx n = Var (name ("X_" ++ noClash ++ show n) ::: (V top)) 
  
transification :: String -> Transformation
transification noClash = 
      Trans {
           trans    = Transify,
           format   = CNF,
           names  = ["q_"],
           propsL = [Transitive], 
           propsR = \_ -> [], -- \k -> [Reflexive `for` (q k)], 		   
           posR = \k n a1 a2 -> [[Pos (qlit k a1 a2)], 
                                [Neg (qlit k (varx n) a1), 
                                Pos (qlit k (varx n) a2) ]],
           negR = \k n a1 a2 -> [[Neg (qlit k a1 a2)]],
           proofLR = Just ([Pos (rlit 1 arg1 arg2)]),
           proofRL = Just [(Neg (qlit 1 arg1 arg2))] }
   where 
    q k           = (name ("q_" ++ noClash ++ show k) ::: ([top,top]:-> bool))
    r k           = (name ("r_" ++ noClash ++ show k) ::: ([top,top]:-> bool))
    qlit k a b    = (Fun (q k) [a,b] :=: truth)
    rlit k a b    = (Fun (r k) [a,b] :=: truth)
    varx n = Var (name ("X_" ++ noClash ++ show n) ::: (V top)) 


ordification :: String -> Transformation
ordification noClash = 
   Trans {
     trans   = Ordify,
     format  = TFF,
     names   = ["rep_"],
     propsL  = [Total,Transitive,AntiSymmetric],
     propsR  = \k -> [[Neg ((Fun (rep k) [varx]) :=: (Fun (rep k) [vary])),
                      Pos (varx :=: vary)]],
   -- injectivity of rep		   
     posR    = \k n a1 a2 -> [[Pos ((lesseq [Fun (rep k)  [a1], 
                              Fun (rep k) [a2]]) :=: truth)]],
     negR    = \k n a1 a2 -> [[Neg ((lesseq [Fun (rep k)  [a1], 
                              Fun (rep k) [a2]]) :=: truth)]],
     proofLR = undefined,
     proofRL = undefined}
   where rep n = (name ("rep_" ++ noClash ++ show n) ::: ([top]:-> top)) 
         lesseq  = Fun (( name "$lesseq" ) ::: ([top,top] :-> bool)) 

arg1,arg2 :: Term
arg1 = Var $ (name "arg1") ::: ([top] :-> top)
arg2 = Var $ (name "arg2") ::: ([top] :-> top)
 

for :: Property -> Symbol -> Clause
Reflexive      `for` s  = [(Pos (Fun s [varx,varx] :=: truth))]
Transitive     `for` s  = [Pos (Fun s [varx,varz] :=: truth), 
                          Neg (Fun s [varx,vary] :=: truth) , 
                          Neg (Fun s  [vary,varz] :=: truth)]
Symmetric      `for` s  = [Pos (Fun s [varx,vary] :=: truth), 
                          Neg (Fun s [vary,varx] :=: truth)]
Total          `for` s  = [Pos (Fun s [varx,vary] :=: truth), 
                          Pos (Fun s [vary,varx] :=: truth)]
AntiSymmetric  `for` s  = [Pos (varx :=: vary), Neg (Fun s [varx,vary] :=: truth),
                          Neg (Fun s [vary,varx] :=: truth)] 
Functional     `for` s  = [Pos (Fun s [vary,varz] :=: truth), 
                          Neg (Fun s [varx,vary] :=: truth) , 
                          Neg (Fun s  [varx,varz] :=: truth)]
Serial2        `for` s  = error "Serial2"  -- we don't know the skolem function...
Irreflexive    `for` s  = [(Neg (Fun s [varx,varx] :=: truth))]
Coreflexive    `for` s  = [(Neg (Fun s [varx,vary] :=: truth)), 
                           Pos (varx :=: vary) ] 
--StrictlyTotal `for` s = undefined (We don't deal with these)
--StrictlyAntiSymmetric `for` s = undefined
 
varx = Var  (name "X" ::: (V top)) 
vary = Var  (name "Y" ::: (V top))
varz = Var  (name "Z" ::: (V top))  
  
 ---  ******************************************************************************** --


-- main function	
equalify :: (?flags :: Flags) => [Clause] -> [Clause] -> [Clause] -> IO ClauseAnswer
equalify theory hyps obligs = do

  let
    ify      = (Flags.ify ?flags)
    tf       = (Flags.thisFile ?flags)    
    outdir'  = (Flags.dir ?flags)
    outdir   = case outdir' of
                 Nothing -> "" 
                 Just d  -> d ++ "/"
    outfile  = outdir ++(takeBaseName tf) ++ "_" ++ show ify ++ (takeExtension tf) 
    tobligs  = theory++hyps++obligs
    noClash  = noClashString (map toForm tobligs)
    propmap  = getAllProperties tobligs  -- :: [(RelationLit,[Property])]
                                         -- All properties of relations in input clauses.
                                         -- A symmetric property yields two entries 
                                         -- in the table, one for each direction
                                         -- and vice versa for pos+neg properties.   
    transformation = (case ify of 
      Equalify      -> equalification 
      Equalify_Idem -> equalification_idempotent
      Transify      -> transification 
      Transify_Refl -> transification_with_reflexivity
      PEqualify     -> pequalification 
      Ordify        -> ordification 
      _         -> error "add new transformations here!"
      ) noClash
    expanded = (expand (propsL transformation))
    -- all the properties that can be removed (implied by "left-properties")		
    relits = findApplicable propmap transformation 
    -- The RelationLits found to have the necessary properties of the chosen transformation   
  mapM putStrLn (map show propmap)
  putStrLn ("\n\n" ++ show expanded)
  putStrLn ("\n\n" ++ show relits)
  if relits == [] then return $ NoAnswerClause NA -- No applicable relations
   else do  
      let 
          wt  = transform (WTheory (map Cl theory) (map Cl hyps) (map Cl obligs)) 
                    transformation relits noClash expanded 1 
          ss  = transformationComment transformation relits expanded noClash 1
      writeProblemToFile outfile wt (format transformation) ss 
      return $ NoAnswerClause NA
    
                       -- ***** Formatting and printing ***** --  
    
writeProblemToFile :: FilePath -> WorkingTheory -> Format -> String -> IO ()
writeProblemToFile f wt fmt  ss = 
 do
   putStrLn ("Writing to file " ++ show f)
   h <- openFile f WriteMode
   hSetBuffering h LineBuffering
   hPutStrLn h $ ss
   let 
      format' a b = unlines.(clausesOrComments2format fmt a b)  
      axs = axioms wt
      hps = hyps wt
      cjs = negconjs wt
   hPutStr h $ format' hps 1 "hypothesis"
   hPutStr h $ format' axs ((length hps)+1) "axiom"
   hPutStr h $ format' cjs ((length axs + length hps) +1) "negated_conjecture" 
   hClose h
 
clausesOrComments2format :: Format -> ClausesOrComments -> Int -> String -> [String]
clausesOrComments2format _ [] _ _ =  []
clausesOrComments2format f ((Cl c):cs) n s 
  | f == CNF = (clause2cnf  c n s) : clausesOrComments2format f cs (n+1) s
  | f == TFF =
      (if s == "negated_conjecture" 
        then [clauses2tffconj [c' | Cl c' <- (Cl c:cs)] n] ++
         ["%% " ++ s | Comment s <- cs]  
            else   (clause2tff  c n s) : clausesOrComments2format f cs (n+1) s)
  | otherwise = error "add new formats here"   
  
clausesOrComments2format f (c:cs) n  s = (show c) : (clausesOrComments2format f cs n s)


transformationComment :: Transformation -> [RelationLit] -> [(Property,Bool,Bool)] -> 
                           String -> Int -> String
transformationComment _ [] expanded _ _ = ""
transformationComment tr ((Rel r b1 b2):rs) expanded noClash n = 
   let s = if b1 ==  Just False then " negated " else " " in
     "%% " ++ s ++ showList' (propsL tr) ++ " relation " ++ show r ++
     " replaced by relations/functions " ++ showNames (names tr) noClash n ++ "\n" ++ 
     " %% " ++ "   The following properties of " ++ show r ++ 
     " can be removed: \n " ++ unwords (map (showexp b1 b2) expanded) ++  "\n" ++ 
     transformationComment tr rs expanded noClash (n+1) ++ "\n" 
            
showexp b1 b2 (prop,b1',b2') = "%%      * " ++
  (if b1 == Just b1' || b1 == Nothing then "" else " negated " ++ 
    if b2 /= Just b2' || b2 == Nothing then "" else "flipped ") ++ show prop ++ "\n"

showNames [] _ _ = ""
showNames [n] noClash k    = n ++ noClash ++ show k
showNames (n:xs) noClash k = (showNames [n] noClash k) ++ ", " ++ showNames xs noClash k

--- ************************* Transformation functions ****************************** ---
   
   
transform :: WorkingTheory -> Transformation -> [RelationLit] -> String -> 
               [(Property,Bool,Bool)]-> Int ->  WorkingTheory
transform wt _ [] _ _ _ = wt
transform wt tr (rel@(Rel r x y):rs) noClash exps k = 
  let new_wt            = transform wt tr rs noClash exps (k+1) 
      transformPart pt = transformWith tr rel pt k exps
  in
           WTheory ([Cl prop | prop <- (propsR tr) k] 
                    ++ transformPart (axioms new_wt))
                    (transformPart (hyps new_wt))  
                    (transformPart (negconjs new_wt))    


transformWith :: Transformation -> RelationLit -> [ClauseOrComment] -> 
                  Int -> [(Property,Bool,Bool)] -> [ClauseOrComment]
transformWith t r cs k exps =  (concat [transformClause t r c k exps | c <- cs]) 


transformClause :: Transformation -> RelationLit -> ClauseOrComment -> 
                   Int -> [(Property,Bool,Bool)] -> [ClauseOrComment]
transformClause _ _ (Comment comment) _ _ = [Comment comment]
transformClause t r (Cl c) k toremove = 
   case filter (isJust.snd) [((p,b1,b2),(withProperty p c)) | (p,b1,b2) <- toremove ] of
        [] -> map Cl $ transformLits k 1 t r c
        xs -> transformOrComment xs r k t c
        --xs -> trace ("!! " ++ show c ++ "\n" ++ show xs) $ transformOrComment xs r k t c


transformOrComment [] r k t c = map Cl $ transformLits k 1 t r c
transformOrComment (((p,b1',b2'),Just rel@(Rel _ b1'' b2'')):xs) r k t c  =
  if (rel =*= (((if b1' then id else negaR).(if b2' then flipR else id)) r)) then
    [Comment $ "Removed " ++ isnegatedS b1'' ++ show p ++ " axiom of " ++ show rel] 
      else transformOrComment xs r k t c
   where isnegatedS b = if b == Just True || b == Nothing then "" else "negated "
   

transformLits :: Int -> Int -> Transformation -> RelationLit -> Clause -> [Clause]
transformLits _ _ _ _ [] = [[]]
transformLits k n t r (l:ls) = 
   let cs = transformLit k n t r l in  
   [c ++ cc | c <- cs, cc <- transformLits k (n+1) t r ls]

  
transformLit :: Int -> Int -> Transformation -> RelationLit -> Literal -> [Clause]
transformLit k n t rel@(Rel r b1 b2) lit = 
   if not (isInteresting lit r) then [[lit]] 
     else 
       let transformFun = if sameSign lit rel then (posR t) else (negR t) in
        transformFun k n (arg 1 lit) (arg 2 lit)
    where 
          arg n lit = case the lit of 
             ((Fun _ xs) :=: _)     -> xs!!(n-1)
             _                     -> error "?"


---- *************** Computing properties ************* ---


findApplicable :: PropertyMap ->Transformation -> [RelationLit]
findApplicable pmap t = trace (show pmap) $ (nubBy (sameR) (filter (isApplicable pmap t) (map fst pmap)))
  where sameR (Rel r _ _) (Rel r' _ _) = r == r'

isApplicable :: PropertyMap  -> Transformation -> RelationLit -> Bool
isApplicable pmap trans r =
  and [(  ((trace ("finding applicable: ") $ hasProperty r prop pmap))) | prop <- propsL trans]

  
type PropertyMap = [(RelationLit,[Property])]
type Literal     = Signed Atom   
 
expand :: [Property] -> [(Property,Bool,Bool)]
--all properties implied by list
expand [] = []
expand ps =  fixlist ++ computeImplications fixlist
  where 
      fixlist = [fix p | p <- ps]
      fix p = case p of 
                  Irreflexive           -> (Reflexive,False,False)
                  StrictlyAntiSymmetric -> (Total,False,False)
                  StrictlyTotal         -> (AntiSymmetric,False,False)
                  _                     -> (p, True,False)

computeImplications :: [(Property,Bool,Bool)] -> [(Property,Bool,Bool)]
computeImplications props = impliedlist
   where 
     impliedlist        = nub [convert2prop p | (p,impls)  <- implications, 
                                and [(elem i shownprops) | i <- impls]]
     shownprops         = map show' props
     show' (prop,b1,b2) = show prop ++ if not b1 then "_neg" else 
                            "" ++ if b2 then "_flip" else ""

sameSign :: Literal -> RelationLit -> Bool
sameSign (Pos _) (Rel _ (Just b) _) = b
sameSign (Neg _) (Rel _ (Just b) _) = not b
sameSign l r = error (show l ++ "    " ++ show r)

isInteresting :: Signed Atom -> Symbol -> Bool
isInteresting sa s   = case the sa of
  (Fun f ts :=: _)  -> f == s  
  _                 -> False
  
(=*=) (Rel r mb1 mb2) (Rel r' mb1' mb2') = r == r' && (mb1 == mb1'  || mb1 == Nothing || mb1' == Nothing)

    
getAllProperties :: [Clause] -> PropertyMap -- [(RelationLit,Property)]
getAllProperties cs = let
      prps            = concat $ map collectProperties' cs    
      impliedProps  =   getImpliedProps $ prps
      in deriveFromTableRec $ sortByFirst $ prps ++ impliedProps
      

deriveFromTable :: [(RelationLit,[Property])] -> [(RelationLit,[Property])]
deriveFromTable ps = sortByFirst [(r,p) | (r,_) <- ps, p <- allProperties,  
                                   elem r (getRelsWithProperty ps p) ]

deriveFromTableRec :: [(RelationLit,[Property])] ->  [(RelationLit,[Property])]
deriveFromTableRec rps =  
   let rps'  = deriveFromTable rps 
       rps'' = sortByFirst $ getImpliedProps [(r,p) | (r,pps) <- rps', p <- pps ] ++ 
                              [(r,p) | (r,pps) <- rps', p <- pps]
   in
       if (length rps == length rps'') then rps else deriveFromTableRec rps''
       


getRelsWithProperty :: [(RelationLit,[Property])] -> Property -> [RelationLit]
getRelsWithProperty rps  p =  let impls = sortByFirst implications in
                               [ r | (r,_) <- rps, (hasProperty r p rps)]


--getImpliedProps :: [((Symbol,Int),Property)] -> [((Symbol,Int),Property)]
getImpliedProps :: [(RelationLit,Property)] -> [(RelationLit,Property)]
getImpliedProps prps =  [(r2,Reflexive)   
                                  | (r, Impl r2) <- prps, elem (r,Reflexive) prps] ++
                        [(r2,Total) 
                                  | (r, Impl r2) <- prps, elem (r,Total) prps] ++
                        [(r2,Serial2)      | (r, Impl r2) <- prps, elem (r,Serial2) prps] ++
                        [(r,AntiSymmetric) 
                                  | (r, Impl r2) <- prps, elem (r2,AntiSymmetric) prps] ++
                        [(r, Coreflexive)  | (r, Impl r2) <- prps, elem (r2,Coreflexive) prps]  

 
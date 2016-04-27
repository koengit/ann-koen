module Infinox.Relations where

import Form
import Flags( Flags, Method(InjNotSurj,SurjNotInj,Serial,Sausage))
import Infinox.Conjecture
import Infinox.Generate
import Infinox.Util
import System.Directory
import qualified Data.Set as S
import qualified Infinox.Symbols as Sym
import Infinox.Types
import Infinox.Settings
import Debug.Trace
import Control.Monad.Reader
import Data.List (nub)
import Data.Set as S( Set )
import Output
import System.Process (system)
import qualified Data.Set as S

continueRelations :: Method -> [Relation] -> Settings Result
continueRelations method rels = do 
	settings <- ask
	let
			rflag'		=		case relflag settings of
											Nothing 	-> Just "-"
											Just "-"	-> Just "-"
											_					-> relflag settings
			sig'			=   sig settings
			psymbols	=		S.toList (psymbs sig')
			ps				=	case (pflag settings) of 
									Nothing  -> [Nothing]
									Just "-" -> Nothing : (map Just $ collectSubsets (pflag settings) psymbols)
									Just p'  -> (map Just $ collectSubsets (pflag settings) psymbols)
								--Nothing : (map Just $ collectSubsets (pflag settings) psymbols)
			rs        =  	collectRelations rflag' psymbols False
										--collect all predicates with at least two "X"
			rs' 			= 	concatMap genRelsXY rs
										--convert to predicates containing (all combinations of) 
										--variables "X" and "Y"
			
			testrels = if method == Serial then (rs' ++ map nt rs')
						else rs' ++  map flipallthexsandysaround rs' ++ 
										(map nt rs') ++ map flipallthexsandysaround (map nt rs' ) 
			-- relations made from functions are unnecessary:
			-- r(X,Y) <=> f(X) = f(Y) is never irreflexive. 
			-- r(X,Y) <=> f(X) = Y: transitivity makes it necessarily reflexive which contradicts irreflexivity..

	continueRelations' method testrels testrels ps 
	
flipallthexsandysaround :: Relation -> Relation
flipallthexsandysaround rel = subst ((Sym.x |=> (Var Sym.y)) |+| (Sym.y  |=> (Var Sym.x))) rel
	
makeAntiSymmetric :: Relation -> Relation
makeAntiSymmetric r = r /\ nt (flipallthexsandysaround r)

continueRelations' :: Method -> [Relation] -> [Relation] -> [Maybe Form] -> Settings Result	
continueRelations' _ _ _ [] = return None
	
continueRelations' method [] rs (p:ps) = 
	continueRelations' method rs rs ps

continueRelations' method (r:rs) rs' (p:ps) = do
 if r == equality then continueRelations' method rs rs' (p:ps) else do
	b <-  checkProperty method r p 
	if b then case p of
		Nothing ->	return $ F r
		Just p'	->	return $ FF r p' 
		else
			continueRelations' method rs rs' (p:ps) 

-------------------------------------------------------------------------------

checkProperty :: Method -> Relation -> Maybe Form -> Settings Bool
checkProperty method r p = 
	do
		settings <- ask
		let 
			v        = verbose settings
			noClash' = noClash settings
			tempdir' = tempdir settings
			problem  = axiomfile settings
			pr		   = prover settings
			conj = case method of 
				Serial ->		
					form2conjecture noClash' 0 (conjSerial r p)
					
				Sausage -> 	
							form2conjecture noClash' 0 (conjSerialOnSausage r p)	
			provefile = tempdir' ++ "checksr"
		Settings $ lift $ maybePrint v ("Checking relation: " ) (Just r)
		Settings $ lift $ maybePrint v "under " p	
	--	Settings $ lift $ print conj
	--	Settings $ lift $ putStrLn $ show problem
		Settings $ lift $ system $ "cp " ++ problem ++ " " ++ provefile
		b <- Settings $ lift $ prove pr conj provefile (elimit settings)
	--	Settings $ lift $ putStrLn $ show b
		Settings $ lift $ removeFile provefile
		return b					

-------------------------------------------------------------------------------	

equal = \x -> \y -> Atom (x :=: y)

getSymbol :: Relation -> Symbol
getSymbol (Atom ((Fun s _) :=: _)) = s

containsSymbol :: Form -> Symbol -> Bool
containsSymbol f a = S.member a (symbols f) 

conjSerialOnSausage :: Relation -> Maybe Form -> Form
conjSerialOnSausage rel subset = 
	case subset of
	 Just pr  -> 
	 	existsRel "R" rel $ \r ->
			existsPred "P" pr $ \p -> 
				let p'  =  \x   -> p x /\ exist v (r x v) in
								
				exist x (p' x)
				/\
				forEvery x ((nt (p' x)) \/ exist y (p' y /\ (r x y) /\ 
			 				(forEvery z (nt (p' z) \/ ((nt (r z x)) \/ r z y)))))
							/\ forEvery w ((nt (p' w)) \/ nt (r w w))	
	 Nothing ->   	
	 	existsRel "R" rel $ \r ->
			let p  =  \x   -> exist w (r x w) in
			exist x (p x)
			/\
			forEvery x ((nt (p x)) \/ exist y (p y /\ (r x y) /\ 
			 			(forEvery z (nt (p z) \/ ((nt (r z x)) \/ r z y)))))
						/\ forEvery w ((nt (p w)) \/ nt (r w w))
    where
     x = Var Sym.x
     y = Var Sym.y
     z = Var Sym.z
     w = Var Sym.w
     v = Var Sym.v
					
--seriality
-- we don't check the version with arguments flipped, because when adding antisymmetry, 
-- we get that by adding the negated relation as a candidate.
conjSerial :: Relation -> Maybe Form -> Form		
conjSerial rel subset = 
	case subset of
		Nothing	-> 
						existsRel "R" rel $ \r ->
							let 
								r' = \x y -> r x y  /\ nt (r y x) -- we force the relation to be antisymmetric,
								p  =  \x   -> exist w (r' x w)    -- as this is necessary when it is both transitive and irreflexive.
								-- we also consider the subset on which the relation always is serial.
										
							in
						
							exist x (p x)  --p non empty    	
		  		  	/\ 

					  	(forEvery [x,y,z] ( (nt (p x) \/ nt (p y) \/ nt (p z)) \/ 
								((nt (r' x y)) \/ (nt (r' y z)) \/ (r' x z) ))) 
						/\ (forEvery x (nt (p x) \/ exist y (p y /\ r' x y))) -- transitive & serial in p
		
	
	
		Just pr	->
			existsPred "P" pr $ \p -> 
				existsRel "R" rel $ \r ->
					--let p'  = \x -> exist w ((r x w) /\ nt (r w x)) in 
					
					let 
						r' = \x y -> r x y  /\ nt (r y x) 
						p'  = \x   -> p x /\ exist w (r' x w) in
					
					exist x (p' x) --  /\ --p non empty  
					/\ 

								  	(forEvery [x,y,z] ( (nt (p' x) \/ nt (p' y) \/ nt (p' z)) \/ 
												( ((nt (r x y)) \/ (nt (r y z)) \/ (r x z) ))))  /\
												 
										 (forEvery x (nt (p' x) \/ exist y (p' y /\ r' x y))) -- transitive & serial in p

		
 where
  x = Var Sym.x
  y = Var Sym.y
  z = Var Sym.z
  w = Var Sym.w

{- should be subsumed by other methods?
conjRelation :: Relation -> Maybe Form -> Form
conjRelation rel subset = 
	case subset of
		Nothing	-> 
			existsRel "R" rel $ \r ->
				let 
					surjective = forEvery y (exist x (r x y))
					injective	 = forEvery [x,y,z] $ (equal x y) \/ nt (r x z /\ r y z) --(nt (r x z)) \/ (nt (r y z)) \/ equal x y
					total			 = forEvery x $ exist y $ r x y 
					function	 = forEvery [x,y,z] $ (nt (r x y)) \/ (nt (r x z)) \/ equal y z 
				in
					((total /\ injective) `Equiv`  (nt (surjective /\ function)))		
						-- \/ (serial /\ transitive /\ irreflexive)		
		Just p' -> --why nt p?
			existsPred "P" p' $ \p -> 
				existsRel "R" rel $ \r ->
					let
						surjective = forEvery y $ (nt (p y)) \/ (exist x (p x /\ (r x y)))
						injective  = forEvery [x,y,z] $ (nt(p x) \/ nt(p y) \/ nt(p z))  \/  
																							 (equal x y) \/ nt (r x z /\ r y z)
						total			 = forEvery x $ nt(p x) \/ (exist y $ p y /\ r x y) 
						function	 = forEvery [x,y,z] $ (nt (p x) \/ nt (p y) \/ nt (p z)) \/ 
														((nt (r x y)) \/ (nt (r x z)) \/ equal y z) 
					in
						((total /\ injective) `Equiv`  (nt (surjective /\ function)))	
	 where
	  x = Var Sym.x
	  y = Var Sym.y
	  z = Var Sym.z
-}
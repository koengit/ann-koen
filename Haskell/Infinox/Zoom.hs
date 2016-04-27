module Infinox.Zoom  where

import System.IO
import Form
import Infinox.Util (finiteModel)
import Infinox.Conjecture (form2axioms)
import System.Random
import Data.List
import qualified Data.Set as Set

import Debug.Trace
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class


zoom :: FilePath -> [Form] -> String -> Int -> IO [Form]
zoom dir fs noClash plimit   = evalStateT  ( mini (hasNoFiniteModel dir noClash plimit) fs) (0,0,0)
	
hasNoFiniteModel :: FilePath -> String -> Int -> [Form] -> StateT (Int,Int,Int) IO Bool
hasNoFiniteModel dir noClash plimit fs = do
         liftIO (putStrLn (show (length fs)))
         (count1,count2,count3) <- get
         put (count1,count2,count3+1)
         liftIO $ do		
            h <- openFile (dir ++ "zoom"++(show (count3+1))) WriteMode
            hPutStr h $ (form2axioms fs noClash)
            hClose h
            b <- finiteModel (dir ++ "zoom"++(show count3)) plimit
            return $ not b
	
--TODO :add a parameter "strict" for strict zooming, i.e. always zoom until the end

 -- precondition for mini p xs: p is monotonic, p xs = True
  -- postcondition: the result is a smallest subset ys of xs such that p ys = True
mini :: (Eq a, Symbolic a) => ([a] -> StateT (Int,Int,Int) IO Bool) -> [a] -> StateT (Int,Int,Int) IO [a]
mini p [] = return []
mini p [x] = do 
		ans <- p [] 
		if ans then return [] else return [x]
mini p xs = do
      (count1,count2,count3) <- get
      let lenxs   = length xs
          (as,bs) = (take (lenxs `div` 2) xs, drop (lenxs `div` 2) xs)     
	  
          doBlock = do
	          pas <- p as
             
                  if pas then mini p as else do
          
                   pbs <- p bs
             
                   if pbs then mini p bs else 
                    do
                     as' <- mini (\ys -> p (ys ++ bs)) as
                     bs' <- mini (\ys -> p (as' ++ ys)) bs
		    
                     liftIO $ permute $ as'++bs'
				   
     
      if count3 > 20 then 
         let 	(fs,rest) = Set.partition isFunSymbol (symbols xs) 
	      	(ps,_)    = Set.partition isPredSymbol rest
       	   	sfs       = Set.size fs
		sps       = Set.size ps
		average_varityP = (foldr (+) 0 (map arity (Set.toList ps))) `div` sps
		average_varityF = (foldr (+) 0 (map arity (Set.toList fs))) `div`sfs
		continue  = (sfs+sps <= 5) || (sfs + sps <= 10 && average_varityP <= 3 && average_varityF <= 2) in do
         liftIO (putStrLn ( show (  "number of functions: " ++ show sfs ++ "    number of predicates: " ++ show sps )))
         if not continue then do
		 liftIO $ putStrLn $ "Giving up: " ++ "number of functions: " ++ show sfs ++ "    number of predicates: " ++ show sps 
		 return xs  
		 
          else doBlock
        else doBlock
  --    liftIO (putStrLn (show count ++ " mini: length of input " ++ (show (length xs))))
      
     
	  
--miniCloud :: 
    
     
permute :: Eq a => [a] -> IO [a]
permute as = do 
	g <- getStdGen
	permute' g as

permute' :: Eq a => StdGen -> [a] -> IO [a]
permute' _ [] = return []
permute' g  xs = do
	let 
		(a,g') 		= randomR (0,(length xs) -1) g 
		el				= xs !! a
	xs'			<- permute' g' (delete el xs)
	return (el:xs')
	  
    

-------------------------------------------------------------------------------

-- gå igenom varje shrinklista. när vi hittar en utan ändl. modell vill vi 
-- gå tillbaka och kolla vilka axiom vi lagt till i det senaste steget.
-- om vi kombinerade A och B i sista steget, fixera den minsta av A och B,
-- om den minsta är B, så zoomar vi A och lägger alltid till B.
-- när vi nästa gång hittar en delmängd utan ändl modell fixerar vi den minsta
-- av AA+B och AB+B och fortsätter.

{-


zoom :: FilePath -> [Form] -> String -> Int -> IO [Form]
zoom dir fs noClash plimit   = do
	cs <- zoom' dir (fs,length fs,False) (shrink fs) noClash plimit 0 
	return $ sort $ cs

zoom' :: FilePath -> ([Form],Int,Bool)-> [([Form],Int,Bool)] -> String -> Int -> Int -> IO [Form]

zoom' dir (best,0,False) _ noClash plimit count = do
	putStrLn "no more unfixed. unfixing fixed."
	zoom' dir (best,length best,True) (shrink best) noClash plimit count
	
zoom' dir (best,0,True) _ noClash plimit count = do
		putStrLn "already unfixed fixed."
		return best
	
zoom' dir (best,k,_) [] _ _ _  = do 
									putStrLn "shrinkList empty"
									return best

	
zoom' dir (best,k,b) ffs noClash plimit count  = do

	 mb <- smallestUnsat dir ffs noClash plimit count
	 case mb of
		 
		Nothing ->  do -- fimos of all shrinklists:
					if b then return best else do
					-- we take the most recent without fimo and swap places of the fixed and unfixed axioms.
					putStrLn ("all shrink lists have finite models...")
					putStrLn $ "We have " ++ (show k) ++ " unfixed axioms and " ++ (show ((length best) -k)) ++ " fixed axioms."
					putStrLn $ "Switching places..." 
					let (newfs,splitk) = (drop k best ++ take k best, (length best) - k) in 
						do 
							putStrLn $ "We have " ++ (show ((length best) -k)) ++ " unfixed axioms and " ++ (show k) ++ " fixed axioms."
							putStrLn "shrinking the new unfixed axioms."
							shrinkedunfixed 	<- (shrinkUnfixed splitk newfs)
							putStrLn "zooming the new shrinked lists."
							zoom' dir (best,k,True) shrinkedunfixed noClash plimit (count+1)
						--	newfs'				<- zoom' dir (best,k) shrinkedunfixed noClash plimit (count+1)
											--switching places of fixed and unfixed
						--	zoom' dir (newfs',splitk) (shrink newfs') noClash plimit (count+3)
							
						
					--	(call with newfs as the thingy and shrinkfixed splitk newfs)
					--	we need a boolean parameter to say if we got somewhere. (to know when to stop).
		
		Just (f',k,split,newcount) -> do 
											putStrLn "shrinking new problem..."
											putStrLn $ (show k) ++ " unfixed axioms, " ++ (show ((length f')-k)) ++ " fixed axioms" 
											shrinkedunfixed <- (shrinkUnfixed k f')
											zoom' dir (f',k,b) shrinkedunfixed noClash plimit newcount
			
shrinkUnfixed :: Int -> [Form] -> IO [([Form],Int,Bool)]
shrinkUnfixed k fs = 
	let 
				unfixed = (take k fs)
				fixed   = (drop k fs) 
				-- we know that at least one of the axioms in "fixed" is important.			 
				
	in 
		do 	unfixed' <- permute unfixed
			return $ addtoShrinkList fixed (shrink unfixed')
	
		 
	
addtoShrinkList :: [Form] -> [([Form],Int,Bool)] -> [([Form],Int,Bool)]
addtoShrinkList _ [] = []
addtoShrinkList fixed ((fs,k',_):xs) = (fs++fixed,k',True):(addtoShrinkList fixed xs)






smallestUnsat :: FilePath  -> [([Form],Int,Bool)] -> String -> Int -> Int -> IO (Maybe ([Form],Int,Bool,Int))

smallestUnsat _ [] _ _ newcount  = return Nothing
smallestUnsat dir ((fs,k,split):ffs) noClash plim count = do
    if count >= 350 then return Nothing else do
     h <- openFile (dir ++ "zoom"++(show count)) WriteMode
--	 let axioms = if (not split) then fs else 
     hPutStr h $ (form2axioms fs noClash)
     hClose h
     b <- finiteModel (dir ++ "zoom"++(show count)) plim
     if b then 
		 do
		 	putStrLn $ "fimo. " ++ (show count) ++ " trying next in shrinklist."
 			smallestUnsat dir ffs noClash plim (count+1)  
       --if finite model - discard f and try the other shrinked lists
      else do
		  putStrLn $ "no fimo! "
		  return $ Just (fs,k,True,count+1) --if no finite model - return f
	
	
	
	
shrink :: [a] -> [([a],Int,Bool)]
shrink xs = map mkTriple $ removeChunks xs
	 where
	  removeChunks xs = rem (length xs) xs
	   where
      
	    rem :: Int -> [a] -> [([a],Int)]
	    rem 0 _  = []
	    rem 1 _  = [([],0)]
	    rem n xs = (xs1,0)
	             : (xs2,0)
	             : ( [ (xs2 ++ xs1',nl1+(length xs2)) | (xs1',nl1) <- rem n1 xs1, not (null xs1') ]
	        `ilv`[ (xs1 ++ xs2',nl2+(length xs1)) | (xs2',nl2) <- rem n2 xs2, not (null xs2') ]
	               )
			   
	     where
      
	      n1  = n `div` 2
	      xs1 = take n1 xs
	      n2  = n - n1
	      xs2 = drop n1 xs
	      lxs1 = length xs1
	      lxs2 = length xs2
  
	      []     `ilv` ys     = ys
	      xs     `ilv` []     = xs
	      (x:xs) `ilv` (y:ys) = x : y : (xs `ilv` ys)
		  
          mkTriple (a,b) = (a,b,False)


-}


{-

zoom' :: FilePath -> [Form] -> [([Form],Int)] -> String -> Int -> Int -> IO [Form]
zoom' dir best [] _ _ _  = return best
zoom' dir best ffs noClash plim n  = do  
   (f',count) <- smallestUnsat dir best ffs noClash plim n 
   if f' == best then return f' else do
		f'' <- permute f'
		zoom' dir f' (shrink f'') noClash plim count 
		
--fix the new axioms. zoom the rest but always add the fixed ones to the theory.
-- when we can't zoom anymore, we swap places of fixed and unfixed.

smallestUnsat :: FilePath -> [Form] -> [([Form],Int)] -> String -> Int -> Int -> IO ([Form],Int)
smallestUnsat _ best [] _ _ count  = return (best,count)
smallestUnsat dir best ((f,k):fs) noClash plim n  = do
   if n >= 350 then return (best,n) else do
    h <- openFile (dir ++ "zoom"++(show n)) WriteMode
    hPutStr h $ (form2axioms f noClash)
    hClose h
    b <- finiteModel (dir ++ "zoom"++(show n)) plim
    if b then do
	 smallestUnsat dir best fs noClash plim (n+1)  
      --if finite model - discard f and try the other shrinked lists
     else return (f,n+1) --if no finite model - return f


shrink :: [a] -> [([a],Int)]
shrink xs = removeChunks xs
 where
  removeChunks xs = rem (length xs) xs
   where
    rem :: Int -> [a] -> [([a],Int)]
    rem 0 _  = []
    rem 1 _  = [([],0)]
    rem n xs = (xs1,length xs1)
             : (xs2,length xs2)
             : ( [ (xs2 ++ xs1',nl1) | (xs1',nl1) <- rem n1 xs1, not (null xs1') ]
           `ilv` [ (xs1 ++ xs2',nl2) | (xs2',nl2) <- rem n2 xs2, not (null xs2') ]
               )
			   
     where
      
      n1  = n `div` 2
      xs1 = take n1 xs
      n2  = n - n1
      xs2 = drop n1 xs
      lxs1 = length xs1
      lxs2 = length xs2
  
      []     `ilv` ys     = ys
      xs     `ilv` []     = xs
      (x:xs) `ilv` (y:ys) = x : y : (xs `ilv` ys)

-- (axioms,n)  -> zoom : take n axioms 
--			   then add (drop n axioms)	
	  -}
{-		
		
smallestUnsat _ best [] _ _ count  = return (best,count)
smallestUnsat dir best ((as,k):fs) noClash plim n  = 
   if n >= 350 then return (best,n) else do
    h <- openFile (dir ++ "zoom"++(show n)) WriteMode
    hPutStr h $ (form2axioms  noClash)
    hClose h
    b <- finiteModel (dir ++ "zoom"++(show n)) plim
    if b then do
	 smallestUnsat dir best fs noClash plim (n+1)  
      --if finite model - discard f and try the other shrinked lists
     else return (f,n+1) --if no finite model - return f

-}







{-

zoom' dir best [] _ _ _  = return best
zoom' dir best fs noClash plim n  = do  
   (f,count) <- smallestUnsat dir best fs noClash plim n 
   if f == best then return f else do
		f' <- permute f
		zoom' dir f (shrink f') noClash plim count


zoom :: FilePath -> [Form] -> String -> Int -> IO [Form]
zoom dir fs noClash plimit   = do
	cs <- zoom' dir fs  (shrink fs) noClash plimit 0 
	return $ sort cs 

zoom' :: FilePath -> [Form] -> [[Form]] -> String -> Int -> Int -> IO [Form]
zoom' dir best [] _ _ _  = return best
zoom' dir best fs noClash plim n  = do  
   (f,count) <- smallestUnsat dir best fs noClash plim n 
   if f == best then return f else do
		f' <- permute f
		zoom' dir f (shrink f') noClash plim count 

permute :: Eq a => [a] -> IO [a]
permute as = do 
	g <- getStdGen
	permute' g as

permute' :: Eq a => StdGen -> [a] -> IO [a]
permute' _ [] = return []
permute' g  xs = do
	let 
		(a,g') 		= randomR (0,(length xs) -1) g 
		el				= xs !! a
	xs'			<- permute' g' (delete el xs)
	return (el:xs')

smallestUnsat _ best [] _ _ count  = return (best,count)
smallestUnsat dir best (f:fs) noClash plim n  = do
   if n >= 350 then return (best,n) else do
    h <- openFile (dir ++ "zoom"++(show n)) WriteMode
    hPutStr h $ (form2axioms f noClash)
    hClose h
    b <- finiteModel (dir ++ "zoom"++(show n)) plim
    if b then do
	 smallestUnsat dir best fs noClash plim (n+1)  
      --if finite model - discard f and try the other shrinked lists
     else return (f,n+1) --if no finite model - return f

	
shrink :: [a] -> [[a]]
shrink xs = removeChunks xs
 where
  removeChunks xs = rem (length xs) xs
   where
    rem 0 _  = []
    rem 1 _  = [[]]
    rem n xs = xs1
             : xs2
             : ( [ xs1' ++ xs2 | xs1' <- rem n1 xs1, not (null xs1') ]
           `ilv` [ xs1 ++ xs2' | xs2' <- rem n2 xs2, not (null xs2') ]
               )
     where
      n1  = n `div` 2
      xs1 = take n1 xs
      n2  = n - n1
      xs2 = drop n1 xs
  
      []     `ilv` ys     = ys
      xs     `ilv` []     = xs
      (x:xs) `ilv` (y:ys) = x : y : (xs `ilv` ys)



-}




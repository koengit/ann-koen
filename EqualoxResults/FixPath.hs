import Data.List
import System.IO
import Debug.Trace

pickProblems :: IO ()
pickProblems = do
  r <- readFile "statistics"
  
  let r1 = tail $ tail $ lines r
      rs = filter (tokeep) (divide r1)
  putStrLn (show rs)
  mapM_ (writeFiles "moreTos" "moreStos") rs
  return ()
 
 
writeFiles f1 f2 (a,b,c) = do
  if (((read b) :: Double) > ((read c) :: Double)) then 
	 appendFile f1 (a ++ "\n")
	  else appendFile f2 (a ++ "\n")
 

name (a,b,c) = a
tots (a,b,c) = b
stots (a,b,c) = c
ratio (a,b,c) = max b c / min b c

tokeep (a,b,c) = ratio (a, (read b) :: Double ,(read c) ::Double) > 2

divide [] = []
divide (x:y:z:xs) = (x,y,z) : divide xs


fixPath :: FilePath -> IO ()
fixPath file = do
  r     <- readFile file
  u     <- readFile "contentsU"
  t     <- readFile "contentsT"
  c     <- readFile "contentsC"
  s     <- readFile "contentsS"
  let ls = lines r 
      lsu = lines u
      lss = lines s
      lsc = lines c
      lst = lines t
      output' = map (addPath lsu lst lsc lss) ls
      output =  unlines output'
  writeFile "listord" output
 
 
--checkRating


  
addPath u t c s l = trace (show l) $ 
 if elem l u then "Unsatisfiable/" ++ l 
	else if elem l t then "Theorem/" ++ l
	  else if elem l c then "Countersatisfiable/" ++ l
            else if elem l s then "Satisfiable/" ++ l
             else if elem l u then "Unknown/" ++ l
              else error "?"
  
  
findMissing = do
	r <- readFile "listalltrans"
	r2 <- readFile "test"
	let ls1 = lines r
	    ls2 = lines r2
	    ls3 = ls1 \\ ls2
	writeFile "listmissing" (unlines ls3)
 
  

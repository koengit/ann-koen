import System.IO 
import Data.List.Split
import Debug.Trace

compareResults :: FilePath -> FilePath -> IO ()
compareResults file1 file2 = do
	s1 <- readFile file1
	s2 <- readFile file2
	let 
		ss1 = lines s1
		ss2 = lines s2
		zips = zip ss1 ss2 in 
		
		showResult $ keepInteresting zips
		
		
showResult [] = return ()
		
showResult ((a,b,c,d):xs) = do
	let 
		lb = (length a) < 16 
		lb2 = (length b) < 16
	if lb then (putStrLn $ a ++ "\t\t\t\t" ++ b ++ (if lb2 then "\t\t\t" else "\t") ++ c ++ "\t" ++ show d)
		else (putStrLn $ a ++ "\t" ++ b ++ (if lb2 then "\t\t\t\t" else "\t") ++ c ++ "\t" ++ show d)
	showResult xs
		
		
keepInteresting :: [(String,String)] -> [(String,String,String,Double)]
keepInteresting [] =[]
keepInteresting (((a,b)):xs) = 
	case isInteresting (a,b) of
		Just (a',f) -> if abs f < 30 && f /= 0 then keepInteresting xs else
			
				 if a == a' then (a,b,"Original",f): keepInteresting xs 
				  else (a,b,"Equalified",f) : keepInteresting xs
		_	    -> keepInteresting xs
		
	
allBlank [] = True
allBlank (' ':xs) = allBlank xs
allBlank _ = False


isInteresting :: (String,String) -> Maybe (String,Double)
isInteresting (a,b) = 
	
	let 
		[file1,status1] = take 2 $ words a
		[file2,status2] = take 2 $ words b in
	if (status1 == "fail" && status2 /= "fail") then Just (b,0) 
	  else if (status2 == "fail" && status1 /= "fail") then Just (a,0)
	  else if (status1 == "fail" || status2 == "fail") then Nothing
	else let 
		time1' = (words a !! 2)
		time2' = (words b !! 2) 
		time1 = ((read (init (drop 2 time1'))) :: Double)
		time2 = ((read (init (drop 2 time2')) :: Double))		
		biggest = (time1 - time2) > 0	
		factor  = if biggest then time1/time2 else -time2/time1 in 
		if factor > 0 then Just (a,factor) else Just (b,factor)
		-- if ((abs (((read (init (drop 2 time1))) :: Double) - ((read (init (drop 2 time2)) :: Double)))) > 10) then
		-- if ( > 0 then Just b else Just a
		--else Nothing
		
		
fixProblemList :: FilePath -> FilePath -> IO ()
fixProblemList file out = do
	s <- readFile file
	let 	
		s3 = map fixLine (lines s)
	h <- openFile out WriteMode
	hSetBuffering h NoBuffering
	hPutStr h $ unlines s3
	hClose h
	
fixLine s = s ++ ".p" --"Problems/" ++ take 3 s ++ "/" ++ s ++ ".p"
	
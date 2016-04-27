module Main where
	
import ParseProblem
import Form
import Infinox.Zoom
import System.Time
import System.IO
import System.Environment
import Infinox.Conjecture (form2axioms)

main :: IO ()
main = do
	args <- getArgs
	zoomtest $ head args
       
	
zoomtest :: FilePath -> IO ()
zoomtest file = 
	do 
		input 	<- readProblem file 
		time1 <- getClockTime
		zoomed <- zoom "temp/" (map what input) "noClash" 5
	      
		time2 <- getClockTime
	        let
	           time = tdSec $ diffClockTimes time2 time1
   	        h <- openFile ("zoomed/" ++ file++"_zoomed_" ++ show time) WriteMode
   	        hPutStr h $ (form2axioms zoomed "noClash")
   	        hClose h
		 
		
	-- 	zoomed2 <- zoom "temp2" zoomed "noClash" 5
	--	putStrLn $ show (length input) ++ "      " ++ show (length zoomed) ++ "   " ++ show (length zoomed2)
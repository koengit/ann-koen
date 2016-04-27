module Infinox.Classify where

import System.IO
import System.Process (system)
import System.Time 
import System.Directory (removeFile,createDirectoryIfMissing) 
import Control.Concurrent (threadDelay)
import Data.List
import Control.Monad.Reader (ask, liftIO,lift)
import qualified Data.Set as S

import qualified Flags as F
import Flags( Flags, Method(InjNotSurj,SurjNotInj,Serial,Sausage))
import Form
import Output

import Infinox.Conjecture
import Infinox.Generate
import Infinox.Relations
import Infinox.Zoom (zoom)
import Infinox.InjOnto
import Infinox.Util	
import Infinox.Auto (continueAuto)
import Infinox.Leo  (classifyWithLeo)
import Infinox.Settings

import Debug.Trace

-----------------------------------------------------------------------------------------

classifyProblem :: (?flags :: Flags) => [Clause] -> [Clause] -> IO ClauseAnswer
classifyProblem theory oblig = let cs = theory ++ oblig in do

	createDirectoryIfMissing False (F.temp ?flags)

	let
		tempdir 					= (F.temp ?flags) ++ "/" ++ (subdir (F.thisFile ?flags))
		--zoomfile					= zoomf (F.thisFile ?flags)
		verbose						=  F.verbose ?flags > 0	
		methods						=  F.method ?flags	
		eflag						  =  F.elimit ?flags
		pflag 						=  F.subset ?flags
		forms 						= map toForm cs
		noClash 					= noClashString forms
		axiomfile					= tempdir ++ "axiomfile"
		termdepth					= F.termdepth ?flags
		funflag						= F.function ?flags
		relflag						= F.relation ?flags
		leoflag						= F.leo ?flags
		proverflag				= F.prover ?flags
    
	createDirectoryIfMissing False tempdir
	starttime   	<- getClockTime
	
	fs  		<- if (F.zoom ?flags) then do											
								if verbose then putStrLn "Zooming..." else return ()  
								zoom tempdir forms noClash (F.plimit ?flags)
								
							else return forms --the formulas in which to search for candidates	
							
	time2 <- getClockTime
	let time3 = tdSec $ diffClockTimes time2 starttime
	let
		sig 			= getSignature fs (F.function ?flags)
		axioms 		= form2axioms forms noClash
		settings 	= MSet axiomfile tempdir fs sig noClash verbose 
									funflag relflag pflag termdepth eflag proverflag
	h <- openFile axiomfile WriteMode			
	hSetBuffering h NoBuffering
	hPutStr h axioms	
	hClose h
	result <- runWithSettings settings $ classifyWithMethods methods 
	finish starttime result tempdir (F.thisFile ?flags) (F.outfile ?flags)

{-
	saveZoomed fs (zoomfile++(show time3)) noClash
	return (if length fs == 1 then (NoAnswerClause GaveUp) else (NoAnswerClause GaveUp))
	
zoomf inputfile = (filter ( (not . (flip elem) ['/','.','-',' '])) inputfile) ++ "_Zoomed" 

saveZoomed fs file noClash = do 
    h <- openFile file WriteMode
    hPutStr h $ (form2axioms fs noClash)
    hClose h	
	
	-}									
{-

	let
		sig 			= getSignature fs (F.function ?flags)
		axioms 		= form2axioms forms noClash
		settings 	= MSet axiomfile tempdir fs sig noClash verbose 
									funflag relflag pflag termdepth eflag proverflag
	h <- openFile axiomfile WriteMode			
	hSetBuffering h NoBuffering
	hPutStr h axioms	
	hClose h
	result <- runWithSettings settings $ classifyWithMethods methods 
	finish starttime result tempdir (F.thisFile ?flags) (F.outfile ?flags)
-}

classifyWithMethods :: [Method] -> Settings Result
classifyWithMethods [] = return None
classifyWithMethods (m:ms)  = do
	liftIO $ putStrLn $ show m
	result <- classifyWithMethod m
	case result of 
		None -> classifyWithMethods ms 
		_		 -> do 	
				--		Settings $ lift $ putStrLn "hej"
						return result


classifyWithMethod :: Method -> Settings Result
classifyWithMethod m  = do
	settings <- ask
	let 
			funflag' 	= funflag settings
			d			= depthflag settings
			fs			= forms settings
			sig'		= sig settings
	if m == Serial || m == Sausage then do
		continueRelations m [] -- we don't want to try function symbols.
		
		 -- || m == Relation || m == Trans then do
		--	let		--collect all functions with arity <= 5, 
		--			funsymbs	= fsymbs $ sig settings
		--			funs		=	filter ((\x -> x <= 5). funArity) $ 
		--								sortTerms $ nub $ getFunsFromSymbols funsymbs funflag' 1
		--			rels	= concatMap makeRelations funs
			
			
		else if m == InjNotSurj || m == SurjNotInj then do	
				let
					funs	=	collectTestTerms sig' funflag' fs d
				traceM  $ show funs
				
				let
					(method,rflag')	=	if m == InjNotSurj then 
																(conjInjNotOnto, relflag settings) 
															else (conjNotInjOnto,Nothing) in 
																continueInjOnto method funs rflag'
			else case m of
			--	Auto 			-> continueAuto
			 --	Leo				-> liftIO $ classifyWithLeo $ axiomfile settings
				_    			-> undefined -- add new methods here!!

-----------------------------------------------------------------------------------------

finish time1 result dir file out = do
   time2 <- getClockTime
   let
      time = tdSec $ diffClockTimes time2 time1
   threadDelay 1000000
 --  system $ "rm -r " ++  dir
   maybeAppendFile out ( file ++ " : " ++ show result ++ " : " ++ show time ++ "\n" )
   case result of
    None					->	return $ NoAnswerClause GaveUp
    _						->	return FinitelyUnsatisfiable	
   where
      maybeAppendFile Nothing _     =  return ()
      maybeAppendFile (Just f) x    =  appendFile f x

subdir inputfile = (filter ( (not . (flip elem) ['/','.','-',' '])) inputfile) ++ "_TEMP/"



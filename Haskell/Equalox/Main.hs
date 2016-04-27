module Equalox.Main where

--import qualified Main
import Runner
import Flags
import Equalox.Equalify


---------------------------------------------------------------------------
-- main

main :: IO ()
main =
  do 
	putStrLn "Equalox, version 1.0, 2013-06-26"
    -- Main.main Equalox equalify
	runner Equalox equalify

---------------------------------------------------------------------------
-- the end.

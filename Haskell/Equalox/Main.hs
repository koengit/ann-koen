module Equalox.Main where

--import qualified Main
import Runner
import Flags
--import Equalox.Equalify
import Equalox.Equalify


---------------------------------------------------------------------------
-- main

main :: IO ()
main =
  do 
	putStrLn "Equalox, version 1.0"
    -- Main.main Equalox equalify
--	runner' Equalox equalify
        runner' Equalox equalify
---------------------------------------------------------------------------
-- the end.

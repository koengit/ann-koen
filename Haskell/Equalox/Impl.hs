module Main where

import System.Process
import System.IO
import Data.Char
import Data.List
import qualified SAT
import SAT hiding ( neg )
import SAT.Optimize
import SAT.Unary ( count )


eproof = "/home/koen/Work/Code/E/PROVER/eproof"

type Name = String
type Form = String
type Prop = (String, String)

base :: [Prop]
base =
  [ ("transitive",    "![X,Y,Z] : ((p(X,Y) & p(Y,Z)) => p(X,Z))")
  , ("reflexive",     "![X] : p(X,X)")
  , ("symmetric",     "![X,Y] : (p(X,Y) => p(Y,X))")
  , ("right_euclidean", "![X,Y,Z] : ((p(X,Y) & p(X,Z)) => p(Y,Z))")
  , ("coreflexive",   "![X,Y] : (p(X,Y) => X=Y)")
  , ("antisymmetric", "![X,Y] : ((p(X,Y) & p(Y,X)) => X=Y)")
  , ("total",         "![X,Y] : (p(X,Y) | p(Y,X))")
  , ("serial",        "![X]:?[Y]: p(X,Y)")
  ]

flp :: Prop -> Prop
flp (name, form) = (name ++ "_flip", map (\c -> if c == 'p' then 'q' else c) form)

neg :: Prop -> Prop
neg (name, form) = (name ++ "_neg", concatMap (\c -> if c == 'p' then "~p" else [c]) form)

props :: [Prop]
props =
  sortOn (\(name,_) -> (length (filter (=='_') name), length name, name))
  [ q
  | p <- base
  , q <- [p, flp p, neg p, flp (neg p)]
  ]

prove :: [Prop] -> Prop -> IO (Maybe [Prop])
prove assumps goal =
  do --putStrLn (unwords (map fst assumps) ++ " => " ++ fst goal ++ "?")
     writeFile "input.tptp" $ unlines $
       [ "fof(def_q, axiom, (![X,Y] : (p(X,Y) <=> q(Y,X))))." ] ++
       [ "fof(" ++ name ++ ", axiom, (" ++ form ++ "))."
       | (name,form) <- assumps
       ] ++
       [ "fof(" ++ name ++ ", conjecture, (" ++ form ++ "))."
       | (name,form) <- [goal]
       ]
     system (eproof ++ " --auto --tstp-format --soft-cpu-limit=1 input.tptp > output")
     s <- readFile "output"
     let ls = lines s
     return $
       if "# SZS status Theorem" `elem` ls
         then Just [ p
                   | l <- ls
                   , _:name:_:"file":_ <- [reverse (words (map space l))]
                   , p@(name',_) <- assumps
                   , name' == name
                   ]
         else Nothing
 where
  space a | a `elem` ",()" = ' '
          | otherwise      = a

synonyms :: [Prop] -> IO [Prop]
synonyms []     = do return []
synonyms (p@(_,a):ps) =
  do qs <- sequence
           [ do m <- prove [] ("goal", "((" ++ a ++ ") <=> (" ++ b ++ "))")
                case m of
                  Nothing -> do return (Just q)
                  Just _  -> do putStrLn (fst p ++ " <=> " ++ fst q)
                                return Nothing
           | q@(_,b) <- ps
           ]
     ps' <- synonyms [ q | Just q <- qs ]
     return (p:ps')

zoom :: [[Prop]] -> [Prop] -> Prop -> IO [[Prop]]
zoom bads assumps goal =
  do s <- newSolver
     xs <- sequence [ newLit s | _ <- assumps ]
     u  <- count s xs
     sequence_
       [ addClause s [ SAT.neg x | (a,x) <- assumps `zip` xs, a `elem` p ]
       | p <- bads
       ]
     
     let loop =
           do hPutStr stderr "*"
              hFlush stderr
              b <- solveMaximize s [] u
              hPutStr stderr "\b \b"
              hFlush stderr
              if b then
                do bs <- sequence [ modelValue s x | x <- xs ]
                   let assumps' = [ a | (a,True) <- assumps `zip` bs ]
                   mass <- prove assumps' goal
                   ps <- case mass of
                           Just ass' ->
                             do putStrLn (fst goal ++ " <= {" ++ concat (intersperse ", " (map fst ass')) ++ "}")
                                addClause s [ SAT.neg x | (a,x) <- assumps `zip` xs, a `elem` ass' ]
                                return [ass']
                           
                           Nothing ->
                             do addClause s [ x | (a,x) <- assumps `zip` xs, a `notElem` assumps' ]
                                return []
                   qs <- loop
                   return (ps ++ qs)
               else
                do deleteSolver s
                   return []
     loop

main :: IO ()
main =
  do putStrLn ("=== " ++ show (length props) ++ " PROPERTIES ===")
     putStrLn (show (map fst props))
     putStrLn "=== SYNONYMS ==="
     props' <- synonyms props
     putStrLn ("=== NOW: " ++ show (length props') ++ " PROPERTIES ===")
     putStrLn (show (map fst props'))
     putStrLn "=== FINDING BADS ==="
     qs <- subsume `fmap` zoom [] props' ("false", "$false")
     putStrLn "=== IMPLICATIONS ==="
     impls <- concat `fmap` sequence
              [ do putStrLn ("+++ " ++ fst p ++ "...")
                   ps <- zoom qs (props' \\ [p]) p
                   return [ (p,qs) | qs <- subsume ps ]
              | p <- base
              ]
     putStrLn "=== SUMMARY ==="
     putStr $ unlines
       [ fst p ++ " <= {" ++ concat (intersperse ", " (map fst qs)) ++ "}"
       | (p,qs) <- impls
       ]

subsume :: Eq a => [[a]] -> [[a]]
subsume []      = []
subsume (xs:xss)
  | any (`subsetOf` xs) yss = yss
  | otherwise               = xs : yss
 where
  yss = subsume xss
  
  as `subsetOf` bs = all (`elem` bs) as
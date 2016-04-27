{-|
Module      : SAT.Val
Description : Functions for working with symbolic values
-}
module SAT.Val(
  -- * The Val type
    Val
  , newVal
  , val

  -- * Inspection
  , (.=)
  , domain

  -- * Models
  , modelValue
  )
 where

import qualified SAT
import SAT hiding ( modelValue )
import SAT.Util( usort )
import SAT.Bool( atMostOne )
import SAT.Equal
import SAT.Order

import Data.List( tails )
import Control.Monad( when )

------------------------------------------------------------------------------

-- | The Val type, for representing symbolic values.
newtype Val a = Val [(Lit,a)]
 deriving ( Eq, Ord, Show )

-- | Creates a symbolic value, with concrete values all elements of the
-- specified list. The list has to be non-empty.
newVal :: Ord a => Solver -> [a] -> IO (Val a)
newVal s xs =
  case xs' of
    []    -> do error "SAT.Val.newVal: empty list"
    [x]   -> do return (val x)
    [x,y] -> do q <- newLit s
                return (Val [(q,x),(neg q,y)])
    _     -> do qs <- sequence [ newLit s | x <- xs' ]
                addClause s qs
                atMostOne s qs
                return (Val (qs `zip` xs'))
 where
  xs' = usort xs

-- | Creates a symbolic value with only one concrete element.
val :: a -> Val a
val x = Val [(true,x)]

-- | Returns all possible concrete values for a symbolic value.
domain :: Val a -> [a]
domain (Val qxs) = map snd qxs

-- | Returns the literal representing the symbolic value having the concrete
-- specified value.
(.=) :: Ord a => Val a -> a -> Lit
Val qxs .= x = go qxs
 where
  go []          = false
  go ((q,y):qxs) =
    case x `compare` y of
      LT -> false
      EQ -> q
      GT -> go qxs

------------------------------------------------------------------------------

instance Ord a => Equal (Val a) where
  equalOr s pre p q =
    sequence_
    [ case pqx of
        (Just p,  Nothing, _) -> addClause s (neg p : pre)
        (Nothing, Just q,  _) -> addClause s (neg q : pre)
        (Just p,  Just q,  _) -> addClause s (neg p : q : pre)
    | pqx <- stitch p q
    ]

  notEqualOr s pre p q =
    sequence_
    [ case pqx of
        (Just p, Just q, _) -> addClause s (neg p : neg q : pre)
        _                   -> return ()
    | pqx <- stitch p q
    ]

instance Ord a => Order (Val a) where
  lessTupleOr s pre incl (x,p) (y,q) =
    do w <- newLessLit s incl p q
       when (w /= true) $
         notEqualOr s (w:pre) x y
       sandwich false true n xys
   where
    xys = [ (lit a,lit b) | (a,b,_) <- stitch x y ]
    n   = length xys

    lit Nothing  = false
    lit (Just x) = x

    sandwich lft rgt _ [] =
      do return ()

    sandwich lft rgt n xys | n <= 2 =
      do sequence_ [ addClause s (neg lft : neg x : pre) | (x,_) <- xys ]
         sequence_ [ addClause s (rgt     : neg y : pre) | (_,y) <- xys ]
         sequence_ [ addClause s (neg y   : neg x : pre)
                   | (_,y):xys' <- tails xys
                   , (x,_) <- xys'
                   ]

    sandwich lft rgt n xys =
      do lft' <- newLit s
         rgt' <- newLit s
         addClause s [neg lft,  lft']
         addClause s [neg rgt', lft']
         addClause s [neg rgt', rgt]
         sandwich lft  rgt' k     (take k xys)
         sandwich lft' rgt  (n-k) (drop k xys)
     where
      k = n `div` 2

stitch :: Ord a => Val a -> Val a -> [(Maybe Lit, Maybe Lit, a)]
stitch (Val pxs) (Val qys) = go pxs qys
 where
  go []          qys         = [ (Nothing, Just q, y) | (q,y) <- qys ]
  go pxs         []          = [ (Just p, Nothing, x) | (p,x) <- pxs ]
  go ((p,x):pxs) ((q,y):qys) =
    case x `compare` y of
      LT -> (Just p,  Nothing, x) : go pxs ((q,y):qys)
      EQ -> (Just p,  Just q,  x) : go pxs qys
      GT -> (Nothing, Just q,  y) : go ((p,x):pxs) qys

------------------------------------------------------------------------------

-- | Returns the concrete value of the symbolic value in the found model.
-- (/Only use when 'solve' has returned True!/)
modelValue :: Solver -> Val a -> IO a
modelValue s (Val qxs) = go qxs
 where
  go []          = error "SAT.Val.modelValue: no trues in list"
  go ((q,x):qxs) = do b <- SAT.modelValue s q
                      if b then return x else go qxs

------------------------------------------------------------------------------

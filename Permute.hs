{-# LANGUAGE Rank2Types #-}
-- Copyright © 2012 Bart Massey

-- | Basic permutation functions, as inspired by
-- <http://www.math.utah.edu/mathcircle/notes/permutations.pdf>.
-- 
-- /These functions are really only believed to work on finite
-- permutations. In addition, many of the algorithms are quite
-- inefficient./

module Permutations (
  -- * Types
  Permutation(..), Sigma(..), Cycles(..), 
  -- * Creation
  fromSigma, fromCycles, 
  -- * Conversion
  toSigma, toCycles,
  -- * Special permutations
  id, idN, inverseN
  ) where

import Prelude hiding (id)
import Data.List
import Data.Ord

-- | This is the type of permutation functions.
type Permutation a = [a] -> [a]

-- | This is the type of 𝛔 specifications of permutation
-- functions. A 𝛔 specification of a permutation function is
-- a permutation of [1..n] representing the position to
-- which each input element will be carried.
type Sigma = [Int]

-- | This is the type of cycle specifications of permutation
-- functions on /n/ elements. The first argument of the tuple
-- is just /n/. The second is a list of cycles.
-- 
-- Each cycle is a list of positions, with each element
-- representing the position to which the next element in
-- the cycle will be carried. Unit cycles may be omitted
-- from the permutation as implied, but need not be.  For
-- the specification to be well-formed, the concatenation of
-- the list of cycles must be some permutation of a
-- subsequence of @[1..n]@.
type Cycles = (Int, [[Int]])

-- | Given a 𝛔 specification of a permutation, return a function
-- which permutes its input list in the specified way.  
-- 
-- No checking is done on anything here, so one must be
-- careful that the arguments to both fromSigma and the
-- returned function make sense.
fromSigma :: Sigma -> Permutation a
fromSigma sigma =
  (\elems -> map snd $ sortBy (comparing fst) $ zip sigma elems)
  
-- | Given a cycle specification of a permutation, return a
-- function which permutes its input list in the specified
-- way.
-- 
-- No checking is done on anything here, so one must be
-- careful that the arguments to both fromCycles and the
-- returned function make sense.
fromCycles :: Cycles -> Permutation a
fromCycles = fromSigma . toSigma

-- | Given a cycle specification of a permutation, return
-- the 𝛔 specification. In accordance with standard practice
-- unit cycles may be omitted, but they need not be.
toSigma :: Cycles -> Sigma
toSigma (n, cycles) =
  let cycles' = unitPad in
  map snd $ sortBy (comparing fst) $ concatMap traceCycle cycles'
  where
    unitPad =
      cycles ++ map (:[]) ([1..n] \\ concat cycles)
    traceCycle (c : cs) =
      let (cn, cs') = foldr trace (c, []) cs in
      (cn, c) : cs'
      where
        trace c2 (c1, cs') = (c2, (c1, c2) : cs')

-- | Given a key @k@ and an association list @xs@, if the
-- key is not in the association list, return 'Nothing'.
-- Otherwise, return @'Just' (v, ss')@, where @v@ is the
-- leftmost value associated with @k@, and @xs'@ is @xs@
-- with all associations having key @k@ removed.
extractAssoc t xs =
  case foldr extract (Nothing, []) xs of
    (Nothing, _) -> Nothing
    (Just x', xs') -> Just (x', xs')
  where
    extract x'@(k, v) (r, xs')
      | k == t = (Just v, xs')
      | otherwise = (r, x' : xs')

-- | Given a finite 𝛔 specification of a permutation of /n/
-- elements, return the cycle specification and its
-- length. The cycles and their elements will not be in any
-- particular order.  Unit cycles will be omitted.
toCycles :: Sigma -> Cycles
toCycles sigma =
  let cycles = unfoldr collectCycles (zip [1..] sigma) in
  let cycles' = filter ((> 1) . length) cycles in
  (length sigma, cycles')
  where
    collectCycles :: [(Int, Int)] -> Maybe ([Int], [(Int, Int)])
    collectCycles [] = Nothing
    collectCycles ((s1, s2) : ss) =
      Just $ collectCycle s2 [] ss
      where
        collectCycle :: Int -> [Int] -> [(Int, Int)] -> ([Int], [(Int, Int)])
        collectCycle c1 cs ds
          | c1 == s1 = (c1 : cs, ds)
          | otherwise =
              let Just (c2, ds') = extractAssoc c1 ds in
              let (cs'', ds'') = collectCycle c2 cs ds' in
              (c1 : cs'', ds'')

-- | The infinite identity permutation.
id :: Permutation a
id = fromSigma [1..]

-- | The identity permutation on /n/ elements.
idN :: Int -> Permutation a
idN n = fromSigma [1..n]

-- | Permutations can be composed using ordinary function
-- composition, and the inverse of a permutation has a 𝛔 specification
-- given by the action of that permutation on @[1..n]@. Thus, this is
-- just a convenience function. Note that it is only defined
-- for finite permutations.
inverseN :: Int -> (forall a . Permutation a) -> Permutation b
inverseN n perm =
  fromSigma $ perm [1..n]

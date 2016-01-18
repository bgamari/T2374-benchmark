{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ptr (bench) where

import Prelude hiding (reverse)
import GHC.Base
import GHC.Ptr

import Foreign

reverse :: Addr# -> Int# -> Int# -> State# RealWorld -> State# RealWorld
reverse a i j s
  | isTrue# (i <# j) =
    case readIntOffAddr# a i s    of { (# s, x #) ->
    case readIntOffAddr# a j s    of { (# s, y #) ->
    case writeIntOffAddr# a j x s of { s ->
    case writeIntOffAddr# a i y s of { s ->
    reverse a (i +# 1#) (j -# 1#) s }}}}
  | otherwise = s

bench :: Int# -> Int# -> IO ()
bench k n = do p@(Ptr a) <- mallocArray (I# n) :: IO (Ptr Int)
               fill a n
               IO (go a k)
               free p
 where
 go a 0# s = (# s, () #)
 go a i  s = case reverse a 0# (n -# 1#) s of s -> go a (i -# 1#) s

fill :: Addr# -> Int# -> IO ()
fill a n = IO (go 0#)
 where
 go i s
   | isTrue# (i <# n) = case writeIntOffAddr# a i i s of s -> go (i +# 1#) s
   | otherwise        = (# s, () #)

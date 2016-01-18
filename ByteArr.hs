{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module ByteArr (bench) where

import Prelude hiding (reverse)

import Control.Monad.ST

import GHC.ST
import GHC.Base

import Foreign (sizeOf)

reverse :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
reverse arr i j s
  | isTrue# (i <# j) =
    case readIntArray#  arr i    s of { (# s, ei #) ->
    case readIntArray#  arr j    s of { (# s, ej #) ->
    case writeIntArray# arr j ei s of { s ->
    case writeIntArray# arr i ej s of { s ->
    reverse arr (i +# 1#) (j -# 1#) s } } } }
  | otherwise = s



fill :: MutableByteArray# s -> Int# -> State# s -> State# s
fill arr n = go 0#
 where
 go i s
   | isTrue# (i <# n) =
     case writeIntArray# arr i i s of { s -> go (i +# 1#) s }
   | otherwise = s
{-# INLINE fill #-}

bench :: Int -> Int -> ST s ()
bench (I# k) (I# n) = ST go
 where
 go s = case sizeOf (0 :: Int)        of { I# w ->
        case newByteArray# (n *# w) s of { (# s, arr #) ->
        case fill arr n s             of { s ->
        go' arr k s } } }
 go' arr 0# s = (# s, () #)
 go' arr k  s = case reverse arr 0# (n -# 1#) s of { s ->
                go' arr (k -# 1#) s }

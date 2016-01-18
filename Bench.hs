{-# LANGUAGE MagicHash #-}
module Main (main) where

import GHC.Types
import Control.Monad.ST
import qualified ByteArr
import qualified Ptr
import Criterion.Main

k, n :: Int
k = 100000
n = 25000

main :: IO ()
main = defaultMain
    [ bench "ptr" $ nfIO $ case k of { (I# k#) ->
                               case n of { (I# n#) -> Ptr.bench k# n# } }
    , bench "bytearr" $ nf (\(k,n) -> runST $ ByteArr.bench k n) (k,n)
    ]

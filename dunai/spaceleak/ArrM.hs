{-# LANGUAGE Arrows #-}


import Data.MonadicStreamFunction
import Control.Monad.Trans.MSF.Except (reactimateB)

main :: IO ()
main = reactimateB $ count >>> mainSF >>> arrM (\n -> print n >> return (n > 10))

mainSF ::  MSF IO Int Int
mainSF = foldr1 (>>>) $ replicate 1000000 $ arrM return

{-# LANGUAGE Arrows #-}


import Data.MonadicStreamFunction
import Control.Monad.Trans.MSF.List
import Control.Monad.Trans.MSF.Except (reactimateB)

main :: IO ()
main = reactimateB $ mainSF >>> arrM (\n -> let s = sum n in print s >> return (s > 1000000))

mainSF ::  MSF IO a [Int]
mainSF = parallelS 100000 $ arr (const 1) >>> accumulateWith (+) 0
-- mainSF = widthFirst $ replicateS 100000 $ arr (const 1) >>> accumulateWith (+) 0

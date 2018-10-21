module Control.Monad.Trans.MSF.List
  ( module Control.Monad.Trans.MSF.List
  , module Control.Monad.Trans.List
  ) where

-- External
import Control.Monad.Trans.List
  hiding (liftCallCC, liftCatch) -- Avoid conflicting exports

-- Internal
import Data.MonadicStreamFunction

-- * List monad

-- Name alternative (in the article): collect
widthFirst :: Monad m => MSF (ListT m) a b -> MSF m a [b]
widthFirst msf = widthFirst' [msf] where
    widthFirst' msfs = MSF $ \a -> do
        (bs, msfs') <- unzip . concat <$> mapM (runListT . flip unMSF a) msfs
        return (bs, widthFirst' msfs')


-- Name alternatives: "choose", "parallely" (problematic because it's not multicore)
sequenceS :: Monad m => [MSF m a b] -> MSF (ListT m) a b
sequenceS msfs = MSF $ \a -> ListT $ sequence $ apply a <$> msfs
  where
    apply a msf = do
        (b, msf') <- unMSF msf a
        return (b, sequenceS [msf'])
-- sequenceS = foldl (<+>) arrowzero . map liftMSFTrans

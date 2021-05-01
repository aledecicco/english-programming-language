module Location where

import Control.Monad.Trans.State ( put, get, gets, modify, runStateT, StateT )

import AST

--


-- Definition

type LocationT m r = StateT Location m r

initialLocation :: Location
initialLocation = (0, 0)

getCurrentLocation ::Monad m => LocationT m Location
getCurrentLocation = get

setCurrentLocation :: Monad m => Location -> LocationT m ()
setCurrentLocation = put

withLocation :: (Monad m, Foldable a) => Annotated a -> (Annotated a -> LocationT m b) -> LocationT m b
withLocation a f = setCurrentLocation (getAnnotation a) >> f a

runLocationT :: LocationT m a -> Location -> m (a, Location)
runLocationT = runStateT

--

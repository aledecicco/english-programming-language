module Location where

import Control.Monad.Trans.State ( put, get, gets, modify, runStateT, StateT )

import AST

--


--

type LocationT m r = StateT Location m r

getLocation :: Foldable a => a b -> b
getLocation = head . foldr (:) []

getFirstLocation :: Foldable a => [a b] -> b
getFirstLocation = getLocation . head

initialLocation :: Location
initialLocation = (0, 0)

getCurrentLocation ::Monad m => LocationT m Location
getCurrentLocation = get

setCurrentLocation :: Monad m => Location -> LocationT m ()
setCurrentLocation = put

withLocation :: (Monad m, Foldable a) => Annotated a -> (Annotated a -> LocationT m b) -> LocationT m b
withLocation a f = setCurrentLocation (getLocation a) >> f a

runLocationT :: LocationT m a -> Location -> m (a, Location)
runLocationT = runStateT

--

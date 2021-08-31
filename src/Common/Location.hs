{-|
Module      : Location
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

A monad transformer for keeping track of a 'Location'.
-}

module Location where

import AST (Annotated, Location)
import Control.Monad.Trans.State.Strict


type LocationT = StateT Location

-- | Returns the location of an annotated element.
getLocation :: Foldable a => a Location -> Location
getLocation = head . foldr (:) []

-- | Returns the location of the first element in a list of annotated elements.
getFirstLocation :: Foldable a => [a Location] -> Location
getFirstLocation = getLocation . head

initialLocation :: Location
initialLocation = (0, 0)

getCurrentLocation ::Monad m => LocationT m Location
getCurrentLocation = get

setCurrentLocation :: Monad m => Location -> LocationT m ()
setCurrentLocation = put

-- | Performs an action with the given annotated element as argument.
-- Sets the element's location as the current one.
withLocation :: (Monad m, Foldable a) => Annotated a -> (Annotated a -> LocationT m b) -> LocationT m b
withLocation arg f = setCurrentLocation (getLocation arg) >> f arg

runLocationT :: LocationT m a -> Location -> m (a, Location)
runLocationT = runStateT

module Monpad.Plugins where

import Data.List.NonEmpty (NonEmpty)

import Monpad

data Plugin a b where
    Plugin :: ServerConfig e s a b -> Plugin a b

--TODO this should be the other way round really (to match the usual 'with*' convention)
    -- but we end up wanting to use 'flip', and we can't without `ImpredicativeTypes`
withPlugin :: (forall e s. ServerConfig e s a b -> x) -> Plugin a b -> x
withPlugin f (Plugin p) = f p

plugins :: NonEmpty (Plugin a b) -> Plugin a b
plugins = foldl1 \(Plugin x) (Plugin y) -> Plugin $ combineConfs x y

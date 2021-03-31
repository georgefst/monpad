module Monpad.Plugins where

import Data.List.NonEmpty (NonEmpty)

import Monpad

data Plugin a b where
    Plugin :: ServerConfig e s a b -> Plugin a b

withPlugin :: Plugin a b -> (forall e s. ServerConfig e s a b -> x) -> x
withPlugin (Plugin p) f = f p

plugins :: NonEmpty (Plugin a b) -> Plugin a b
plugins = foldl1 \(Plugin x) (Plugin y) -> Plugin $ combineConfs x y

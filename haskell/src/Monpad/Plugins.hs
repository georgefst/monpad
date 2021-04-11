module Monpad.Plugins where

import Control.Monad.Reader

import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Optics

import Monpad

data Plugin a b where
    Plugin :: ServerConfig e s a b -> Plugin a b

withPlugin :: Plugin a b -> (forall e s. ServerConfig e s a b -> x) -> x
withPlugin (Plugin p) f = f p

plugins :: NonEmpty (Plugin a b) -> Plugin a b
plugins = foldl1 \(Plugin x) (Plugin y) -> Plugin $ combineConfs x y

onLayoutChange ::
    (Layout a b -> Monpad e s a b [ServerUpdate a b]) ->
    Update a b ->
    Monpad e s a b [ServerUpdate a b]
onLayoutChange f = \case
    ServerUpdate (SwitchLayout i) -> asks (Map.lookup i . view #layouts) >>= maybe mempty f
    ServerUpdate (SetLayout l) -> f l
    _ -> mempty

module Monpad.Plugins where

import Control.Monad.Reader
import Data.Tuple.Extra

import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map

import Monpad

data Plugin a b where
    Plugin :: ServerConfig e s a b -> Plugin a b

withPlugin :: Plugin a b -> (forall e s. ServerConfig e s a b -> x) -> x
withPlugin (Plugin p) f = f p

plugins :: NonEmpty (Plugin a b) -> Plugin a b
plugins = foldl1 \(Plugin x) (Plugin y) -> Plugin $ combineConfs x y

onLayoutChange ::
    (Layout a b -> Monpad e s a b [ServerUpdate a b]) ->
    ServerUpdate a b ->
    Monpad e s a b [ServerUpdate a b]
onLayoutChange f = \case
    SwitchLayout i -> asks (Map.lookup i . fst3) >>= maybe mempty f
    SetLayout l -> f l
    _ -> mempty

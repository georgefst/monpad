module Monpad.Plugins where

import Monpad

data Plugin a b where
    Plugin :: ServerConfig e s a b -> Plugin a b

withPlugin :: Plugin a b -> (forall e s. ServerConfig e s a b -> x) -> x
withPlugin (Plugin p) f = f p

plugins :: [Plugin a b] -> Plugin a b
plugins = foldl (\(Plugin x) (Plugin y) -> Plugin $ combineConfs x y) (Plugin @() @() mempty)

-- | This should only be used as part of an 'onUpdate' field.
onLayoutChange ::
    (Layout a b -> Monpad e s a b [ServerUpdate a b]) ->
    Update a b ->
    Monpad e s a b [ServerUpdate a b]
onLayoutChange f = \case
    -- since 'onLayout' is called after the server updates its state, we don't actually need any info from the message
    ServerUpdate (SwitchLayout _) -> f =<< getCurrentLayout
    ServerUpdate (SetLayout _) -> f =<< getCurrentLayout
    _ -> mempty

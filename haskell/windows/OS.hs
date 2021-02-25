module OS where

import Debug.Trace qualified as Debug
import Monpad

type E = ()
type S = ()
type A = Unit
type B = Unit

conf :: Layout A B -> ServerConfig E S A B
conf _l = Debug.trace "no device created - OS not yet supported" mempty

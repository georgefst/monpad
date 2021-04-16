-- | Compile-time configuration.
module Opts where

import Deriving.Aeson (SumObjectWithSingleField)

type JSON = '[SumObjectWithSingleField]

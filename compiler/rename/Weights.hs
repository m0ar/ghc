{-# LANGUAGE DeriveDataTypeable #-}
module Weights where

import Data.Data (Data(..))
import Data.Typeable
import Prelude (Integer, String)

class ClassName w where
  className :: (i -> w) -> String

newtype Weight = Weight Integer deriving (Data, Typeable)

instance ClassName Weight where
  className _ = "Weight"

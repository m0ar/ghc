{-# LANGUAGE DeriveDataTypeable #-}
module Weights (Weight(..)) where

import Prelude (Integer)
import Data.Data (Data(..))
import Data.Typeable

newtype Weight = Weight Integer 
    deriving (Data, Typeable)


